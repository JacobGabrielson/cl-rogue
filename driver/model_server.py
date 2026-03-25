#!/usr/bin/env python3
"""
model_server.py — XGBoost inference server for cl-rogue monster AI.

Listens on a Unix domain socket, receives JSON game-state lines from the
Lisp game process, runs the trained XGBoost model, and returns a 1-byte
action index (0–8, matching ACTION_ORDER in train_model.py).

Protocol (per query):
    Client → server:  JSON line  (text, newline-terminated, ASCII)
    Server → client:  1 byte     (action index 0–8)

Action indices match ACTION_ORDER = ['y','k','u','h','.','l','b','j','n'].
Index 4 ('.', stay) is used as a safe fallback on any error.

Usage (from repo root, venv active):
    python driver/model_server.py [--socket PATH] [--model PATH]
"""

import argparse
import json
import os
import signal
import socket
import sys

_HERE = os.path.dirname(os.path.abspath(__file__))
_REPO = os.path.dirname(_HERE)
sys.path.insert(0, _HERE)

from train_model import featurize
import xgboost as xgb

DEFAULT_SOCK  = '/tmp/cl-rogue-model.sock'
DEFAULT_MODEL = os.path.join(_REPO, 'model', 'monster_model_combined.ubj')

_STAY_IDX = 4   # index of '.' in ACTION_ORDER


def load_model(path):
    m = xgb.Booster()
    m.load_model(path)
    return m


def predict_one(model, rec):
    """Return action index (int) for one JSON record dict."""
    import numpy as np
    feats = featurize(rec)
    dm = xgb.DMatrix(feats.reshape(1, -1))
    return int(model.predict(dm)[0])


def handle_client(conn, model):
    """Service one client connection until it closes."""
    buf = b''
    try:
        while True:
            chunk = conn.recv(4096)
            if not chunk:
                break
            buf += chunk
            while b'\n' in buf:
                nl = buf.index(b'\n')
                line = buf[:nl].decode('ascii', errors='replace').strip()
                buf = buf[nl + 1:]
                if not line:
                    continue
                try:
                    rec  = json.loads(line)
                    idx  = predict_one(model, rec)
                    conn.sendall(bytes([idx]))
                except Exception as e:
                    # Send 'stay' so the monster doesn't crash the game
                    conn.sendall(bytes([_STAY_IDX]))
    except Exception:
        pass
    finally:
        try:
            conn.close()
        except Exception:
            pass


def main():
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--socket', default=DEFAULT_SOCK,
                        help=f'Unix socket path (default: {DEFAULT_SOCK})')
    parser.add_argument('--model',  default=DEFAULT_MODEL,
                        help=f'XGBoost model file (default: …/monster_model_combined.ubj)')
    args = parser.parse_args()

    # Clean up stale socket
    try:
        os.unlink(args.socket)
    except FileNotFoundError:
        pass

    print(f'Loading model from {args.model} …', flush=True)
    model = load_model(args.model)
    print('Model loaded.', flush=True)

    srv = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    srv.bind(args.socket)
    srv.listen(1)
    print(f'Listening on {args.socket}', flush=True)

    def _shutdown(sig, frame):
        srv.close()
        try:
            os.unlink(args.socket)
        except Exception:
            pass
        sys.exit(0)

    signal.signal(signal.SIGTERM, _shutdown)
    signal.signal(signal.SIGINT,  _shutdown)

    while True:
        try:
            conn, _ = srv.accept()
            print('Client connected.', flush=True)
            handle_client(conn, model)
            print('Client disconnected.', flush=True)
        except OSError:
            break


if __name__ == '__main__':
    main()
