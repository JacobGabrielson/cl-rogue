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
import logging
import os
import signal
import socket
import sys
import time

_HERE = os.path.dirname(os.path.abspath(__file__))
_REPO = os.path.dirname(_HERE)
sys.path.insert(0, _HERE)

from train_model import featurize
import xgboost as xgb

DEFAULT_SOCK  = '/tmp/cl-rogue-model.sock'
DEFAULT_MODEL = os.path.join(_REPO, 'model', 'monster_model_v3.ubj')

ACTION_ORDER = ['y', 'k', 'u', 'h', '.', 'l', 'b', 'j', 'n']
_STAY_IDX    = 4   # index of '.' in ACTION_ORDER

# Rough monster name table (A–Z)
_MONSTER_NAMES = {
    'A': 'giant ant',   'B': 'bat',         'C': 'centaur',
    'D': 'dragon',      'E': 'floating eye', 'F': 'violet fungi',
    'G': 'gnome',       'H': 'hobgoblin',   'I': 'inv. stalker',
    'J': 'jackal',      'K': 'kobold',      'L': 'leprechaun',
    'M': 'mimic',       'N': 'nymph',       'O': 'orc',
    'P': 'purple worm', 'Q': 'quasit',      'R': 'rust monster',
    'S': 'snake',       'T': 'troll',       'U': 'umber hulk',
    'V': 'vampire',     'W': 'wraith',      'X': 'xorn',
    'Y': 'yeti',        'Z': 'zombie',
}

def load_model(path):
    m = xgb.Booster()
    m.load_model(path)
    return m


def predict_one(model, rec):
    """Return (action_index, action_char) for one JSON record dict."""
    import numpy as np
    feats = featurize(rec)
    dm    = xgb.DMatrix(feats.reshape(1, -1))
    idx   = int(model.predict(dm)[0])
    return idx, ACTION_ORDER[idx]


def fmt_rec(rec):
    """Short human-readable summary of a query record."""
    mt    = rec.get('monster_type', '?')
    name  = _MONSTER_NAMES.get(mt.upper(), mt)
    mr, mc = rec.get('mr', -1), rec.get('mc', -1)
    pr, pc = rec.get('pr', -1), rec.get('pc', -1)
    php   = rec.get('player_hp_frac', 1.0)
    mhp   = rec.get('monster_hp_frac', 1.0)
    dist  = abs(mr - pr) + abs(mc - pc)
    return (f"{mt}({name}) @({mr},{mc})  player @({pr},{pc})  "
            f"dist={dist}  pHP={php:.0%}  mHP={mhp:.0%}")


log = logging.getLogger('model_server')


def handle_client(conn, model):
    """Service one client connection until it closes."""
    n_queries = 0
    t_start   = time.monotonic()
    buf = b''
    try:
        while True:
            chunk = conn.recv(4096)
            if not chunk:
                break
            buf += chunk
            while b'\n' in buf:
                nl   = buf.index(b'\n')
                line = buf[:nl].decode('ascii', errors='replace').strip()
                buf  = buf[nl + 1:]
                if not line:
                    continue
                try:
                    rec        = json.loads(line)
                    idx, act   = predict_one(model, rec)
                    conn.sendall(bytes([idx]))
                    n_queries += 1
                    grid = rec.get('grid_window', [])
                    log.info("[%4d] %s  → %r (idx=%d)",
                             n_queries, fmt_rec(rec), act, idx)
                    for i, row in enumerate(grid):
                        tag = ' >>> ' if i == 4 else '     '
                        log.info("  %s|%s|", tag, row)
                except Exception as e:
                    conn.sendall(bytes([_STAY_IDX]))
                    log.error("[ERR ] %s  RAW: %r", e, line[:200])
    except Exception:
        pass
    finally:
        elapsed = time.monotonic() - t_start
        log.info("— %d queries in %.1fs (%.1f ms/query avg)",
                 n_queries, elapsed,
                 elapsed / max(n_queries, 1) * 1000)
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
                        help='XGBoost model file')
    parser.add_argument('--log', default=None,
                        help='Log file path (default: stdout only)')
    args = parser.parse_args()

    # Set up logging to stdout + optional file
    handlers = [logging.StreamHandler()]
    if args.log:
        handlers.append(logging.FileHandler(args.log, mode='w'))
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s %(message)s',
        datefmt='%H:%M:%S',
        handlers=handlers,
    )

    # Clean up stale socket
    try:
        os.unlink(args.socket)
    except FileNotFoundError:
        pass

    log.info('Loading model from %s …', args.model)
    model = load_model(args.model)
    log.info('Model loaded.')

    srv = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    srv.bind(args.socket)
    srv.listen(1)
    log.info('Listening on %s', args.socket)
    if args.log:
        log.info('Logging to %s', args.log)

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
            log.info('── Client connected ──────────────────────────────')
            handle_client(conn, model)
            log.info('── Client disconnected ───────────────────────────')
        except OSError:
            break


if __name__ == '__main__':
    main()
