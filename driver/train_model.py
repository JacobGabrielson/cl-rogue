#!/usr/bin/env python3
"""
train_model.py — Feature engineering + XGBoost classifier for monster AI.

Reads the JSONL produced by collect_training_data.py, builds a feature
vector for each record, trains an XGBoost multi-class classifier, and
reports top-1 accuracy plus a per-monster-type confusion matrix.

The trained model is saved to model/monster_model.ubj (XGBoost binary).

Usage (from repo root, venv active):
    python driver/train_model.py [options]

Options:
    --data PATH     JSONL file to read  (default: training_data/data.jsonl)
    --model PATH    Where to save the model  (default: model/monster_model.ubj)
    --val-split F   Fraction held out for validation  (default: 0.1)
    --test-split F  Fraction held out for final test  (default: 0.1)
    --rounds N      XGBoost boosting rounds  (default: 300)
    --seed N        Random seed  (default: 42)
"""

import argparse
import json
import os
import sys

import numpy as np
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import train_test_split
import xgboost as xgb

# ── Action label mapping ─────────────────────────────────────────────────── #
# Laid out as the rogue numpad:  7 8 9 → y k u
#                                4 5 6 → h . l
#                                1 2 3 → b j n
ACTION_ORDER = ['y', 'k', 'u', 'h', '.', 'l', 'b', 'j', 'n']
ACTION_TO_IDX = {ch: i for i, ch in enumerate(ACTION_ORDER)}
N_CLASSES = len(ACTION_ORDER)

# ── Tile classification (must match doc/model-driven-monsters.md) ─────────── #
_TILE_MAP = {
    ' ':  0,   # unseen / empty
    '.':  1,   # floor
    '#':  2,   # passage
    '|':  3,   # wall
    '-':  3,   # wall
    '+':  4,   # door
    '@':  5,   # player
}
_ITEM_CHARS = frozenset(':!?*)]/=>~`^&{},')
N_TILE_CLASSES = 8


def tile_class(ch):
    if ch in _TILE_MAP:
        return _TILE_MAP[ch]
    if ch.isupper() or ch == 'M':   # monster (M = the monster itself)
        return 6
    if ch in _ITEM_CHARS:
        return 7
    return 0   # unknown → unseen


# ── Feature engineering ──────────────────────────────────────────────────── #
# Total feature count:
#   9×9 grid × 8 one-hot classes  =  648
#   monster type one-hot (A–Z)    =   26
#   distance to player (norm.)    =    1
#   direction to player (dr, dc)  =    2
#   player HP fraction            =    1
#   monster HP fraction           =    1
#   n_allies (norm.)              =    1
#   player visible in window      =    1
#                                 -------
#                                   681

_MAP_DIAG = (24**2 + 80**2) ** 0.5   # ≈ 83, used to normalise distance


def featurize(record):
    """Return a flat numpy float32 array for one JSONL record."""
    feats = []

    # ── 9×9 grid one-hot ────────────────────────────────────────────────── #
    window = record['grid_window']
    for row in window:
        for ch in row:
            oh = [0.0] * N_TILE_CLASSES
            # The monster's own cell is 'M'; treat as floor for the grid feat.
            oh[tile_class(ch)] = 1.0
            feats.extend(oh)

    # ── Monster type one-hot ─────────────────────────────────────────────── #
    mt = [0.0] * 26
    idx = ord(record['monster_type'].upper()) - ord('A')
    if 0 <= idx < 26:
        mt[idx] = 1.0
    feats.extend(mt)

    # ── Scalar features ──────────────────────────────────────────────────── #
    mr, mc = record['mr'], record['mc']
    pr, pc = record['pr'], record['pc']

    manhattan = abs(mr - pr) + abs(mc - pc)

    # Distance (normalised)
    feats.append(manhattan / _MAP_DIAG)

    # Direction to player (unit vector in Manhattan metric)
    if manhattan > 0:
        feats.append((pr - mr) / manhattan)
        feats.append((pc - mc) / manhattan)
    else:
        feats.extend([0.0, 0.0])

    # Player HP fraction
    feats.append(float(record.get('player_hp_frac', 1.0)))

    # Monster HP fraction (1.0 for all current data; placeholder for future)
    feats.append(float(record.get('monster_hp_frac', 1.0)))

    # n_allies (normalised; cap at 8)
    feats.append(min(record.get('n_allies', 0) / 8.0, 1.0))

    # Player visible in window
    feats.append(1.0 if any('@' in row for row in window) else 0.0)

    return np.array(feats, dtype=np.float32)


# ── Data loading ─────────────────────────────────────────────────────────── #

def load_dataset(path):
    records, X_list, y_list = [], [], []
    skipped = 0
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            rec = json.loads(line)
            action = rec.get('action', '')
            if action not in ACTION_TO_IDX:
                skipped += 1
                continue
            records.append(rec)
            X_list.append(featurize(rec))
            y_list.append(ACTION_TO_IDX[action])

    if skipped:
        print(f"  Skipped {skipped} records with unknown action labels.")

    X = np.stack(X_list)
    y = np.array(y_list, dtype=np.int32)
    print(f"  Loaded {len(X)} examples, {X.shape[1]} features, {N_CLASSES} classes.")
    return X, y, records


# ── Training ─────────────────────────────────────────────────────────────── #

def train(args):
    print(f"Loading {args.data} ...")
    X, y, records = load_dataset(args.data)

    # Split: train / val / test
    X_tv, X_test, y_tv, y_test = train_test_split(
        X, y, test_size=args.test_split, random_state=args.seed, stratify=y)
    val_frac_of_tv = args.val_split / (1.0 - args.test_split)
    X_train, X_val, y_train, y_val = train_test_split(
        X_tv, y_tv, test_size=val_frac_of_tv, random_state=args.seed, stratify=y_tv)

    print(f"Split: train={len(X_train)}  val={len(X_val)}  test={len(X_test)}")

    dtrain = xgb.DMatrix(X_train, label=y_train)
    dval   = xgb.DMatrix(X_val,   label=y_val)
    dtest  = xgb.DMatrix(X_test,  label=y_test)

    params = {
        'objective':        'multi:softmax',
        'num_class':        N_CLASSES,
        'eval_metric':      'merror',
        'eta':              0.1,
        'max_depth':        6,
        'subsample':        0.8,
        'colsample_bytree': 0.8,
        'seed':             args.seed,
        'nthread':          -1,
    }

    print(f"\nTraining XGBoost ({args.rounds} rounds) ...")
    evals_result = {}
    model = xgb.train(
        params,
        dtrain,
        num_boost_round=args.rounds,
        evals=[(dtrain, 'train'), (dval, 'val')],
        evals_result=evals_result,
        early_stopping_rounds=30,
        verbose_eval=50,
    )

    # ── Evaluation ──────────────────────────────────────────────────────── #
    def accuracy(dmat, y_true):
        preds = model.predict(dmat).astype(int)
        return (preds == y_true).mean()

    train_acc = accuracy(dtrain, y_train)
    val_acc   = accuracy(dval,   y_val)
    test_acc  = accuracy(dtest,  y_test)

    print(f"\n{'='*50}")
    print(f"  Train accuracy : {train_acc*100:.1f}%")
    print(f"  Val   accuracy : {val_acc*100:.1f}%")
    print(f"  Test  accuracy : {test_acc*100:.1f}%")
    print(f"{'='*50}")

    if test_acc < 0.80:
        print("  ⚠  Below 80% target — consider more data or CNN (Option B).")
    else:
        print("  ✓  Meets 80% target.")

    # ── Per-monster confusion ────────────────────────────────────────────── #
    # Map test indices back to their monster types
    test_start = len(X_train) + len(X_val)
    # (records list is in original order, same as X)
    # Re-split indices to recover test set records
    all_idx = np.arange(len(X))
    tv_idx, test_idx = train_test_split(
        all_idx, test_size=args.test_split, random_state=args.seed,
        stratify=y)
    test_records = [records[i] for i in test_idx]

    preds_test = model.predict(dtest).astype(int)

    monster_types = sorted(set(r['monster_type'] for r in test_records))
    print(f"\nPer-monster test accuracy:")
    for mt in monster_types:
        mask = np.array([r['monster_type'] == mt for r in test_records])
        if mask.sum() == 0:
            continue
        acc = (preds_test[mask] == y_test[mask]).mean()
        n = mask.sum()
        name = _MONSTER_NAMES.get(mt, mt)
        flag = '' if acc >= 0.80 else '  ← needs more data'
        print(f"  {mt} ({name:20s})  n={n:4d}  acc={acc*100:.1f}%{flag}")

    # ── Save model ───────────────────────────────────────────────────────── #
    os.makedirs(os.path.dirname(os.path.abspath(args.model)), exist_ok=True)
    model.save_model(args.model)
    print(f"\nModel saved to {args.model}  "
          f"({os.path.getsize(args.model) / 1024:.0f} KB)")

    # Also save feature metadata alongside the model
    meta_path = args.model.replace('.ubj', '_meta.json')
    meta = {
        'action_order': ACTION_ORDER,
        'n_features': int(X.shape[1]),
        'n_classes': N_CLASSES,
        'n_train': len(X_train),
        'test_acc': round(float(test_acc), 4),
    }
    with open(meta_path, 'w') as f:
        json.dump(meta, f, indent=2)
    print(f"Metadata saved to {meta_path}")


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


# ── Main ─────────────────────────────────────────────────────────────────── #

def main():
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--data',       default='training_data/data.jsonl')
    parser.add_argument('--model',      default='model/monster_model.ubj')
    parser.add_argument('--val-split',  type=float, default=0.1)
    parser.add_argument('--test-split', type=float, default=0.1)
    parser.add_argument('--rounds',     type=int,   default=300)
    parser.add_argument('--seed',       type=int,   default=42)
    args = parser.parse_args()
    train(args)


if __name__ == '__main__':
    main()
