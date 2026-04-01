#!/usr/bin/env python3
"""
collect_lisp_training.py — Automated data collection for model retraining.

Runs headless cl-rogue games with a monster-seeking agent. The Lisp side
collects (state, action) pairs in /tmp/cl-rogue-training.jsonl using the
exact same feature builder as inference, ensuring training/inference parity.

Usage:
    source venv/bin/activate
    python collect_lisp_training.py --games 200 --output training_data/lisp_v1.jsonl
"""

import argparse
import os
import random
import re
import shutil
import string
import sys
import time

_HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, _HERE)

from rogue_driver import RogueDriver

BINARY = os.path.join(_HERE, "..", "cl-rogue")
LISP_TRAINING_FILE = "/tmp/cl-rogue-training.jsonl"

MONSTER_CHARS = set(string.ascii_uppercase) - {'@'}
MOVES = list("hjklyubn")

# Direction keys for moving toward a target
DIR_MAP = {
    (-1, -1): 'y', (-1, 0): 'k', (-1, 1): 'u',
    ( 0, -1): 'h',              ( 0, 1): 'l',
    ( 1, -1): 'b', ( 1, 0): 'j', ( 1, 1): 'n',
}


def sign(x):
    return (x > 0) - (x < 0)


def count_lines(path):
    try:
        with open(path) as f:
            return sum(1 for _ in f)
    except FileNotFoundError:
        return 0


def find_monsters(snapshot):
    """Find all monster positions on screen (rows 1-21, the dungeon area)."""
    monsters = []
    for r in range(1, 22):
        for c, ch in enumerate(snapshot[r]):
            if ch in MONSTER_CHARS:
                monsters.append((r, c, ch))
    return monsters


def move_toward(pr, pc, tr, tc):
    """Return direction key to move from (pr,pc) toward (tr,tc)."""
    dr, dc = sign(tr - pr), sign(tc - pc)
    return DIR_MAP.get((dr, dc), random.choice(MOVES))


def parse_hp(status_line):
    """Extract current/max HP from the status bar."""
    m = re.search(r'Hp:\s*(\d+)\((\d+)\)', status_line)
    if m:
        return int(m.group(1)), int(m.group(2))
    return None, None


def play_one_game(game_num, max_steps=3000):
    """Play one game with a monster-seeking agent."""
    try:
        os.unlink(LISP_TRAINING_FILE)
    except FileNotFoundError:
        pass

    try:
        with RogueDriver(BINARY, rows=24, cols=80, idle_secs=0.08) as driver:
            ok = driver.wait_stable(timeout=10.0)
            if not ok:
                return 0

            last_pos = None
            stuck = 0
            explore_dir = random.choice(MOVES)

            for step in range(max_steps):
                snap = driver.snapshot()
                pos = driver.find_player()
                if pos is None:
                    break

                pr, pc = pos

                # Detect stuck
                if pos == last_pos:
                    stuck += 1
                else:
                    stuck = 0
                last_pos = pos

                # Game over check
                msg = driver.message_line()
                if msg and ("press return" in msg.lower() or
                            "top ten" in msg.lower()):
                    driver.send_key("\r")
                    driver.wait_stable(timeout=2.0)
                    break

                # Respond to "--More--" prompts
                if msg and "--more--" in msg.lower():
                    driver.send_key(" ")
                    driver.wait_stable(timeout=1.0)
                    continue

                # Find visible monsters
                monsters = find_monsters(snap)

                if monsters:
                    # Target nearest monster
                    nearest = min(monsters, key=lambda m: abs(m[0]-pr) + abs(m[1]-pc))
                    tr, tc, mch = nearest
                    dist = abs(tr - pr) + abs(tc - pc)

                    # Move toward monster
                    key = move_toward(pr, pc, tr, tc)
                    driver.send_key(key)
                    driver.wait_stable(timeout=1.0)

                elif stuck > 8:
                    # Change direction when stuck
                    explore_dir = random.choice(MOVES)
                    driver.send_key(explore_dir)
                    driver.wait_stable(timeout=0.5)
                    stuck = 0

                else:
                    # Explore: keep going in current direction, occasionally turn
                    if random.random() < 0.15:
                        explore_dir = random.choice(MOVES)
                    driver.send_key(explore_dir)
                    driver.wait_stable(timeout=0.5)

                # Eat food if hungry (check message line)
                msg = driver.message_line()
                if msg and ("hungry" in msg.lower() or "weak" in msg.lower()):
                    driver.send_key("e")  # eat
                    driver.wait_stable(timeout=1.0)
                    # Select food if prompted
                    snap2 = driver.snapshot()
                    msg2 = snap2[0] if snap2 else ""
                    if "which" in msg2.lower() or "what" in msg2.lower():
                        driver.send_key("f")  # food is usually 'f'
                        driver.wait_stable(timeout=1.0)

            n_examples = count_lines(LISP_TRAINING_FILE)
            return n_examples

    except Exception as e:
        print(f"  Game {game_num}: error: {e}", file=sys.stderr)
        return 0


def main():
    parser = argparse.ArgumentParser(
        description="Collect training data from Lisp-side feature builder")
    parser.add_argument("--games", type=int, default=200,
                        help="Number of games to play (default: 200)")
    parser.add_argument("--max-steps", type=int, default=3000,
                        help="Max steps per game (default: 3000)")
    parser.add_argument("--output", default=None,
                        help="Output JSONL path")
    args = parser.parse_args()

    if args.output is None:
        args.output = os.path.join(_HERE, "..", "training_data", "lisp_v1.jsonl")

    os.makedirs(os.path.dirname(os.path.abspath(args.output)), exist_ok=True)
    open(args.output, "w").close()

    total = 0
    print(f"Collecting: {args.games} games, max {args.max_steps} steps")
    print(f"Output: {args.output}\n")

    for i in range(1, args.games + 1):
        n = play_one_game(i, max_steps=args.max_steps)
        total += n

        if n > 0 and os.path.exists(LISP_TRAINING_FILE):
            with open(LISP_TRAINING_FILE) as src, \
                 open(args.output, "a") as dst:
                shutil.copyfileobj(src, dst)

        print(f"  Game {i:3d}/{args.games}: {n:4d} examples  (total: {total:6d})")

    print(f"\nDone. {total} total examples in {args.output}")


if __name__ == "__main__":
    main()
