#!/usr/bin/env python3
"""
observe.py — Headless game observer / Claude-as-player harness.

Runs cl-rogue with the heuristic player and prints annotated screen
snapshots whenever monsters are visible, so you can evaluate AI behaviour
without a terminal.

Usage (from repo root, venv active):
    python driver/observe.py [--turns N] [--every K]

Options:
    --turns N   Max player turns to run  (default: 60)
    --every K   Print snapshot every K turns  (default: 1)
    --monsters  Only print when at least one monster is visible
"""

import argparse
import os
import random
import re
import sys

_HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, _HERE)

from rogue_driver import RogueDriver
from collect_training_data import (
    find_player, find_monsters, parse_status, player_move
)

BINARY = os.path.join(_HERE, '..', 'cl-rogue')


def render_snapshot(snap, turn, hp, max_hp, monsters):
    """Pretty-print one screen frame."""
    sep = '─' * 80
    print(f"\n{sep}")
    print(f"Turn {turn:3d}   HP {hp}/{max_hp}   Monsters visible: {len(monsters)}")
    print(sep)
    # Show rows 1–22 (dungeon only, skip message/status lines)
    for r in range(1, 23):
        line = snap[r] if r < len(snap) else ''
        print(f"{r:2d}│{line}")
    print(sep)
    if monsters:
        print("Monsters:")
        for mr, mc, ch in monsters:
            print(f"  {ch} at ({mr},{mc})")
    print()


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--turns',    type=int, default=60)
    parser.add_argument('--every',    type=int, default=1)
    parser.add_argument('--monsters', action='store_true',
                        help='Only print when monsters are visible')
    args = parser.parse_args()

    with RogueDriver(BINARY, rows=24, cols=80) as driver:
        ok = driver.wait_stable(timeout=10.0)
        if not ok:
            print("Game didn't start.", file=sys.stderr)
            return

        visited      = set()
        player_state = {}

        for turn in range(args.max_turns if hasattr(args, 'max_turns') else args.turns):
            snap = driver.snapshot()

            player_pos = find_player(snap)
            if player_pos is None:
                print(f"Turn {turn}: player not found (dead?)")
                break
            pr, pc = player_pos
            visited.add((pr, pc))

            hp, max_hp, level = parse_status(snap[23])
            monsters = find_monsters(snap)

            should_print = (turn % args.every == 0)
            if args.monsters:
                should_print = should_print and bool(monsters)

            if should_print:
                render_snapshot(snap, turn, hp, max_hp, monsters)

            key = player_move(snap, pr, pc, visited, turn, player_state)
            driver.send_key(key)
            driver.wait_stable(timeout=3.0)

        # Final state
        snap = driver.snapshot()
        player_pos = find_player(snap)
        if player_pos:
            pr, pc = player_pos
            hp, max_hp, level = parse_status(snap[23])
            monsters = find_monsters(snap)
            render_snapshot(snap, args.turns, hp, max_hp, monsters)
        print("Done.")


if __name__ == '__main__':
    main()
