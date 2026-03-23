#!/usr/bin/env python3
"""
example.py — Demonstrate the RogueDriver headless game interface.

Run from the driver/ directory with the venv active:
    source venv/bin/activate
    python example.py

Or directly:
    ./venv/bin/python example.py

The cl-rogue binary must exist at ../cl-rogue (build it first with
`make` or `sbcl --load build.lisp` from the repo root).
"""

import sys
import time
from rogue_driver import RogueDriver


# Arrow-key escape sequences (vi-style alternatives: h j k l)
LEFT  = "h"
DOWN  = "j"
UP    = "k"
RIGHT = "l"


def print_screen(driver, label=""):
    if label:
        print(f"\n{'='*60}")
        print(f"  {label}")
        print('='*60)
    print(driver.snapshot_str())
    pos = driver.find_player()
    if pos:
        print(f"  [player @ row={pos[0]}, col={pos[1]}]")
    else:
        print("  [player not found on screen]")


def walk(driver, keys, label=None):
    """Send a sequence of movement keys and wait for the screen to settle."""
    driver.send_keys(keys)
    driver.wait_stable()
    if label:
        print_screen(driver, label)


def find_monsters(driver):
    """Return a dict of monster char -> list of (row, col) positions.

    Excludes the bottom status line (row 23) which contains uppercase letters
    in words like 'Level', 'Gold', 'Hp', 'Str', 'Ac', 'Exp'.
    """
    snap = driver.snapshot()
    dungeon_rows = snap[1:-1]   # skip message row 0 and status row 23
    monsters = {}
    for r, line in enumerate(dungeon_rows, start=1):
        for c, cell in enumerate(line):
            if cell.isupper():
                monsters.setdefault(cell, []).append((r, c))
    return monsters


def main():
    binary = "../cl-rogue"

    print("Starting cl-rogue headless driver example...")
    print(f"Binary: {binary}")
    print()

    with RogueDriver(binary, rows=24, cols=80) as driver:

        # ── Initial screen ────────────────────────────────────────────── #
        ok = driver.wait_stable(timeout=10.0)
        if not ok:
            print("ERROR: game did not become stable after 10 s", file=sys.stderr)
            sys.exit(1)

        print_screen(driver, "Initial dungeon")

        pos = driver.find_player()
        if pos is None:
            print("ERROR: could not locate player '@' on screen", file=sys.stderr)
            sys.exit(1)

        # ── Show inventory ────────────────────────────────────────────── #
        driver.send_key("i")
        driver.wait_stable()
        print_screen(driver, "Inventory ('i')")
        driver.send_key(" ")   # dismiss
        driver.wait_stable()

        # ── Show help ─────────────────────────────────────────────────── #
        driver.send_key("?")
        driver.wait_stable()
        print_screen(driver, "Help screen ('?')")
        driver.send_key(" ")
        driver.wait_stable()

        # ── Walk around ───────────────────────────────────────────────── #
        walk(driver, [RIGHT, RIGHT, RIGHT], "After 3 steps right")
        walk(driver, [DOWN,  DOWN,  DOWN],  "After 3 steps down")
        walk(driver, [LEFT,  LEFT,  LEFT],  "After 3 steps left")
        walk(driver, [UP,    UP,    UP],    "After 3 steps up")

        # ── Status / message lines ────────────────────────────────────── #
        print("\n" + "="*60)
        print("  Status / message lines")
        print("="*60)
        print(f"  msg:    {repr(driver.message_line())}")
        print(f"  status: {repr(driver.status_line())}")

        # ── Monster scan ──────────────────────────────────────────────── #
        monsters = find_monsters(driver)
        print("\n" + "="*60)
        print("  Visible monsters")
        print("="*60)
        if monsters:
            for ch, positions in sorted(monsters.items()):
                for r, c in positions:
                    print(f"  '{ch}' at row={r}, col={c}")
        else:
            print("  No monsters visible on current screen.")

        # ── Walk toward the nearest monster (if any) ──────────────────── #
        if monsters and driver.find_player():
            pr, pc = driver.find_player()
            # pick the closest one
            best_ch = None
            best_dist = float("inf")
            best_pos = None
            for ch, positions in monsters.items():
                for mr, mc in positions:
                    d = abs(mr - pr) + abs(mc - pc)
                    if d < best_dist:
                        best_dist = d
                        best_ch   = ch
                        best_pos  = (mr, mc)

            if best_pos:
                mr, mc = best_pos
                print(f"\nWalking toward '{best_ch}' at ({mr},{mc}), "
                      f"distance {best_dist} from player ({pr},{pc})")
                steps = []
                for _ in range(abs(mr - pr)):
                    steps.append(DOWN if mr > pr else UP)
                for _ in range(abs(mc - pc)):
                    steps.append(RIGHT if mc > pc else LEFT)
                walk(driver, steps, f"After walking toward '{best_ch}'")

        # ── Final snapshot ────────────────────────────────────────────── #
        print_screen(driver, "Final state")

    print("\nDriver closed cleanly.")


if __name__ == "__main__":
    main()
