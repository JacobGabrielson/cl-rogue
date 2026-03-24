#!/usr/bin/env python3
"""
collect_training_data.py — Self-play episode runner + Ollama LLM labeller.

Drives cl-rogue headlessly, observes monster positions each turn, queries
an Ollama LLM to label each monster's intended move, and writes
(state, action) pairs as JSONL.

Usage (from repo root, venv active):
    python driver/collect_training_data.py [options]

Options:
    --episodes N        Number of game episodes  (default: 5)
    --max-turns N       Max player turns per episode  (default: 300)
    --output PATH       JSONL file to append to  (default: training_data/data.jsonl)
    --model NAME        Ollama model  (default: llama3.2:1b)
    --ollama URL        Ollama base URL  (default: http://localhost:11434)
    --no-llm            Use A* expert only — no Ollama queries
"""

import argparse
import json
import os
import random
import re
import sys
import time

import requests

_HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, _HERE)

from rogue_driver import RogueDriver
from expert import DIRS, legal_moves, choose_action

# ── Monster name table (A–Z, matching init.lisp order) ──────────────────── #
MONSTER_NAMES = [
    "giant ant", "bat", "centaur", "dragon", "floating eye",
    "violet fungi", "gnome", "hobgoblin", "invisible stalker", "jackal",
    "kobold", "leprechaun", "mimic", "nymph", "orc",
    "purple worm", "quasit", "rust monster", "snake", "troll",
    "umber hulk", "vampire", "wraith", "xorn", "yeti", "zombie",
]

def monster_name(ch):
    idx = ord(ch.upper()) - ord('A')
    if 0 <= idx < len(MONSTER_NAMES):
        return MONSTER_NAMES[idx]
    return "unknown"


# ── Screen parsing ───────────────────────────────────────────────────────── #

def parse_status(line):
    """Return (hp, max_hp, level) from the status line."""
    m = re.search(r'Hp:\s*(\d+)\((\d+)\)', line)
    hp, max_hp = (int(m.group(1)), int(m.group(2))) if m else (1, 1)
    m = re.search(r'Level:\s*(\d+)', line)
    level = int(m.group(1)) if m else 1
    return hp, max_hp, level


def find_player(snap):
    """Return (row, col) of '@', or None."""
    for r, line in enumerate(snap):
        c = line.find('@')
        if c != -1:
            return r, c
    return None


def find_monsters(snap):
    """Return list of (row, col, char) for uppercase letters in dungeon rows."""
    results = []
    for r in range(1, 23):   # skip message row 0 and status row 23
        for c, ch in enumerate(snap[r]):
            if ch.isupper():
                results.append((r, c, ch))
    return results


def grid_window(snap, mr, mc, size=9):
    """Return a (size × size) ASCII window centred on (mr, mc).

    Out-of-bounds cells are filled with ' '.
    The monster's own cell is replaced with 'M'.
    """
    half = size // 2
    rows = []
    for dr in range(-half, half + 1):
        row = []
        for dc in range(-half, half + 1):
            r, c = mr + dr, mc + dc
            if dr == 0 and dc == 0:
                row.append('M')
            elif 1 <= r <= 22 and 0 <= c < len(snap[r]):
                row.append(snap[r][c])
            else:
                row.append(' ')
        rows.append(''.join(row))
    return rows


# ── Ollama labeller ──────────────────────────────────────────────────────── #

_VALID_ACTIONS = set('hjklyubn.')

_SYSTEM_PROMPT = (
    "You control a monster in a dungeon game. "
    "Respond with ONLY a single move character from the legal moves provided. "
    "No words, no explanation — just one character."
)

_USER_PROMPT_TEMPLATE = """\
You are a {name} ('{ch}'). You are aggressive — press the attack when there
is a reasonable chance of killing the player. Only retreat if nearly dead
AND the player is clearly winning.

Player (@) is at ({pr},{pc}), HP {php:.0f}%. You are at ({mr},{mc}).
{n_allies} ally monster(s) nearby.

Map (9x9, you=M):
{ascii_map}

Legal moves: {moves}
h=left l=right k=up j=down y=NW u=NE b=SW n=SE .=stay
Move:"""


def ollama_label(args, name, ch, mr, mc, pr, pc, php_frac, n_allies, window, moves):
    """Query Ollama via the chat API and return a valid action char, or None."""
    ascii_map = '\n'.join(window)
    moves_str = ' '.join(sorted(moves))
    user_msg = _USER_PROMPT_TEMPLATE.format(
        name=name, ch=ch, mr=mr, mc=mc, pr=pr, pc=pc,
        php=php_frac * 100, n_allies=n_allies,
        ascii_map=ascii_map, moves=moves_str,
    )
    try:
        resp = requests.post(
            f"{args.ollama}/api/chat",
            json={
                "model": args.model,
                "stream": False,
                "options": {"temperature": 0.7, "num_predict": 4},
                "messages": [
                    {"role": "system", "content": _SYSTEM_PROMPT},
                    {"role": "user",   "content": user_msg},
                ],
            },
            timeout=60,
        )
        resp.raise_for_status()
        text = resp.json().get("message", {}).get("content", "").strip()
        # Scan from the end in case the model adds punctuation after the char.
        moves_set = set(moves)
        for char in reversed(text):
            if char in _VALID_ACTIONS and char in moves_set:
                return char
        return None
    except Exception as e:
        print(f"  [ollama error: {e}]", file=sys.stderr)
        return None


# ── Player heuristic ─────────────────────────────────────────────────────── #

_MOVE_KEYS = list('hjklyubn')
_WALKABLE_PLAYER = frozenset('.#+%:!?*)]/=>~`^&{},')
_HUNGER_WORDS = ('hungry', 'weak', 'faint')


def player_move(snap, pr, pc, visited, turn, player_state):
    """Return a key (or keys) for the player.

    Implements a basic but effective survival strategy:
      1. Eat food when hungry (avoids starvation death)
      2. Attack adjacent monsters (fight back)
      3. Pick up items on current cell (food, potions)
      4. Descend stairs (progress levels for variety)
      5. Explore unvisited walkable tiles

    player_state is a mutable dict for inter-turn bookkeeping.
    Returns a single key string.
    """
    msg = snap[0].lower()

    # ── 1. Eat food if hungry ─────────────────────────────────────────────── #
    # 'e' opens eat prompt; we follow with 'a' to eat first inventory item.
    # Use a 2-step state machine so we don't flood the game with 'e' every turn.
    if player_state.get('eating') == 'sent_e':
        player_state['eating'] = None
        return 'a'   # confirm eat first food item
    if any(w in msg for w in _HUNGER_WORDS):
        player_state['eating'] = 'sent_e'
        return 'e'

    # ── 2. Attack an adjacent monster ────────────────────────────────────── #
    for d, (dr, dc) in DIRS.items():
        if d == '.':
            continue
        nr, nc = pr + dr, pc + dc
        if 0 <= nr < len(snap) and 0 <= nc < len(snap[nr]):
            if snap[nr][nc].isupper():
                return d   # bump into it to attack

    # ── 3. Pick up item on current cell ──────────────────────────────────── #
    if snap[pr][pc] in ':!?*)/]=':
        return ','

    # ── 4. Descend stairs if adjacent ────────────────────────────────────── #
    for r in range(max(1, pr - 1), min(23, pr + 2)):
        for c in range(max(0, pc - 1), min(80, pc + 2)):
            if snap[r][c] == '%' and (r != pr or c != pc):
                return '>'

    # ── 5. Explore unvisited walkable tiles ──────────────────────────────── #
    unvisited, walkable = [], []
    for key, (dr, dc) in DIRS.items():
        if key == '.':
            continue
        nr, nc = pr + dr, pc + dc
        if not (0 <= nr < len(snap) and 0 <= nc < len(snap[nr])):
            continue
        if snap[nr][nc] in _WALKABLE_PLAYER:
            walkable.append(key)
            if (nr, nc) not in visited:
                unvisited.append(key)

    if unvisited:
        return random.choice(unvisited)
    if walkable:
        return random.choice(walkable)
    return random.choice(_MOVE_KEYS)


# ── Episode runner ───────────────────────────────────────────────────────── #

def run_episode(args, episode_num, out_file):
    """Run one episode and write training examples to out_file."""
    binary = os.path.join(_HERE, '..', 'cl-rogue')
    n_examples = 0
    n_llm_attempts = 0
    n_llm_fallback = 0

    with RogueDriver(binary, rows=24, cols=80) as driver:
        ok = driver.wait_stable(timeout=10.0)
        if not ok:
            print(f"  Episode {episode_num}: game did not start, skipping.",
                  file=sys.stderr)
            return 0, 0

        visited = set()
        player_state = {}
        for turn in range(args.max_turns):
            snap = driver.snapshot()

            # Locate player
            player_pos = find_player(snap)
            if player_pos is None:
                break   # player died / screen changed
            pr, pc = player_pos
            visited.add((pr, pc))

            # Parse status line
            hp, max_hp, level = parse_status(snap[23])
            php_frac = hp / max_hp if max_hp > 0 else 1.0

            # Find all visible monsters
            monsters = find_monsters(snap)
            monster_cells = {(r, c) for r, c, _ in monsters}

            # When using the LLM, sample at most one monster per turn to
            # keep collection speed tractable on CPU.  The rest get the
            # A* expert label.
            if not args.no_llm and monsters:
                llm_idx = random.randrange(len(monsters))
            else:
                llm_idx = -1

            for idx, (mr, mc, ch) in enumerate(monsters):
                allies = [(r, c) for r, c, _ in monsters if (r, c) != (mr, mc)]
                n_allies = len(allies)

                window = grid_window(snap, mr, mc)
                moves = legal_moves(snap, mr, mc, pr, pc)

                action = None
                source = 'expert'   # default for non-LLM-sampled monsters

                if idx == llm_idx:
                    n_llm_attempts += 1
                    name = monster_name(ch)
                    action = ollama_label(
                        args, name, ch, mr, mc, pr, pc,
                        php_frac, n_allies, window, moves,
                    )
                    if action is not None:
                        source = 'llm'
                    else:
                        n_llm_fallback += 1
                        source = 'fallback'

                if action is None:
                    action = choose_action(
                        snap, mr, mc, pr, pc,
                        monster_hp_frac=1.0,
                        player_hp_frac=php_frac,
                        allies=allies,
                    )

                record = {
                    "episode": episode_num,
                    "turn": turn,
                    "dungeon_level": level,
                    "monster_type": ch,
                    "monster_name": monster_name(ch),
                    "mr": mr,
                    "mc": mc,
                    "pr": pr,
                    "pc": pc,
                    "player_hp": hp,
                    "player_max_hp": max_hp,
                    "player_hp_frac": round(php_frac, 3),
                    "n_allies": n_allies,
                    "grid_window": window,
                    "legal_moves": moves,
                    "action": action,
                    "source": source,
                }
                out_file.write(json.dumps(record) + '\n')
                n_examples += 1

            # Advance the game with a player move
            key = player_move(snap, pr, pc, visited, turn, player_state)
            driver.send_key(key)
            driver.wait_stable(timeout=3.0)

    return n_examples, (n_llm_attempts, n_llm_fallback)


# ── Main ─────────────────────────────────────────────────────────────────── #

def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--episodes',  type=int,   default=5,
                        help='Number of game episodes (default: 5)')
    parser.add_argument('--max-turns', type=int,   default=300,
                        help='Max player turns per episode (default: 300)')
    parser.add_argument('--output',    default='training_data/data.jsonl',
                        help='JSONL output path (default: training_data/data.jsonl)')
    parser.add_argument('--model',     default='llama3.2:1b',
                        help='Ollama model name (default: llama3.2:1b)')
    parser.add_argument('--ollama',    default='http://localhost:11434',
                        help='Ollama base URL (default: http://localhost:11434)')
    parser.add_argument('--no-llm',    action='store_true',
                        help='Use A* expert only, skip Ollama queries')
    args = parser.parse_args()

    os.makedirs(os.path.dirname(os.path.abspath(args.output)), exist_ok=True)

    total_examples = 0
    total_llm_attempts = 0
    total_llm_fallback = 0

    with open(args.output, 'a') as out_file:
        for ep in range(args.episodes):
            print(f"Episode {ep + 1}/{args.episodes} ...", flush=True)
            n, (n_att, n_fail) = run_episode(args, ep, out_file)
            total_examples += n
            total_llm_attempts += n_att
            total_llm_fallback += n_fail
            if n_att:
                pct = 100 * n_fail / n_att
                print(f"  {n} examples  (LLM: {n_att - n_fail} ok, {n_fail} fallback = {pct:.0f}%)")
            else:
                print(f"  {n} examples  (expert-only)")

    print(f"\nDone. Total: {total_examples} examples written to {args.output}")
    if total_llm_attempts and not args.no_llm:
        pct = 100 * total_llm_fallback / total_llm_attempts
        print(f"Overall LLM fallback rate: {pct:.1f}%  "
              f"({'OK' if pct < 20 else 'HIGH — consider a larger model'})")


if __name__ == '__main__':
    main()
