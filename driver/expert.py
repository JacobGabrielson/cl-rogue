"""
expert.py — A* pathfinder + tactical-rules fallback for monster action selection.

Called by collect_training_data.py when the LLM returns an illegal move.

Public API:
    legal_moves(grid, mr, mc, pr, pc)       -> list of legal direction chars
    choose_action(grid, mr, mc, pr, pc, ...) -> one direction char
"""

import heapq

# direction char -> (delta_row, delta_col)
DIRS = {
    'h': ( 0, -1),   # left
    'l': ( 0, +1),   # right
    'k': (-1,  0),   # up
    'j': (+1,  0),   # down
    'y': (-1, -1),   # up-left
    'u': (-1, +1),   # up-right
    'b': (+1, -1),   # down-left
    'n': (+1, +1),   # down-right
    '.': ( 0,  0),   # stay
}

# Tile chars a monster can walk through (items, traps, stairs, etc. are fine)
_WALKABLE = frozenset('.#+%:!?*)]/=>~`^&{},')


def _is_walkable(ch):
    return ch in _WALKABLE


def legal_moves(grid, mr, mc, pr, pc):
    """Return sorted list of direction chars the monster can legally take.

    The player's cell always counts as walkable (attack move).
    Uppercase letters (other monsters) are not walkable.
    """
    rows = len(grid)
    cols = len(grid[0]) if rows else 0
    moves = ['.']  # stay is always legal
    for d, (dr, dc) in DIRS.items():
        if d == '.':
            continue
        nr, nc = mr + dr, mc + dc
        if not (0 <= nr < rows and 0 <= nc < cols):
            continue
        ch = grid[nr][nc]
        if (nr == pr and nc == pc) or _is_walkable(ch):
            moves.append(d)
    return sorted(moves)


def _astar(grid, start, goal):
    """Return the first step toward goal as (nr, nc), or None if unreachable.

    The goal cell is always treated as passable (attack on player cell).
    """
    rows = len(grid)
    cols = len(grid[0]) if rows else 0

    def h(r, c):
        return abs(r - goal[0]) + abs(c - goal[1])

    # heap entries: (f, g, pos, first_step_or_None)
    heap = [(h(*start), 0, start, None)]
    visited = set()

    while heap:
        f, g, pos, first = heapq.heappop(heap)
        if pos in visited:
            continue
        visited.add(pos)
        if pos == goal:
            return first  # None means start == goal
        r, c = pos
        for d, (dr, dc) in DIRS.items():
            if d == '.':
                continue
            nr, nc = r + dr, c + dc
            if (nr, nc) in visited:
                continue
            if not (0 <= nr < rows and 0 <= nc < cols):
                continue
            ch = grid[nr][nc]
            is_goal = (nr, nc) == goal
            if is_goal or _is_walkable(ch):
                ng = g + 1
                nfirst = first if first is not None else (nr, nc)
                heapq.heappush(heap, (ng + h(nr, nc), ng, (nr, nc), nfirst))
    return None


def _delta_to_dir(dr, dc):
    for d, (r, c) in DIRS.items():
        if r == dr and c == dc:
            return d
    return '.'


def choose_action(grid, mr, mc, pr, pc,
                  monster_hp_frac=1.0, player_hp_frac=1.0, allies=None):
    """Choose the best tactical action for the monster at (mr, mc).

    Parameters
    ----------
    grid            : list of strings, full 24-row screen snapshot
    mr, mc          : monster row/col
    pr, pc          : player row/col
    monster_hp_frac : monster current HP / max HP  (0-1; default 1.0)
    player_hp_frac  : player current HP / max HP   (0-1; default 1.0)
    allies          : list of (r, c) positions of other monsters, or None

    Returns
    -------
    One direction char from 'hjklyubn.'
    """
    if allies is None:
        allies = []

    legal = set(legal_moves(grid, mr, mc, pr, pc))

    # ── 1. Attack ────────────────────────────────────────────────────────── #
    if abs(mr - pr) <= 1 and abs(mc - pc) <= 1 and (mr != pr or mc != pc):
        dr = max(-1, min(1, pr - mr))
        dc = max(-1, min(1, pc - mc))
        d = _delta_to_dir(dr, dc)
        if d in legal:
            return d

    # ── 2. Retreat ───────────────────────────────────────────────────────── #
    # Only flee when HP is very low AND the player is clearly winning.
    # Monsters are biased toward aggression and rarely retreat.
    if monster_hp_frac < 0.15 and player_hp_frac > monster_hp_frac:
        best_d = '.'
        best_dist = abs(mr - pr) + abs(mc - pc)
        for d in legal:
            if d == '.':
                continue
            dr, dc = DIRS[d]
            nr, nc = mr + dr, mc + dc
            dist = abs(nr - pr) + abs(nc - pc)
            if dist > best_dist:
                best_dist = dist
                best_d = d
        return best_d

    # ── 3. Flank ─────────────────────────────────────────────────────────── #
    # If an ally is already closer to the player, approach from the other side.
    closer_allies = [
        (ar, ac) for ar, ac in allies
        if abs(ar - pr) + abs(ac - pc) < abs(mr - pr) + abs(mc - pc)
    ]
    if closer_allies:
        ally_r, ally_c = closer_allies[0]
        best_d, best_score = None, float('-inf')
        for d in legal:
            if d == '.':
                continue
            dr, dc = DIRS[d]
            nr, nc = mr + dr, mc + dc
            dist_to_player = abs(nr - pr) + abs(nc - pc)
            dist_to_ally   = abs(nr - ally_r) + abs(nc - ally_c)
            # maximise separation from ally while closing on player
            score = dist_to_ally - dist_to_player
            if score > best_score:
                best_score, best_d = score, d
        if best_d and best_d in legal:
            return best_d

    # ── 4. Chase via A* ──────────────────────────────────────────────────── #
    first_step = _astar(grid, (mr, mc), (pr, pc))
    if first_step is not None:
        nr, nc = first_step
        d = _delta_to_dir(nr - mr, nc - mc)
        if d in legal:
            return d

    # ── 5. Greedy fallback ───────────────────────────────────────────────── #
    best_d, best_dist = '.', abs(mr - pr) + abs(mc - pc)
    for d in legal:
        if d == '.':
            continue
        dr, dc = DIRS[d]
        nr, nc = mr + dr, mc + dc
        dist = abs(nr - pr) + abs(nc - pc)
        if dist < best_dist:
            best_dist, best_d = dist, d
    return best_d
