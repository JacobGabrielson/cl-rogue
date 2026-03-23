# Model-Driven Monster AI

## Overview

The original Rogue monster AI is a pure greedy pathfinder: every monster
that is "running" (ISRUN flag set) calls `do-chase` each turn, which
picks the adjacent cell that minimises Manhattan distance to the target.
Confused monsters move randomly; Invisible Stalkers and Bats have a
higher confusion probability. That's it.

The goal of this project is to replace (or optionally augment) that
fixed pathfinder with a small learned model so monsters feel meaningfully
smarter — flanking, retreating when hurt, coordinating with other nearby
monsters, etc. — without requiring a live LLM call on every game tick.

The pipeline is:

1. **Generate training data** — use Claude (via the Anthropic API) to
   act as each monster given a structured description of the game state.
   The headless Python driver (`driver/rogue_driver.py`) makes it
   straightforward to capture thousands of state snapshots and Claude's
   recommended moves.

2. **Train a small model** — fit a compact classifier/policy network on
   those (state, action) pairs so it can be bundled with the game and
   run locally at game speed.

3. **Hook it in** — replace (or wrap) `do-chase` / `chase` in
   `chase.lisp` with a call to the model for any monster that has a new
   `ISMODEL` flag set.

---

## State Representation

Each monster decision is represented as a fixed-size feature vector
assembled from the game state visible to that monster at the moment of
its turn.

### Local grid window
A 9×9 tile window centred on the monster (81 cells). Each cell is
one-hot encoded over the following tile classes:

| Symbol | Class |
|--------|-------|
| ` ` (space / unseen) | 0 |
| `.` floor | 1 |
| `#` passage | 2 |
| `\|` / `-` wall | 3 |
| `+` door | 4 |
| `@` player | 5 |
| letter A–Z (other monster) | 6 |
| item (`:!?*)]/=/>`) | 7 |

That gives 81 × 8 = **648 bits** for the local map.

### Monster state (scalar features)
- Monster type (26 one-hot values, A–Z)
- Current HP fraction (0–1)
- Monster level (normalised)
- Distance to player (normalised by map diagonal)
- Player visible? (bool)
- Player HP fraction (0–1, from status line — monsters should press harder
  when the player is weak)
- Number of other monsters in the same room
- Flags: ISHASTE, ISSLOW, ISBLIND, ISHUH, ISMEAN, ISGREED (6 bits)

Total input size: ≈ **700 floats** (sparse; most grid cells are 0).

### Action space
Nine discrete actions (same as the rogue movement grid):

```
7 8 9       y k u
4 5 6  →    h . l
1 2 3       b j n
```

`5` (stay) is included so the model can choose to hold position (e.g.
blocking a corridor while another monster flanks).

---

## Training Data Generation

### Collection script (`driver/collect_training_data.py`)

Uses `RogueDriver` to run many short game episodes and, at each monster
turn, asks Claude to choose the best action.

```
for each episode:
    start game, wait for dungeon to render
    for each turn:
        take snapshot
        for each running monster M:
            build prompt: ASCII map excerpt + scalar features
            ask Claude: "You are monster M at (r,c). The player is at (pr,pc).
                         Your HP is X. Pick the best move from {h,j,k,l,y,u,b,n,.}
                         and explain briefly."
            parse action letter from response
            record (feature_vector, action_index)
        send player move (random or heuristic)
    until player dies or level 3 reached
```

The prompt given to Claude describes:
- The tile type at every position in the 9×9 window (ASCII art excerpt)
- The monster's type, HP, flags
- The player's visible HP from the status line
- Other monsters in the same room

Claude is asked to return a single letter (`h`, `j`, `k`, `l`, `y`,
`u`, `b`, `n`, `.`) and a one-sentence rationale. Only the letter is
used for training; the rationale is logged for debugging.

Target corpus size: **50,000–100,000 (state, action) pairs** across many
episodes and dungeon levels.

### Quality filters
- Discard examples where Claude returns an illegal move (e.g. moving into
  a wall). These indicate ambiguous prompts and add noise.
- Stratify by monster type so rare monsters (P Dragon, U Unicorn) are not
  underrepresented.
- Include episodes from dungeon levels 1–10 so the model sees a range of
  difficulty.

---

## Model Architecture

### Option A — Gradient-boosted trees (XGBoost / LightGBM)
- Pros: fast to train (minutes), tiny serialised size (~1 MB), no
  GPU needed, easy to call from Python.
- Cons: no spatial inductive bias; the 9×9 grid is flattened.
- Good default choice for a first working version.

### Option B — Small CNN + MLP
```
Input: 9×9×8 one-hot grid  →  Conv2d(8→16, 3×3) → ReLU → Conv2d(16→32, 3×3)
       → flatten → concat scalar features → FC(256) → FC(9, softmax)
```
- Pros: spatial locality is explicitly modelled; likely generalises
  better across monster types.
- Cons: needs PyTorch (≈200 MB dep), slightly more complex training loop.

### Option C — Distilled decision tree
Train Option A/B first, then distil into a depth-12 decision tree for
zero-dependency inference (pure Python or even translatable to Lisp).

**Recommended starting point:** Option A (XGBoost). Switch to Option B if
accuracy plateaus below ~70 % top-1.

---

## Integration into cl-rogue

### New monster flag: `ISMODEL`
Add `(defconstant ISMODEL #o0200000)` to `rogue.lisp`. Set it on some or
all monsters at spawn time (configurable, defaulting to the "harder"
monster types from level 5 onward).

### Inference sidecar process
Rather than embedding Python inside SBCL, run a small Flask/FastAPI
server (`driver/model_server.py`) that loads the model once and serves
requests over a Unix socket or localhost TCP port.

On each monster turn where `ISMODEL` is set, cl-rogue calls a new Lisp
function `model-move` that:
1. Serialises the feature vector as a compact string or JSON blob.
2. Sends it to the model server via a socket write.
3. Reads back the action index (single byte).
4. Maps the action to a `coord` delta and passes it to the existing move
   machinery (bypassing `chase`).

The call is fully synchronous and should complete in < 1 ms over a local
socket.

### Fallback
If the model server is not running, `model-move` silently falls back to
the original `do-chase` logic. This keeps the game playable without the
AI component.

### New source files
| File | Purpose |
|------|---------|
| `driver/collect_training_data.py` | Episode runner + Claude labelling |
| `driver/train_model.py` | Feature engineering + XGBoost/PyTorch training |
| `driver/model_server.py` | Inference server (Unix socket) |
| `driver/requirements-ai.txt` | Extra deps (anthropic, xgboost/torch) |
| `model-move.lisp` | Lisp client for the inference server |

`model-move.lisp` is added to `cl-rogue.asd` (after `chase.lisp`).

---

## Phased Implementation Plan

### Phase 1 — Data collection scaffold
- [ ] Write `collect_training_data.py`: drives multiple episodes, captures
      (snapshot, monster positions, player position) tuples, calls Claude
      API, writes JSONL to `training_data/`.
- [ ] Write a validation script that spot-checks 100 examples and reports
      how often Claude chose a legal move.
- [ ] Target: 10,000 examples to prove out the pipeline before scaling.

### Phase 2 — Feature engineering & baseline model
- [ ] Write `featurise.py`: reads JSONL, builds the 700-float vectors,
      splits train/val/test, saves as numpy `.npz`.
- [ ] Train XGBoost baseline; report top-1 accuracy on held-out test set.
- [ ] Confusion matrix by monster type to find which monsters Claude (and
      therefore the model) handles least consistently.
- [ ] Target: > 65 % top-1 accuracy. (Random baseline = 11 %.)

### Phase 3 — Inference server
- [ ] Write `model_server.py`: loads saved model, listens on
      `/tmp/cl-rogue-model.sock`, decodes feature vectors, returns action byte.
- [ ] Benchmark round-trip latency; must be < 5 ms p99.

### Phase 4 — Lisp integration
- [ ] Add `ISMODEL` flag and `model-move.lisp`.
- [ ] Wire into `do-chase`: if `(on th ISMODEL)`, call `model-move`
      instead of `chase`. Use fallback if socket unavailable.
- [ ] Add a wizard-mode command (`^M`?) to toggle model AI on/off at runtime.
- [ ] Playtest on dungeon levels 1–5 against a human player.

### Phase 5 — Iteration
- [ ] Scale training data to 100 k examples.
- [ ] Experiment with CNN (Option B) if XGBoost accuracy is insufficient.
- [ ] Tune prompts: give Claude more context (player inventory, status
      effects) and see whether action quality improves.
- [ ] Consider multi-monster coordination: include positions of all
      monsters in the same room as extra features.

---

## Open Questions

1. **How smart should the model be?** Claude will naturally suggest
   "optimal" play. If every monster plays optimally the game becomes
   unwinnable. Options: sample from the softmax (temperature > 1) to
   add variance; only apply the model to higher-level monster types;
   reduce model quality deliberately via early stopping.

2. **Full map vs local window?** Start with the 9×9 local window. It is
   cheaper in tokens, avoids confusing Claude with irrelevant distant
   detail, and is sufficient for the tactical decisions that matter most
   (step toward player, avoid walls, don't walk into another monster).
   Long-range coordination (funnelling the player toward other monsters)
   can be added later as extra scalar features (e.g. direction and
   distance to the nearest ally) rather than by expanding the grid.

3. **Player modelling?** If the model is trained only on monster state,
   it cannot learn to exploit the player's equipment or status effects
   (e.g. a sleeping player). Adding `player_has_bow`, `player_hp_fraction`,
   `player_is_confused` as scalar features is low-cost and likely helpful.

4. **Serialisation format for the Lisp↔Python socket?** A flat array of
   IEEE-754 floats (little-endian, fixed size) is simplest. Alternatively
   a length-prefixed JSON blob trades ~5× size for debuggability.

5. **Licensing of training data.** The (state, action) pairs are derived
   from the game's RNG output (open-source BSD-licensed code) plus Claude's
   labels. Anthropic's usage policy permits training on Claude outputs for
   non-competitive use — confirm this is still true before publishing.
