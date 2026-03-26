#!/usr/bin/env bash
# train_v3.sh — Combine all data sources and train monster_model_v3.
# Run from repo root with venv active.
set -e

COMBINED=training_data/combined_v3.jsonl
SAMPLED=training_data/sampled_v3.jsonl
MODEL=model/monster_model_v3.ubj
MAX_EXAMPLES=${MAX_EXAMPLES:-200000}

echo "=== Combining datasets ==="
# v2 base (augmented expert, level 1)
wc -l training_data/augmented.jsonl | awk '{print "  v2 augmented:    "$1" records"}'

# deep expert (augmented, levels 1-5)
if [ -f training_data/deep_expert.jsonl ]; then
    wc -l training_data/deep_expert.jsonl | awk '{print "  deep expert:     "$1" records"}'
else
    echo "  deep_expert.jsonl missing — skipping"
fi

# deep LLM (augmented + LLM labels, levels 1-5)
if [ -f training_data/deep_llm.jsonl ]; then
    wc -l training_data/deep_llm.jsonl | awk '{print "  deep LLM:        "$1" records"}'
else
    echo "  deep_llm.jsonl missing — skipping (will train without LLM variety)"
fi

cat training_data/augmented.jsonl \
    training_data/deep_expert.jsonl \
    ${LLM_DATA:-training_data/deep_llm.jsonl} \
    > "$COMBINED" 2>/dev/null || \
cat training_data/augmented.jsonl \
    training_data/deep_expert.jsonl \
    > "$COMBINED"

echo ""
wc -l "$COMBINED" | awk '{print "  Total combined:  "$1" records"}'

echo ""
echo "=== Sampling to $MAX_EXAMPLES examples ==="
shuf -n "$MAX_EXAMPLES" "$COMBINED" > "$SAMPLED"
wc -l "$SAMPLED" | awk '{print "  Sampled:         "$1" records"}'

echo ""
echo "=== Training v3 ==="
python driver/train_model.py \
    --data   "$SAMPLED" \
    --model  "$MODEL" \
    --rounds 800

echo ""
echo "=== Done ==="
ls -lh "$MODEL" "${MODEL%.ubj}_meta.json"
