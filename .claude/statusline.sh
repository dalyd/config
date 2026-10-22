#!/bin/bash
input=$(cat)

MODEL=$(echo "$input" | jq -r '.model.display_name // "?"')
CTX=$(echo "$input" | jq -r '.context_window.used_percentage // 0' | cut -d. -f1)
FIVE_H=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
WEEK=$(echo "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty')
COST=$(echo "$input" | jq -r '.cost.total_cost_usd // empty')
DURATION_MS=$(echo "$input" | jq -r '.cost.total_duration_ms // empty')

# Git info
BRANCH=""
if git rev-parse --git-dir > /dev/null 2>&1; then
    BRANCH=$(git branch --show-current 2>/dev/null)
    STAGED=$(git diff --cached --numstat 2>/dev/null | wc -l | tr -d ' ')
    MODIFIED=$(git diff --numstat 2>/dev/null | wc -l | tr -d ' ')
    UNTRACKED=$(git ls-files --others --exclude-standard 2>/dev/null | wc -l | tr -d ' ')
fi

# Build output
OUT="[$MODEL] ctx:${CTX}%"

if [ -n "$BRANCH" ]; then
    GIT=" | $BRANCH"
    [ "$STAGED" -gt 0 ] && GIT="$GIT +$STAGED"
    [ "$MODIFIED" -gt 0 ] && GIT="$GIT ~$MODIFIED"
    [ "$UNTRACKED" -gt 0 ] && GIT="$GIT ?$UNTRACKED"
    OUT="${OUT}${GIT}"
fi

LIMITS=""
[ -n "$FIVE_H" ] && LIMITS="5h:$(printf '%.0f' "$FIVE_H")%"
[ -n "$WEEK" ] && LIMITS="${LIMITS:+$LIMITS }7d:$(printf '%.0f' "$WEEK")%"
[ -n "$LIMITS" ] && OUT="$OUT | $LIMITS"

if [ -n "$COST" ]; then
    COST_STR=$(printf '$%.2f' "$COST")
    DUR_STR=""
    if [ -n "$DURATION_MS" ]; then
        DURATION_SEC=$(( ${DURATION_MS%.*} / 1000 ))
        DUR_MIN=$(( DURATION_SEC / 60 ))
        DUR_SEC=$(( DURATION_SEC % 60 ))
        if [ "$DUR_MIN" -gt 0 ]; then
            DUR_STR=" (${DUR_MIN}m${DUR_SEC}s)"
        else
            DUR_STR=" (${DUR_SEC}s)"
        fi
    fi
    OUT="$OUT | ${COST_STR}${DUR_STR}"
fi

echo "$OUT"
