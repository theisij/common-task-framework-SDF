#!/usr/bin/env bash
# copilot_review.sh — Wait for Copilot's review of a pull request and print it
#
# Usage: scripts/copilot_review.sh <pr-number> [timeout-minutes]   (default timeout: 15)
#
# Copilot reviews a PR once, 1.5-4 minutes after it is opened (it does not re-review
# later pushes). This polls until that review exists, then prints the verdict and
# every inline comment with its id (needed to reply under it). Exits 2 on timeout.
# Run it in the background (e.g. Claude Code's run_in_background) to keep working meanwhile.
# See .claude/skills/pr-review-cycle/SKILL.md for the full workflow.

set -euo pipefail
pr="${1:?usage: scripts/copilot_review.sh <pr-number> [timeout-minutes]}"
timeout_min="${2:-15}"
repo="$(gh repo view --json nameWithOwner --jq .nameWithOwner)"
bot="copilot-pull-request-reviewer[bot]"

# Both endpoints are paginated: --paginate applies the jq filter to every page
deadline=$(( $(date +%s) + timeout_min * 60 ))
while :; do
  review_id=$(gh api --paginate "repos/$repo/pulls/$pr/reviews" --jq ".[] | select(.user.login == \"$bot\") | .id" | tail -n 1)
  [ -n "$review_id" ] && break
  if [ "$(date +%s)" -ge "$deadline" ]; then
    echo "No Copilot review on #$pr after $timeout_min minutes." >&2
    exit 2
  fi
  sleep 20
done

# Read the latest Copilot review directly by its id
head=$(gh api "repos/$repo/pulls/$pr" --jq '.head.sha[0:7]')
gh api "repos/$repo/pulls/$pr/reviews/$review_id" \
  --jq "\"Copilot review of #$pr (commit \(.commit_id[0:7]); PR head is $head), \(.submitted_at)\""
echo
# Verdict and findings list from the review body (HTML stripped)
gh api "repos/$repo/pulls/$pr/reviews/$review_id" --jq ".body" |
  sed -e 's/<[^>]*>//g' | grep -v '^\s*$' | awk '/^What changed in this PR/ {exit} {print}'
echo
echo "== Inline comments (reply with: gh api -X POST repos/$repo/pulls/$pr/comments/<id>/replies -f body=...)"
gh api --paginate "repos/$repo/pulls/$pr/comments" \
  --jq ".[] | select(.user.login == \"Copilot\" or .user.login == \"$bot\") | select(.in_reply_to_id == null) | \"---- id \(.id)  \(.path):\(.line // .original_line)\n\(.body)\n\""
