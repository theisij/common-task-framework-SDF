---
name: pr-review-cycle
description: Open a pull request, wait for Copilot's review, verify each comment, fix the valid ones on the same branch, reply under every comment, then squash-merge and sync. Use whenever work in this repo is ready to go to main, or when asked to handle Copilot's comments on a PR.
---

# PR review cycle

Use this for every change that goes to `main`. Copilot reviews each PR **once**, 1.5–4 minutes after it is opened, and does **not** re-review later pushes. So: open the PR, wait for that review, act on it, and only then merge. Merging before the review arrives means a follow-up PR (as happened with #18 → #19).

## 1. Prepare the branch

- **Branch:** start from an up-to-date `main`: `git fetch && git switch -c <type>/<short-name> origin/main` (types: `fix/`, `docs/`, `feat/`).
- **Model code changes:** before committing, follow the pre-submission checklist in `CLAUDE.md`: rebuild the standalone files and lock files (`scripts/build_model.R`), run the toy tests, and run `scripts/check_submission.R`. If results can change, plan full HPC reruns before merging (or before tagging a release).
- **Commit message:** say why, not just what, and end with the attribution line from the system prompt.
- **Push:** `git push -u origin <branch>`. If SSH fails with "Permission denied (publickey)", run `export SSH_AUTH_SOCK=/run/user/1000/keyring/ssh` first (see memory).

## 2. Open the PR

```bash
gh pr create --base main --head <branch> --title "<title>" --body-file - <<'EOF'
## Summary
<why the change is needed, with evidence>

## Changes
- ...

## Checks
- [x] <tests and checks that were run, with results>
- [ ] <anything still pending, e.g. HPC reruns>

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
```

`gh pr edit` and `gh pr merge` can fail with a "Projects (classic) is being deprecated" GraphQL error in gh 2.45. Use the REST API instead (below).

## 3. Wait for Copilot's review

```bash
scripts/copilot_review.sh <pr-number>     # polls up to 15 min, prints verdict and inline comments with ids
```

Run it in the background, or wait for it. If it times out, tell the user and ask whether to merge without a review.

## 4. Evaluate every comment

Never accept or reject a comment on reflex. For each one:

1. **Verify the claim** against the code or the data. Reproduce it where possible: a toy-data run, a grep, a small R snippet. Copilot is often right in principle and sometimes wrong in practice. For example, an R argument named `print` does *not* shadow `print()`, and a factor with exposures only in later months does *not* create lookahead.
2. **Classify it:** *valid* (fix it), *valid but not worth the cost now* (explain and ask the user), or *not valid* (explain with evidence).
3. **Fix valid comments on the same branch**, rerun the relevant tests, and push. Since Copilot won't re-review, our own tests are the check.

## 5. Reply under every comment

Reply in the comment's thread, so the decision is recorded next to the code:

```bash
gh api -X POST repos/theisij/common-task-framework-SDF/pulls/<pr>/comments/<comment-id>/replies \
  -f body="**Fixed** in <short-sha>: <what changed, how it was verified>."
# or: "**Not changed**: <reason, with the evidence: numbers, test output>."
```

Keep replies short and factual. If the review has no findings, no reply is needed.

## 6. Merge and sync

```bash
# mergeable_state must be "clean" (it can take a few seconds to compute)
gh api repos/theisij/common-task-framework-SDF/pulls/<pr> --jq '"\(.mergeable) \(.mergeable_state)"'
gh api -X PUT repos/theisij/common-task-framework-SDF/pulls/<pr>/merge \
  -f merge_method=squash -f commit_title="<title> (#<pr>)"
git switch main && git pull --ff-only && git fetch --prune && git branch -D <branch>
```

- **Merge method:** squash, matching the repo's history.
- **Branch cleanup:** the remote branch is deleted automatically (the repo setting is on).
- **HPC:** if the change affects code the HPC runs, sync it too: `module load git/2.48.1-incm`, then `git switch main && git pull --ff-only` in `~/common-task-framework-SDF`.
- **Release:** if the change is a submission, tag the merged commit afterwards (step 5 of the pre-submission checklist in `CLAUDE.md`).

## 7. Report to the user

Give a short table: each Copilot comment, its verdict (fixed / not changed), and the reason. Then the merge commit, and anything still pending (reruns, a release tag, the Dropbox package).

## If the PR was already merged

If the review arrives after the merge, handle valid comments in a small follow-up PR that goes through this same cycle. Reply under the original comments, pointing to the follow-up PR.
