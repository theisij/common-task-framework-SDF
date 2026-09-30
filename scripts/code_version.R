# code_version.R — Write the code-version section of each model's documentation
#
# Records which public code produced a submission: the repository, a git tag and
# its commit, and the SHA-256 of the submitted standalone script at that tag.
# Writes documentation/{model}/code_version.md, included by the model's .qmd.
#
# Usage: Rscript scripts/code_version.R <tag> [model ...]
#   e.g. Rscript scripts/code_version.R ctf-submission-2026-09-30
# The tag must exist on GitHub (git push origin <tag>) before the docs are published.

repo_url <- "https://github.com/theisij/common-task-framework-SDF"
models <- c("factor-ml" = "factor_ml", "minimum-variance" = "minimum_variance",
            "markowitz-ml" = "markowitz_ml")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript scripts/code_version.R <tag> [model ...]")
tag <- args[1]
if (length(args) > 1) models <- models[args[-1]]

git <- function(...) system2("git", c(...), stdout = TRUE)
commit <- git("rev-parse", paste0(tag, "^{commit}"))
commit_date <- git("show", "-s", "--format=%cs", commit)

for (m in names(models)) {
  script <- sprintf("models_R/%s/%s_standalone.R", m, models[[m]])
  # Checksum of the script as stored at the tag (not the working tree)
  tmp <- tempfile(fileext = ".R")
  writeLines(git("show", paste0(tag, ":", script)), tmp)
  sha <- digest::digest(file = tmp, algo = "sha256")
  stopifnot(identical(sha, digest::digest(file = script, algo = "sha256")))  # working tree must match the tag
  lines <- c(
    sprintf("The code is public at <%s>. The submitted weights were produced by the version tagged `%s` (commit `%s`, %s): <%s/tree/%s>.",
            repo_url, tag, substr(commit, 1, 7), commit_date, repo_url, tag),
    "",
    sprintf("- **Model script** (the submitted file): `%s`", script),
    sprintf("- **Dependencies:** `models_R/%s/renv.lock`, for the CTF runtime (R 4.4.2)", m),
    sprintf("- **Commit:** `%s`", commit),
    "- **SHA-256 of the model script:**",
    "",
    "```",
    sha,
    "```",
    "",
    sprintf("To reproduce, check out the tag, place the CTF data in `data/raw/`, run `Rscript scripts/build_model.R models_R/%s/%s.R`, and call `main()` in the resulting standalone script (or submit `models_R/%s/%s.slurm` on a SLURM cluster).",
            m, models[[m]], m, models[[m]])
  )
  writeLines(lines, file.path("documentation", m, "code_version.md"))
  cat(sprintf("%-16s %s  %s\n", m, sha, script))
}
