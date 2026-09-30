#!/usr/bin/env Rscript
# check_submission.R — Check a model's submission files against the CTF rules
#
# Usage:
#   Rscript scripts/check_submission.R models_R/markowitz-ml            # full check
#   Rscript scripts/check_submission.R models_R/markowitz-ml --static   # skip the weights CSV
#
# Checks {model}_standalone.R, the model's renv.lock and data/processed/{model}.csv
# (a required submission file). Exits with an error if any check fails.
# See docs/ctf_rules.md for the rules referenced below.

suppressMessages({
  library(data.table)
  library(arrow)
})
source("scripts/submission_utils.R")

args <- commandArgs(trailingOnly = TRUE)
static_only <- "--static" %in% args
args <- setdiff(args, "--static")
if (length(args) != 1) stop("Usage: Rscript scripts/check_submission.R <model_dir> [--static]")
model_dir <- args[1]
script <- list.files(model_dir, pattern = "_standalone\\.R$", full.names = TRUE)
if (length(script) != 1) stop(sprintf("Expected one *_standalone.R in %s; run scripts/build_model.R first", model_dir))
model <- sub("_standalone\\.R$", "", basename(script))
lock_file <- file.path(model_dir, "renv.lock")
csv_file <- file.path("data", "processed", paste0(model, ".csv"))

failures <- character(0)
check <- function(ok, label, detail = "") {
  cat(sprintf("%s  %s%s\n", if (ok) "PASS" else "FAIL", label, if (!ok && nzchar(detail)) paste0(": ", detail) else ""))
  if (!ok) failures <<- c(failures, label)
}

cat(sprintf("Checking %s\n\n", script))

# Rule 13: source file constraints
check(file.size(script) < 1e6, "Rule 13: model script under 1 MB", sprintf("%.2f MB", file.size(script) / 1e6))
raw <- readBin(script, "raw", file.size(script))
check(!any(raw == as.raw(0)), "Rule 13: no binary (NUL) bytes")
lines <- readLines(script, warn = FALSE, encoding = "UTF-8")
check(all(validUTF8(lines)), "Rule 13: valid UTF-8")

# Rule 11: entrypoint
check(any(grepl("^main\\s*<-\\s*function\\(\\s*chars\\s*,\\s*features\\s*,\\s*daily_ret\\s*\\)", lines)),
      "Rule 11: defines main <- function(chars, features, daily_ret)")

# Rule 15: prohibited operations (full-line comments ignored)
code <- lines[!grepl("^\\s*#", lines)]
prohibited <- c(
  "shell execution" = "\\b(system|system2|shell)\\s*\\(",
  "dynamic code (eval/parse)" = "\\b(eval|parse|evalq)\\s*\\(",
  "string-built formula" = "as\\.formula\\s*\\(|paste0?\\s*\\([^)]*~",
  "network access" = "download\\.file\\s*\\(|\\burl\\s*\\(|\\b(httr|httr2|curl|RCurl)::",
  "source() call" = "\\bsource\\s*\\(",
  "environment modification" = "Sys\\.setenv\\s*\\("
)
for (nm in names(prohibited)) {
  hits <- grep(prohibited[[nm]], code, value = TRUE, perl = TRUE)
  check(length(hits) == 0, sprintf("Rule 15: no %s", nm), paste(trimws(head(hits, 3)), collapse = " | "))
}

# Rules 4, 8, 16: dependencies
pkgs <- model_packages(lines)
check(length(intersect(pkgs, CTF_FORBIDDEN_PKGS)) == 0, "Rules 10/16: no tidyverse or network packages loaded",
      paste(intersect(pkgs, CTF_FORBIDDEN_PKGS), collapse = ", "))
check(file.exists(lock_file), "Rule 4: model renv.lock exists", lock_file)
if (file.exists(lock_file)) {
  locked <- jsonlite::fromJSON(lock_file)$Packages
  missing <- setdiff(pkgs, c(names(locked), CTF_PREINSTALLED))
  check(length(missing) == 0, "Rule 4: every loaded package is locked or pre-installed", paste(missing, collapse = ", "))
  check(length(intersect(names(locked), CTF_FORBIDDEN_PKGS)) == 0, "Rule 16: no tidyverse or network packages in renv.lock",
        paste(intersect(names(locked), CTF_FORBIDDEN_PKGS), collapse = ", "))
  not_cran <- names(locked)[sapply(locked, function(p) !identical(p$Source, "Repository"))]
  check(length(not_cran) == 0, "Rule 4: all locked packages come from a CRAN repository", paste(not_cran, collapse = ", "))
  # R requirement of each locked version, from the Depends field recorded in the lock file
  r_req <- sapply(locked, function(p) {
    dep <- paste(unlist(p$Depends), collapse = ", ")
    m <- regmatches(dep, regexpr("R \\(>= *[0-9.]+\\)", dep))
    if (length(m) == 1) sub("R \\(>= *([0-9.]+)\\)", "\\1", m) else "0.0"
  })
  too_new <- names(r_req)[package_version(r_req) > CTF_R_VERSION]
  check(length(too_new) == 0, sprintf("Rule 8: no locked package requires R newer than %s", CTF_R_VERSION),
        paste(too_new, collapse = ", "))
  check(file.size(lock_file) < 1e6, "Rule 13: renv.lock under 1 MB")
}

# Rules 5, 12: output weights (a required submission file)
if (static_only) {
  cat(sprintf("SKIP  output checks (--static): %s not checked\n", csv_file))
} else if (!file.exists(csv_file)) {
  check(FALSE, "Rule 5: weights CSV exists", sprintf("%s not found (run the model, or use --static)", csv_file))
} else {
  out <- fread(csv_file)
  check(identical(names(out), c("id", "eom", "w")), "Rule 12: columns are exactly id, eom, w", paste(names(out), collapse = ", "))
  check(nrow(out) > 0, "Rule 12: output is non-empty")
  check(!anyNA(out), "Rule 12: no missing values")
  check(is.numeric(out$id) && all(out$id == round(out$id)), "Rule 12: id is integer")
  check(!anyNA(as.Date(as.character(out$eom), format = "%Y-%m-%d")), "Rule 12: eom is a YYYY-MM-DD date")
  check(is.numeric(out$w), "Rule 12: w is numeric")
  check(file.size(csv_file) < 50e6, "Rule 12: output under 50 MB", sprintf("%.1f MB", file.size(csv_file) / 1e6))
  out[, eom := as.Date(eom)]
  check(!any(duplicated(out[, .(id, eom)])), "Rule 12: no duplicated (id, eom)")
  test <- as.data.table(read_parquet("data/raw/ctff_chars.parquet", col_select = c("id", "eom", "ctff_test")))[ctff_test == TRUE, .(id, eom)]
  n_miss <- nrow(test[!out, on = .(id, eom)])
  check(n_miss == 0, "Rule 5: weights for every ctff_test observation", sprintf("%d missing", n_miss))
}

cat("\n")
if (length(failures) > 0) stop(sprintf("%d check(s) failed for %s", length(failures), model))
cat(sprintf("All checks passed for %s\n", model))
