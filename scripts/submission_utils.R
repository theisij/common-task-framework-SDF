# submission_utils.R — Shared helpers for building and checking CTF submissions
# Used by: scripts/build_model.R, scripts/check_submission.R
# See docs/ctf_rules.md (Rules 4, 8, 10, 15, 16)

# Pre-installed in the CTF R 4.4.2 runtime (Rule 8)
CTF_PREINSTALLED <- c("arrow", "data.table", "dplyr", "tidyr")
CTF_R_VERSION <- "4.4.2"

# Must never be loaded or locked: meta-packages that pull in system libraries
# missing from the container, and packages for network access (Rules 10, 15, 16)
CTF_FORBIDDEN_PKGS <- c("tidyverse", "httr", "httr2", "curl", "RCurl", "rvest", "xml2",
                        "googledrive", "googlesheets4", "gargle", "ragg", "downloader")

#' Packages a model script loads: library()/require() calls and pkg:: references,
#' excluding base R packages (full-line comments are ignored)
model_packages <- function(lines) {
  code <- lines[!grepl("^\\s*#", lines)]
  lib <- unlist(regmatches(code, gregexpr("(library|require|requireNamespace)\\(\\s*['\"]?[A-Za-z][A-Za-z0-9.]*", code)))
  lib <- sub("^.*\\(\\s*['\"]?", "", lib)
  ns <- unlist(regmatches(code, gregexpr("[A-Za-z][A-Za-z0-9.]*(?=:::?)", code, perl = TRUE)))
  base <- rownames(installed.packages(priority = "base"))
  sort(setdiff(unique(c(lib, ns)), base))
}
