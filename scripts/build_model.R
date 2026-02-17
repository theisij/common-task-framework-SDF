#!/usr/bin/env Rscript
# build_model.R — Inline source() calls to produce standalone submission files
#
# Usage:
#   Rscript scripts/build_model.R models_R/markowitz-ml/markowitz_ml.R
#   Rscript scripts/build_model.R models_R/factor-ml/factor_ml.R
#   Rscript scripts/build_model.R models_R/minimum-variance/minimum_variance.R
#
# Output: {model_name}_standalone.R alongside the input file

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  stop("Usage: Rscript scripts/build_model.R <model_file.R>")
}
input_file <- args[1]
if (!file.exists(input_file)) {
  stop(sprintf("File not found: %s", input_file))
}

# Track which files have been inlined to avoid duplicates
inlined_files <- character(0)

inline_sources <- function(lines, base_dir = ".") {
  result <- character(0)
  for (line in lines) {
    # Match source("...") calls (single or double quotes)
    m <- regmatches(line, regexec('^\\s*source\\(["\']([^"\']+)["\'].*\\)', line))[[1]]
    if (length(m) == 2) {
      source_path <- m[2]
      # Resolve relative to project root (source calls use project-relative paths)
      full_path <- normalizePath(file.path(base_dir, source_path), mustWork = FALSE)
      if (!file.exists(full_path)) {
        warning(sprintf("Source file not found: %s (from %s)", source_path, full_path))
        result <- c(result, line)
        next
      }
      # Skip if already inlined
      canonical <- normalizePath(full_path)
      if (canonical %in% inlined_files) {
        result <- c(result, sprintf("# [build] Already inlined: %s", source_path))
        next
      }
      inlined_files <<- c(inlined_files, canonical)
      # Read and recursively inline
      source_lines <- readLines(full_path, warn = FALSE)
      result <- c(result, sprintf("# [build] Begin inlined: %s", source_path))
      result <- c(result, inline_sources(source_lines, base_dir))
      result <- c(result, sprintf("# [build] End inlined: %s", source_path))
    } else {
      result <- c(result, line)
    }
  }
  return(result)
}

deduplicate_libraries <- function(lines) {
  # Extract all library() calls
  lib_pattern <- "^\\s*library\\(([^)]+)\\)\\s*$"
  lib_lines <- grep(lib_pattern, lines, value = TRUE)
  # Get unique package names
  pkgs <- gsub(lib_pattern, "\\1", lib_lines)
  pkgs <- unique(trimws(pkgs))
  # Remove all library() lines from body
  body <- lines[!grepl(lib_pattern, lines)]
  # Build header with unique library calls
  header <- paste0("library(", pkgs, ")")
  c(header, "", body)
}

cat(sprintf("Building standalone file from: %s\n", input_file))

# Read input
lines <- readLines(input_file, warn = FALSE)

# Inline source() calls recursively
inlined <- inline_sources(lines)

# Deduplicate library() calls
final <- deduplicate_libraries(inlined)

# Write output alongside input file
input_dir <- dirname(input_file)
input_base <- tools::file_path_sans_ext(basename(input_file))
output_file <- file.path(input_dir, paste0(input_base, "_standalone.R"))
writeLines(final, output_file)

cat(sprintf("Standalone file written to: %s\n", output_file))

# Verify no source() calls remain
remaining <- grep('^\\s*source\\(', final, value = TRUE)
# Filter out source() inside if(FALSE) blocks — those are fine
if (length(remaining) > 0) {
  warning(sprintf("WARNING: %d source() call(s) remain in output:\n  %s",
                  length(remaining), paste(remaining, collapse = "\n  ")))
} else {
  cat("Verified: no source() calls in output.\n")
}

cat("Done.\n")
