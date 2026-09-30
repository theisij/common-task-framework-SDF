# output_utils.R — Enforce the CTF output contract at the end of main()
# Used by: factor_ml, minimum_variance, markowitz_ml
# See docs/ctf_rules.md (Rule 12: output format; Rule 17: logging)

#' Validate, type and log the portfolio weights returned by main()
#'
#' Returns exactly the columns id (integer), eom (Date) and w (double), and stops
#' if the output is empty, has missing values, or repeats an (id, eom) pair.
#'
#' @param weights    data.table with at least id, eom, w
#' @param start_time Sys.time() recorded at the start of main(), for the runtime log
#' @return data.table with columns id, eom, w
finalize_output <- function(weights, start_time) {
  out <- as.data.table(weights)[, .(id = as.integer(id), eom = as.Date(eom), w = as.double(w))]
  if (nrow(out) == 0L) stop("main() output is empty")
  n_na <- out[, sum(is.na(id)) + sum(is.na(eom)) + sum(is.na(w))]
  if (n_na > 0) stop(sprintf("main() output has %d missing values", n_na))
  n_dup <- sum(duplicated(out[, .(id, eom)]))
  if (n_dup > 0) stop(sprintf("main() output has %d duplicated (id, eom) pairs", n_dup))
  cat(sprintf("Output: %s rows, %d months (%s to %s), %s nonzero weights; runtime %.1f minutes\n",
              format(nrow(out), big.mark = ","), uniqueN(out$eom), min(out$eom), max(out$eom),
              format(sum(out$w != 0), big.mark = ","),
              as.numeric(difftime(Sys.time(), start_time, units = "mins"))))
  out
}
