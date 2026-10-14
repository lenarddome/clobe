orderings <- function(length, ordering = "semiorder") {
  if (!is.numeric(length) || base::length(length) != 1 || is.na(length) ||
      length < 1 || length != round(length)) {
    stop("'length' must be a whole number of at least 1.", call. = FALSE)
  }
  if (!is.character(ordering) || base::length(ordering) != 1 ||
      !ordering %in% c("semiorder", "weak")) {
    stop("'ordering' must be either \"semiorder\" or \"weak\".", call. = FALSE)
  }
  n <- if (ordering == "weak") fubini(length) else semiorders(length)
  # at most 2^31 cells, or 16 GB, in the returned array
  if (n * length^2 > 2^31) {
    stop(sprintf(paste(
      "There are too many patterns to enumerate (%.0f); use %s() to count",
      "them."), n, if (ordering == "weak") "fubini" else "semiorders"),
      call. = FALSE)
  }
  .orderings(length, ordering)
}
