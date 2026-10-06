# Counting ordinal patterns. All counts are doubles, which represent integers
# exactly up to 2^53.

.check_counts <- function(x, name, finite = TRUE) {
  invalid <- !is.numeric(x) || anyNA(x) || any(x < 0) ||
    (finite && any(is.infinite(x))) || any(is.finite(x) & x != round(x))
  if (invalid) {
    stop(sprintf("'%s' must contain non-negative whole numbers.", name),
         call. = FALSE)
  }
}

.warn_inexact <- function(x) {
  if (any(abs(x) > 2^53)) {
    warning("Some counts exceed 2^53 and are not exact.", call. = FALSE)
  }
}

# Stirling numbers of the second kind S(i, j) for 0 <= j <= i <= n, as an
# (n + 1) x (n + 1) matrix with S(i, j) in row i + 1 and column j + 1. The
# recurrence only adds non-negative numbers, so an entry is exact if it does
# not exceed 2^53.
.stirling2_table <- function(n) {
  table <- matrix(0, n + 1, n + 1)
  table[1, 1] <- 1
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      table[i + 1, j + 1] <- j * table[i, j + 1] + table[i, j]
    }
  }
  table
}

stirling2 <- function(n, k) {
  .check_counts(n, "n")
  .check_counts(k, "k")
  if (length(n) == 0 || length(k) == 0) return(numeric(0))
  size <- max(length(n), length(k))
  n <- rep_len(n, size)
  k <- rep_len(k, size)
  table <- .stirling2_table(max(n))
  out <- ifelse(k > n, 0, table[cbind(n + 1, pmin(k, n) + 1)])
  .warn_inexact(out)
  out
}

fubini <- function(k, levels = Inf) {
  .check_counts(k, "k")
  .check_counts(levels, "levels", finite = FALSE)
  if (length(k) == 0 || length(levels) == 0) return(numeric(0))
  size <- max(length(k), length(levels))
  k <- rep_len(k, size)
  levels <- rep_len(levels, size)
  table <- .stirling2_table(max(k))
  # weak orderings with j tied groups: the ways to split k items into j
  # groups, times the ways to order the groups
  out <- vapply(seq_len(size), function(i) {
    j <- seq(0, min(k[i], levels[i]))
    sum(factorial(j) * table[k[i] + 1, j + 1])
  }, numeric(1))
  .warn_inexact(out)
  out
}

semiorders <- function(k) {
  .check_counts(k, "k")
  if (length(k) == 0) return(numeric(0))
  table <- .stirling2_table(max(k))
  catalan <- function(j) choose(2 * j, j) / (j + 1)
  # the exponential generating function is C(1 - exp(-x)), where C is the
  # generating function of the Catalan numbers, so the count is an
  # alternating sum; it is exact only if every term is
  terms <- lapply(k, function(k) {
    j <- seq(0, k)
    (-1)^(k - j) * catalan(j) * factorial(j) * table[k + 1, j + 1]
  })
  .warn_inexact(unlist(terms))
  vapply(terms, sum, numeric(1))
}
