patterns <- function(data, thresholds, ordering = "semiorder") {
  if (is.data.frame(data)) {
    if (!all(vapply(data, is.numeric, logical(1)))) {
      stop("All columns of 'data' must be numeric.", call. = FALSE)
    }
    data <- as.matrix(data)
  }
  if (!is.matrix(data)) {
    stop("'data' must be a matrix or data frame with one row per ",
         "participant and one column per stimulus.", call. = FALSE)
  }
  if (!is.numeric(data)) {
    stop("'data' must be numeric.", call. = FALSE)
  }
  if (anyNA(data)) {
    stop("'data' must not have missing values.", call. = FALSE)
  }
  matrices <- lapply(seq_len(nrow(data)), function(row) {
    imac(data[row, ], thresholds, ordering)
  })
  keys <- vapply(matrices, function(m) {
    paste(m[upper.tri(m)], collapse = ",")
  }, character(1))
  unique_keys <- unique(keys)
  pattern <- match(keys, unique_keys)
  counts <- tabulate(pattern, length(unique_keys))
  k <- ncol(data)
  list(
    inequality_matrices = array(
      unlist(matrices[match(unique_keys, keys)]), c(k, k, length(unique_keys))
    ),
    counts = counts,
    frequencies = counts / nrow(data),
    pattern = pattern,
    n = length(unique_keys)
  )
}
