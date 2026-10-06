pattern_distance <- function(x, y = x, method = c("kemeny", "hamming")) {
  method <- match.arg(method)
  as_cube <- function(patterns, name) {
    if (is.matrix(patterns)) patterns <- array(patterns, c(dim(patterns), 1))
    if (!is.array(patterns) || length(dim(patterns)) != 3 ||
        dim(patterns)[1] != dim(patterns)[2]) {
      stop(sprintf("'%s' must be a square matrix or a 3D array of square ",
                   name), "matrices.", call. = FALSE)
    }
    patterns
  }
  both_matrices <- is.matrix(x) && is.matrix(y)
  x <- as_cube(x, "x")
  y <- as_cube(y, "y")
  if (dim(x)[1] != dim(y)[1]) {
    stop("'x' and 'y' must contain matrices of the same dimensions.",
         call. = FALSE)
  }
  distance <- .pattern_distance(x, y, method)
  if (both_matrices) distance[1, 1] else distance
}
