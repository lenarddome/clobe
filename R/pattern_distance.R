pattern_distance <- function(x, y = x, method = c("kemeny", "hamming")) {
  method <- match.arg(method)
  x <- .as_patterns(x, "x")
  y <- .as_patterns(y, "y")
  if (dim(x)[1] != dim(y)[1]) {
    stop("'x' and 'y' must contain matrices of the same dimensions.",
         call. = FALSE)
  }
  .pattern_distance(x, y, method)
}
