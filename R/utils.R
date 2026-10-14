# Returns inequality matrices as a 3D array with one matrix per slice; a
# single matrix becomes an array with one slice.
.as_patterns <- function(patterns, name) {
  if (is.matrix(patterns)) patterns <- array(patterns, c(dim(patterns), 1))
  if (!is.array(patterns) || length(dim(patterns)) != 3 ||
      dim(patterns)[1] != dim(patterns)[2]) {
    stop(sprintf("'%s' must be a square matrix or a 3D array of square ",
                 name), "matrices.", call. = FALSE)
  }
  patterns
}
