\name{pattern_distance}
\alias{pattern_distance}
\title{Distance Between Ordinal Patterns}

\description{
  Measures how far apart inequality matrices are, for example to tell a model
  pattern that narrowly misses a human pattern from one that reverses it.
}

\usage{
pattern_distance(x, y = x, method = c("kemeny", "hamming"))
}

\arguments{
  \item{x, y}{Inequality matrices from \code{\link{imac}}: a square matrix, or
    a 3D array with one matrix per slice. \code{x} and \code{y} must have the
    same number of stimuli.}
  \item{method}{\code{"kemeny"} (default) or \code{"hamming"}. See Details.}
}

\details{

Both distances compare the strict upper triangles, one cell per pair of
stimuli, where each cell is 1 (larger), 0 (approximately equal) or -1
(smaller).

\code{"kemeny"} is the Kemeny-Snell distance, \eqn{\sum_{i<j} |a_{ij} -
b_{ij}|}: a pair that is ordered in one pattern and approximately equal in the
other adds 1, and a pair that is reversed adds 2.

\code{"hamming"} counts the pairs of stimuli whose relation differs.

}

\value{
  If \code{x} and \code{y} are both matrices, a single distance. Otherwise, a
  matrix with one row per slice of \code{x} and one column per slice of
  \code{y}.
}

\author{
  Lenard Dome
}

\references{

  Kemeny, J. G., & Snell, J. L. (1962). \emph{Mathematical models in the
  social sciences}. Ginn.

}

\seealso{
  \code{\link{imac}}, \code{\link{gdistance}}
}

\examples{
thresholds <- c(0.1, -0.1)
a <- imac(c(0.9, 0.5, 0.1), thresholds)
b <- imac(c(0.9, 0.9, 0.1), thresholds)  # stimuli 1 and 2 tied
c <- imac(c(0.1, 0.5, 0.9), thresholds)  # order reversed

pattern_distance(a, b)
pattern_distance(a, c)
pattern_distance(a, c, method = "hamming")

## every pair of slices
pattern_distance(simplify2array(list(a, b, c)))
}
