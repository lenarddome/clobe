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
  \item{x, y}{Inequality matrices, such as from \code{\link{imac}}: a square
    matrix, or a 3D array with one matrix per slice. \code{x} and \code{y}
    must have the same number of stimuli. Cells above the diagonal can hold
    any finite numbers, but not missing or infinite values.}
  \item{method}{\code{"kemeny"} (default) or \code{"hamming"}. See Details.}
}

\details{

Both distances compare the strict upper triangles, one cell per pair of
stimuli. The diagonal and lower triangle are ignored.

\code{"kemeny"} is the sum of the absolute differences between the cells,
\eqn{\sum_{i<j} |a_{ij} - b_{ij}|}. With the coding of \code{\link{imac}},
1 (larger), 0 (approximately equal) and -1 (smaller), this is the
Kemeny-Snell distance: a pair that is ordered in one pattern and
approximately equal in the other adds 1, and a pair that is reversed adds 2.
With graded codes, such as -2 (much smaller) to 2 (much larger), it adds the
number of levels between the two relations. It is only meaningful if the
codes are ordered numbers; for arbitrary labels, use \code{"hamming"}.

\code{"hamming"} counts the pairs of stimuli whose relation differs, for any
coding.

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
