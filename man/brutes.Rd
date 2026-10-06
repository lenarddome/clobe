\name{brutes}
\alias{brutes}
\title{brutes}
\usage{
    brutes(dependent, length, thresholds, ordering = "semiorder",
           return_permutations = FALSE, threads = -1L)
    }

\description{
    A brute-force approach to count the number of possible inequality matrices
    in the universal set for a given experiment.
    }

\arguments{
    \item{dependent}{Numeric vector. All the possible values the dependent variable can take.}
    \item{length}{Integer. The number of independent variables (stimuli).}
    \item{thresholds}{A numeric vector with 2 elements, \code{c(upper, lower)},
        specifying the upper and lower bounds of the differences that are
        treated as approximately equal. See \code{\link{imac}}.}
    \item{ordering}{Either \code{"semiorder"} (default) or \code{"weak"}. See
        \code{\link{imac}}.}
    \item{return_permutations}{Logical. Whether to return every assignment of
        \code{dependent} values to the stimuli. Default is \code{FALSE}, as
        there are \code{length(dependent)^length} of them.}
    \item{threads}{Integer. The number of threads to use. The default,
        \code{-1L}, uses the number set by
        \code{RcppParallel::setThreadOptions()}, or else all available cores.}
}

\details{

The function implements a brute-forced approach to calculate number of elements
in the universal set - the number of possible discretized results observable in
a given experiment (Dome and Wills, 2023). It applies \code{\link{imac}} to
every assignment of \code{dependent} values to the \code{length} stimuli and
keeps the unique inequality matrices.

With \code{ordering = "weak"} and at least \code{length} distinct values in
\code{dependent}, the result is the Fubini number (ordered Bell number) of
\code{length}. With \code{ordering = "semiorder"}, the result is at most the
number of labelled semiorders on \code{length} elements (1, 3, 19, 183, 2371,
...), and reaches it only if \code{dependent} is fine-grained enough relative
to the thresholds to realise every semiorder.

The number of assignments grows as \code{length(dependent)^length}, so the
running time does too. The assignments are split between \code{threads}
threads, and the computation can be interrupted. The result does not depend on
the number of threads.

}
\value{

Returns the following list of elements:

\item{permutations}{\code{NULL}, or if \code{return_permutations = TRUE}, a
    matrix with \code{length} columns and one row per assignment of
    \code{dependent} values to the stimuli, with the last column changing
    fastest.}
\item{inequality_matrices}{3D array. The unique inequality matrices found,
    sorted by pattern.}
\item{n}{Integer. The number of unique inequality matrices found.}

}

\author{
    Lenard Dome
}

\references{

  Dome, L., & Wills, A. (2023, April 11). g-distance: On the comparison of model and human heterogeneity. https://doi.org/10.31234/osf.io/ygmcj

}

\seealso{
  \code{\link{imac}}, \code{\link{gdistance}}
}

\examples{
## size of the universal set for 4 stimuli measured on a 0.1 grid
brutes(seq(0, 1, by = 0.1), length = 4, thresholds = c(0.1, -0.1))$n

## treating only ties as equal gives the Fubini number instead
brutes(seq(0, 1, by = 0.1), length = 4, thresholds = c(0.1, -0.1),
       ordering = "weak")$n
}
