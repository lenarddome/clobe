\name{brutes}
\alias{brutes}
\title{brutes}
\usage{
    brutes(dependent, length, thresholds)
    }

\description{
    A brute-force approach to count the number of possible ineqaulity matrices
    in the universal set for a given experiment.
    }

\arguments{
    \item{dependent}{Numeric vector. All the possible values the dependent variable can take.}
    \item{length}{Integer. The number of independent variables.}
    \item{thresholds}{A numeric vector with 2 elements, specifying the lower and
        upper bounds of the differences. The first element is the upper, while
        the second element is the lower bound.}
}

\details{

The function implements a brute-forced approach to calculate number of elements
in the universal set - the number of possible discretized results observable in
a given experiment (Dome and Wills, 2023).

}
\value{

Returns the following list of elements:

\item{permutations}{The possible combinations of values in the \code{dependent}. Columns equal to \code{length}.}
\item{ineqaulity_matrices}{3D array. The unique ineqaulity matrices found.}
\item{n}{Integer. The number of unique inequality matrices found.}

}

\author{
    Lenard Dome
}

\references{

  Dome, L., & Wills, A. (2023, April 11). g-distance: On the comparison of model and human heterogeneity. https://doi.org/10.31234/osf.io/ygmcj

}
