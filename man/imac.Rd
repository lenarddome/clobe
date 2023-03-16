\name{imac}
\alias{imac}

\title{

    Inequality Matrix Constructor

}

\description{

    Implements the algorithm specified by Dome and Wills (n.d.), which is used
    to create an inequality matrix - strict upper triangular matrix - describing
    qualitatively different model and human behaviours (outputs).

}

\usage{

imac(probabilities = NULL, thresholds = c(0.10, -0.10))

}
\arguments{

  \item{x}{A data.table including the stimuli names and response probabilities. See in \code{details} for more details.}
  \item{stimuli}{A character vector, including the names of each stimulus that
      is used in the creation of the inequality matrix.}
  \item{thresholds}{A numeric vector with 2 elements, specifying the lower and
      upper bounds of the differences. The first element is the upper, while
      the lower element is the lower. See \code{Details} below.}
  \item{midpoint}{Whether to include a midpoint or not.}

}

\details{

    Argument \code{x} must have the following columns:

    \itemize{
        \item{stimulus}{The stimuli as a character, e.g. \code{"AC"}}
        \item{prob}{Porbability of responding with a single category.}
    }
    \code{thresholds = c(upper, lower)} should be set so they specify three
    intervals: \eqn{(\infty, upper]}, \eqn{(upper, lower)}, \eqn{(lower,  -\infty]}.

}

\value{

    Returns a \code{data.frame} with column- and rownames of the stimuli.
    The ineqaulity matrix can include 1 for a positive difference, -1 for the
    negative differnce, and 0 for equality (and inconclusive). The matrix also
    includes \eqn{\empty} as an indifference point.

}

\author{
    Lenard Dome
}

\references{
}

\examples{
## create a data.table object
library(data.table)
dummy <- data.table(stimulus = c("A", "X", "Y", "XY"), prob = c(0.1, 0.5, 0.9, 1))

## construct ineqaulity matrix
imac(x = dummy, stimuli = c("X", "Y", "XY"), thresholds = c(0.20, -0.20))
}
