\name{orderings}
\alias{orderings}
\title{Enumerate Ordinal Patterns}

\description{
  Lists every possible inequality matrix over a number of stimuli: every
  semiorder or every weak ordering.
}

\usage{
orderings(length, ordering = "semiorder")
}

\arguments{
  \item{length}{Integer. The number of stimuli.}
  \item{ordering}{Either \code{"semiorder"} (default) or \code{"weak"}. See
    \code{\link{imac}}.}
}

\details{

Unlike \code{\link{brutes}}, which applies \code{\link{imac}} to every
assignment of values to the stimuli, \code{orderings} constructs the patterns
directly, so its running time depends on the number of patterns rather than
on the number of values.

Weak orderings are constructed from every split of the stimuli into tied
groups and every order of the groups. Semiorders are constructed from the
Catalan number of unlabelled semiorders, each labelled in every way: with the
stimuli sorted by value, the stimuli approximately equal to a stimulus and
above it in the order are a contiguous run, whose end never moves down.

The result is the universal set when the dependent variable can be measured
finely enough to realise every pattern. It is the same as from
\code{brutes(..., ordering = ordering)} with such values, including the order
of the matrices. Its size is \code{\link{semiorders}(length)} or
\code{\link{fubini}(length)}.

The number of patterns grows quickly with \code{length}: 8 stimuli have about
17.6 million semiorders, returned in an array of about 9 GB. The function
refuses to return more than \eqn{2^{31}} cells; use the counting functions to
count larger sets.

}

\value{

A list with the following elements:

\item{inequality_matrices}{3D array. All inequality matrices, sorted by
  pattern.}
\item{n}{Integer. The number of inequality matrices.}

}

\author{
  Lenard Dome
}

\references{

  Dome, L., & Wills, A. J. (2025). g-Distance: On the comparison of model and human heterogeneity. Psychological Review, 132(3), 632-655. https://doi.org/10.1037/rev0000550

  Fishburn, P. C. (1985). \emph{Interval orders and interval graphs: A study
  of partially ordered sets}. Wiley.

}

\seealso{
  \code{\link{brutes}}, \code{\link{semiorders}}, \code{\link{fubini}}
}

\examples{
## the 19 semiorders and 13 weak orderings of 3 stimuli
orderings(3)$n
orderings(3, ordering = "weak")$n

## the same as brutes with values fine enough to realise every pattern
identical(orderings(4)$inequality_matrices,
          brutes(0:10, 4, c(1, -1))$inequality_matrices)
}
