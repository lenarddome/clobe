\name{combinatorics}
\alias{combinatorics}
\alias{stirling2}
\alias{fubini}
\alias{semiorders}

\title{Counting Ordinal Patterns}

\description{
  Count the number of possible ordinal patterns (inequality matrices) over a
  number of stimuli, without enumerating them.
}

\usage{
stirling2(n, k)
fubini(k, levels = Inf)
semiorders(k)
}

\arguments{
  \item{n}{Non-negative whole numbers. The number of items.}
  \item{k}{Non-negative whole numbers. For \code{stirling2}, the number of
    groups; otherwise, the number of stimuli.}
  \item{levels}{Non-negative whole numbers, or \code{Inf}. The maximum
    number of tied groups, such as the number of distinct values the
    dependent variable can take.}
}

\details{

\code{stirling2(n, k)} gives the Stirling numbers of the second kind: the
number of ways to split \eqn{n} items into \eqn{k} non-empty groups,
\eqn{S(n, k)}.

\code{fubini(k)} gives the Fubini numbers (ordered Bell numbers): the number
of weak orderings, or rankings with ties, of \eqn{k} stimuli,
\deqn{a(k) = \sum_{j=0}^{k} j! S(k, j),}
where \eqn{S(k, j)} counts the ways to split the stimuli into \eqn{j} tied
groups and \eqn{j!} the ways to order the groups. With \code{levels}, the sum
stops at \eqn{j = levels}. This is the size of the universal set from
\code{brutes(..., ordering = "weak")} when \code{dependent} has \code{levels}
distinct values.

\code{semiorders(k)} gives the number of labelled semiorders on \eqn{k}
stimuli, the patterns from \code{\link{imac}} with the default
\code{ordering = "semiorder"}, in which approximate equality need not be
transitive. Its exponential generating function is \eqn{C(1 - e^{-x})}, where
\eqn{C} is the generating function of the Catalan numbers, so
\deqn{s(k) = \sum_{j=0}^{k} (-1)^{k-j} C_j j! S(k, j),}
where \eqn{C_j} is the \eqn{j}-th Catalan number. This is the largest
possible size of the universal set from \code{brutes()} with the default
ordering. \code{brutes()} reaches it only if \code{dependent} is fine-grained
enough, relative to the thresholds, to realise every semiorder; comparing
\code{brutes(...)$n} with \code{semiorders(k)} shows whether it is.

All counts are doubles, which represent whole numbers exactly up to
\eqn{2^{53}}. The functions warn if a result may not be exact: from
\code{k = 17} for \code{fubini} and from \code{k = 14} for
\code{semiorders}, whose alternating sum has larger terms than its result.

}

\value{
  A numeric vector of counts, recycled to the length of the longest
  argument.
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
  \code{\link{brutes}}
}

\examples{
## ways to split 4 stimuli into 2 tied groups
stirling2(4, 2)

## weak orderings and semiorders of 1 to 6 stimuli
fubini(1:6)
semiorders(1:6)

## at most 3 distinct values for 4 stimuli
fubini(4, levels = 3)

## is a 0.1 grid fine enough to realise every semiorder of 5 stimuli?
brutes(seq(0, 1, by = 0.1), 5, c(0.1, -0.1), return_matrices = FALSE)$n ==
  semiorders(5)
}
