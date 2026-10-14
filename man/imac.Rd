\name{imac}
\alias{imac}

\title{

    Inequality Matrix Constructor

}

\description{

    Implements the method specified by Dome and Wills (2025) used
    to create an inequality matrix describing ordinal relationships between
    all the possible combination of stimuli pairs.

}

\usage{

imac(probabilities, thresholds, ordering = "semiorder")

}

\arguments{

  \item{probabilities}{A numeric vector of dependent measures.}
  \item{thresholds}{A numeric vector with 2 elements, \code{c(upper, lower)},
      specifying the upper and lower bounds of the differences that are
      treated as approximately equal. \code{upper} must not be smaller than
      \code{lower}.}
  \item{ordering}{Either \code{"semiorder"} (default) or \code{"weak"}. See
      Details.}

}

\details{

This function implements a general method for constructing inequality matrices,
such as the ones used for discretization of results in Dome and Wills (2025).
The generic algorithm constructs a strict upper triangular matrix,
\eqn{M = (m_{ij})}, for \eqn{n} stimuli,

\deqn{M =
\left[ {\begin{array}{cccc}
  * & m_{12} & \cdots & m_{1n}\\
   * & * & \cdots & m_{2n}\\
  \vdots & \vdots & \ddots & \vdots\\
  * & * & * & * \\
\end{array} } \right].}

With \code{ordering = "semiorder"}, each cell \eqn{i < j} takes the value

\deqn{m_{ij} =
\begin{cases}
  1, & \text{if } v_i - v_j > T_{upper} \\
  -1, & \text{if } v_i - v_j < T_{lower} \\
  0, & \text{otherwise}
\end{cases}}

where \eqn{v} is \code{probabilities}. Measurements whose difference is within
the thresholds, including on a threshold, are approximately equal. Because
approximate equality is not transitive (\eqn{a \approx b} and
\eqn{b \approx c} do not imply \eqn{a \approx c}), the matrices describe
semiorders. This reflects that measurements are made with a degree of
uncertainty.

With \code{ordering = "weak"}, the thresholds are ignored and only ties
(\eqn{v_i = v_j}) are approximately equal. Approximate equality is then
transitive, and the matrices describe weak orderings (rankings with ties).

Differences are compared with an absolute tolerance of
\code{sqrt(.Machine$double.eps)}, so that rounding errors, such as
\code{0.3 - 0.2} being slightly smaller than \code{0.1}, do not change the
classification.

}

\value{

    Returns a strict upper triangular matrix, \eqn{M}, with all ordinal
    relationships. It is always rows that are compared to columns. Each cell
    above the diagonal (\eqn{i < j}) takes one of the following values:
    0 (approximate equality); 1 (larger); -1 (smaller). Each cell on or below
    the diagonal (\eqn{i \geq j}) takes the value 2.

}


\author{
    Lenard Dome
}

\references{

  Dome, L., & Wills, A. J. (2025). g-Distance: On the comparison of model and human heterogeneity. Psychological Review, 132(3), 632-655. https://doi.org/10.1037/rev0000550

}

\seealso{
  \code{\link{brutes}}
}

\examples{
## construct inequality matrix
imac(probabilities = runif(8), thresholds = c(0.10, -0.10))

## neighbouring values are approximately equal in a semiorder...
imac(c(0.3, 0.2, 0.1), thresholds = c(0.10, -0.10))
## ...but not in a weak ordering
imac(c(0.3, 0.2, 0.1), thresholds = c(0.10, -0.10), ordering = "weak")
}
