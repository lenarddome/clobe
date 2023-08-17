\name{imac}
\alias{imac}

\title{

    Inequality Matrix Constructor

}

\description{

    Implements the method specified by Dome and Wills (2023) used
    to create an inequality matrix describing ordinal relationships between
    all the possible combination of stimuli pairs.

}

\usage{

imac(probabilities, thresholds)

}

\arguments{

  \item{probabilities}{A numeric vector of dependent measures.}
  \item{thresholds}{A numeric vector with 2 elements, specifying the lower and
      upper bounds of the differences. The first element is the upper, while
      the second element is the lower bound.}

}

\details{

This function implements a general method for constructing inequality matrices,
such as the ones used for discretization fo results in Dome and Wills (2023).
The generic algorithm that constructs those strict upper triangular matrix, \eqn{M = m_{aj}} is

\deqn{M_{a\times j} =
\left[ {\begin{array}{cccc}
  * & a_{12} & \cdots & a_{1j}\\
   * & * & \cdots & a_{2j}\\
  \vdots & \vdots & \ddots & \vdots\\
  * & * & * & * \\
\end{array} } \right].}

Each cell then takes the value output by the following function, where \eqn{a \leq j} and v is the P(rare|stimulus_{a or j}):


\deqn{m_{ij} =
\begin{cases}
  1, & \text{if } v_i - v_j \leq T \\
  -1, & \text{if } v_i - v_j \geq T \\
  0, & \text{otherwise}
\end{cases}}


}

\value{

    Returns a strict upper triangular matrix, \eqn{M_{aj}} with all ordinal relationships.
    Each cell, where \eqn{a > j}, can take on one of the following values:
    0 (approximate equality); 1 (larger); -1 (smaller). It is always rows that
    are compared to columns. Each cell, where \eqn{a \leq j}, will take on the
    values of 2.

}


\author{
    Lenard Dome
}

\references{

  Dome, L., & Wills, A. (2023, April 11). g-distance: On the comparison of model and human heterogeneity. https://doi.org/10.31234/osf.io/ygmcj

}

\examples{
## construct ineqaulity matrix
imac(probabilities = runif(8), thresholds = c(0.10, -0.10))
}
