\name{patterns}
\alias{patterns}
\title{Unique Ordinal Patterns in Data}

\description{
  Converts results, one row per participant or model run, into the unique
  inequality matrices they produce and how often each occurs, ready for
  \code{\link{gdistance}}.
}

\usage{
patterns(data, thresholds, ordering = "semiorder")
}

\arguments{
  \item{data}{A numeric matrix or data frame with one row per participant (or
    model run) and one column per stimulus.}
  \item{thresholds}{A numeric vector with 2 elements, \code{c(upper, lower)}.
    See \code{\link{imac}}.}
  \item{ordering}{Either \code{"semiorder"} (default) or \code{"weak"}. See
    \code{\link{imac}}.}
}

\details{
  Each row is converted with \code{\link{imac}}, and rows with the same
  strict upper triangle are the same pattern. Patterns are numbered in the
  order in which they first appear in \code{data}.
}

\value{

A list with the following elements:

\item{inequality_matrices}{3D array. The unique inequality matrices.}
\item{counts}{Integer vector. The number of rows with each pattern.}
\item{frequencies}{Numeric vector. The proportion of rows with each pattern,
  for the \code{frequencies} argument of \code{\link{gdistance}}.}
\item{pattern}{Integer vector. For each row of \code{data}, the index of its
  pattern.}
\item{n}{Integer. The number of unique patterns.}

}

\author{
  Lenard Dome
}

\references{

  Dome, L., & Wills, A. J. (2025). g-Distance: On the comparison of model and human heterogeneity. Psychological Review, 132(3), 632-655. https://doi.org/10.1037/rev0000550

}

\seealso{
  \code{\link{imac}}, \code{\link{gdistance}}
}

\examples{
thresholds <- c(0.1, -0.1)
## results of 5 participants on 3 stimuli
human <- rbind(c(0.9, 0.5, 0.1),
               c(0.1, 0.5, 0.9),
               c(0.8, 0.5, 0.2),
               c(0.5, 0.5, 0.5),
               c(0.9, 0.4, 0.0))
## results of a model at 3 parameter settings
model <- rbind(c(0.9, 0.5, 0.1),
               c(0.9, 0.9, 0.1),
               c(0.7, 0.5, 0.2))

human <- patterns(human, thresholds)
model <- patterns(model, thresholds)
human$counts

gdistance(human$inequality_matrices, model$inequality_matrices,
          universal = semiorders(3), weight = 0.5,
          frequencies = human$frequencies, xtdo = TRUE)
}
