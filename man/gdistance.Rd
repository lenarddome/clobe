\name{gdistance}
\alias{gdistance}
\title{g-distance}
\description{
  This function implements g-distance as specified in Dome and Wills (2025)
  suitable for lists of inequality matrices.
}
\usage{ 
  gdistance(human, model, universal, weight, frequencies, xtdo = FALSE)
}
\arguments{
  \item{human}{A 3D array of observed inequality matrices.}
  \item{model}{A 3D array of model-produced inequality matrices.}
  \item{universal}{Integer. The number of theoretically possible patterns.}
  \item{weight}{A double. Specifies how much accommodation and predictions are
                weighted in calculating g-distance, between 0 and 1. \code{0.5} weights
                them equally.}
  \item{frequencies}{Vector of doubles. Showing the frequencies as probabilities
                     corresponding to the frequencies observed for the patterns
                     in \code{human}.}
  \item{xtdo}{Boolean. Whether to return alpha and beta. Default is FALSE.}
}

\details{

  The function implements g-distance for the comparison of formal computational
  models as described by Dome and Wills (2025). The general form of g-distance
  is as:

   \deqn{ g = \sqrt{ w(1 - \alpha)^2 + (1-w)\beta^2 } .}

   Two inequality matrices are the same pattern if their strict upper
   triangles are identical (see \code{\link{imac}}).

   For a more thorough overview of the method, see Dome and Wills (2025).

}

\value{
  Returns g-distance, the distance between the current model, and perfect
  accommodation and specificity (PAS point). If \code{xtdo = TRUE}, returns a
  list with g-distance and the following elements:

   \item{alpha}{Double. Accommodation score showing the proportion of accommodated
                patterns relative to all observed human patterns.}
   \item{beta}{Double. The breadth of model predictions returned as the proportion of
               model predictions and universal. See Note 1.}
   \item{accommodation}{Integer vector of the (1-based) indices of accommodated
                        patterns in \code{human}.}
 
}

\author{
    Lenard Dome
}

\references{

  Dome, L., & Wills, A. J. (2025). g-Distance: On the comparison of model and human heterogeneity. Psychological Review, 132(3), 632-655. https://doi.org/10.1037/rev0000550

}

\seealso{
  \code{\link{imac}}, \code{\link{brutes}}
}

\examples{
thresholds <- c(0.1, -0.1)
## two human patterns, observed with frequencies 0.75 and 0.25
human <- simplify2array(list(imac(c(0.9, 0.5, 0.1), thresholds),
                             imac(c(0.1, 0.5, 0.9), thresholds)))
## the model produces the first human pattern and one novel pattern
model <- simplify2array(list(imac(c(0.9, 0.5, 0.1), thresholds),
                             imac(c(0.5, 0.5, 0.1), thresholds)))
## size of the universal set for 3 stimuli
universal <- brutes(seq(0, 1, by = 0.1), 3, thresholds)$n
gdistance(human, model, universal = universal, weight = 0.5,
          frequencies = c(0.75, 0.25), xtdo = TRUE)
}

\note{
  1. If the number of human patterns is equal to the number of possible patterns in the universal set, \code{gdistance} throws a warning and returns \code{beta} == NA. 
}