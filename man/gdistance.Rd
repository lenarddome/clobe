\name{gdistance}
\alias{gdistance}
\title{g-distance}
\description{
  This function implements g-distance as specified in Dome and Wills (2023)
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
                weighted in calculating g-distance. Default is \code{0.5}.}
  \item{frequencies}{Vector of doubles. Showing the frequencies as probabilities
                     corresponding to the frequencies observed for the patterns
                     in \code{human}.}
  \item{xtdo}{Boolean. Whether to return alpha and beta. Default is FALSE.}
}

\details{

  The function implements g-distance for the comparison of formal computational
  models as described by Dome and Wills (2023). The general form of g-distance
  is as:

   \deqn{ g = \sqrt{ w(1 - \alpha)^2 + (1-w)\beta} .}

   For a more thorough overview of the method, see Dome and Wills (2023).

}

\value{
  Returns g-distance, the distance between the current model, and perfect
  accommodation and specificity (PAS point). If \code{xtdo = TRUE}, returns a
  list with g-distance and the following elements:

   \item{alpha}{Double. Accommodation score showing the proportion of accommodated
                patterns relative to all observed human patterns.}
   \item{beta}{Double. The breadth of model predictions returned as the proportion of
               model predictions and universal. See Note 1.}
   \item{accommodation}{Vector of indices of accommodated patterns in \code{human}.}
 
}

\author{
    Lenard Dome
}

\references{

  Dome, L., & Wills, A. (2023, April 11). g-distance: On the comparison of model and human heterogeneity. https://doi.org/10.31234/osf.io/ygmcj

}

\note{
  1. If the number of human patterns is equal to the number of possible patterns in the universal set, \code{gdistance} throws a warning and returns \code{beta} == NA. 
}