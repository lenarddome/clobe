\name{gdistance}
\alias{gdistance}
\title{g-distance}
\description{
  This function implements g-distance as specified in Dome and Wills (TBA)
  suitable for lists of inequality matrices.
}
\usage{ 
  gdistance(human, model, universal, weight = 0.5, xtdo = TRUE)
}
\arguments{
  \item{human}{A list of observed inequality matrices.}
  \item{model}{A list of model-produced inequality matrices.}
  \item{universal}{A double. The number of theoreticall possible patterns.}
  \item{weight}{A double. Specifies how much accomodation and predictions are
    weighted in calculating g-distance. Default is \code{0.5}.}
  \item{xtdo}{Boolean. Whether to return alpha and beta. Default is FALSE.}
}
\value{
  Returns g-distance, the distance between the current model and perfect
  accomodation and specificity (PAS). If \code{xtdo = TRUE}, returns
  a named numeric vector with g-distance, alpha and beta.
}
\author{
    Lenard Dome
}

\references{
}
