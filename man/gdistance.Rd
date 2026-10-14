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
  \item{human}{A 3D array of the distinct inequality matrices observed in
    humans, one pattern per slice. A single matrix is treated as an array with
    one slice. Each pattern should appear once; \code{\link{patterns}} turns
    raw results into this form.}
  \item{model}{A 3D array of model-produced inequality matrices, or a single
    matrix. Patterns may repeat; each counts once. In both \code{human} and
    \code{model}, cells above the diagonal can hold any finite numbers, but
    not missing or infinite values.}
  \item{universal}{Integer. The number of theoretically possible patterns. It
    cannot be smaller than the number of distinct patterns in \code{human}.}
  \item{weight}{A double between 0 and 1. Specifies how much accommodation
    and predictions are weighted in calculating g-distance. \code{0.5} weights
    them equally.}
  \item{frequencies}{Numeric vector with one value per slice of \code{human}:
    the proportion of people who showed each pattern. The values must be
    non-negative and should sum to 1.}
  \item{xtdo}{Boolean. Whether to return alpha and beta. Default is FALSE.}
}

\details{

  The function implements g-distance for the comparison of formal computational
  models as described by Dome and Wills (2025). The general form of g-distance
  is as:

   \deqn{ g = \sqrt{ w(1 - \alpha)^2 + (1-w)\beta^2 } .}

   Accommodation, \eqn{\alpha}, adds up the frequencies of the human patterns
   that the model produces:
   \deqn{\alpha = \sum_{s} f_s \, [\textrm{slice } s \textrm{ of human is
   produced by the model}].}
   It is computed slice by slice, so it is the proportion of people
   accommodated only if every pattern appears once in \code{human} and the
   frequencies sum to 1.

   Excess flexibility, \eqn{\beta}, is the number of distinct model patterns
   that no human showed, divided by the number of possible patterns that no
   human showed:
   \deqn{\beta = \frac{|M \setminus H|}{|U| - |H|},}
   where \eqn{H} and \eqn{M} are the sets of distinct human and model patterns
   and \eqn{|U|} is \code{universal}. Each pattern counts once, however often
   it occurs in \code{human} or \code{model}, and \code{frequencies} play no
   part.

   Two inequality matrices are the same pattern if their strict upper
   triangles are identical (see \code{\link{imac}}), so any coding of the
   relations works, as long as each relation always has the same code.

   For a more thorough overview of the method, see Dome and Wills (2025).

}

\value{
  Returns g-distance, the distance between the current model, and perfect
  accommodation and specificity (PAS point). If \code{xtdo = TRUE}, returns a
  list with g-distance and the following elements:

   \item{alpha}{Double. Accommodation: the summed frequencies of the human
                patterns that the model produces.}
   \item{beta}{Double. Excess flexibility: the number of unique model patterns
               not observed in humans, divided by the number of possible
               patterns not observed in humans (\code{universal} minus the
               number of unique human patterns). See Note 1.}
   \item{accommodation}{Integer vector of the (1-based) indices of accommodated
                        patterns in \code{human}.}

}

\section{Errors and warnings}{

  \code{gdistance} stops with an error if:
  \itemize{
    \item \code{human} or \code{model} is not a square matrix or a 3D array
      of square matrices, or the two have different dimensions;
    \item a cell above the diagonal is missing or infinite;
    \item \code{frequencies} does not have exactly one value per slice of
      \code{human}, or has missing or negative values;
    \item \code{universal} is smaller than the number of distinct patterns in
      \code{human};
    \item \code{weight} is not between 0 and 1.
  }

  It warns, but still returns a result, if:
  \itemize{
    \item \code{frequencies} do not sum to 1 (within
      \code{sqrt(.Machine$double.eps)}), for example because they are counts
      rather than proportions; alpha is then not a proportion;
    \item \code{human} contains the same pattern more than once; beta counts
      it once, but alpha adds the frequency of every copy, so alpha can be
      too large. Pass each pattern once with its total frequency, as
      \code{\link{patterns}} returns them;
    \item every possible pattern was observed (see Note 1).
  }

}

\author{
    Lenard Dome
}

\references{

  Dome, L., & Wills, A. J. (2025). g-Distance: On the comparison of model and human heterogeneity. Psychological Review, 132(3), 632-655. https://doi.org/10.1037/rev0000550

}

\seealso{
  \code{\link{imac}}, \code{\link{brutes}}, \code{\link{patterns}},
  \code{\link{pattern_distance}}
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

## from raw results, one row per participant: patterns() keeps each pattern
## once, with the proportion of participants who showed it
people <- rbind(c(0.9, 0.5, 0.1), c(0.8, 0.5, 0.2), c(0.1, 0.5, 0.9))
human <- patterns(people, thresholds)
gdistance(human$inequality_matrices, model, universal = universal,
          weight = 0.5, frequencies = human$frequencies, xtdo = TRUE)
}

\note{
  1. If the number of unique human patterns is equal to the number of possible patterns in the universal set, \code{gdistance} throws a warning and returns \code{beta} == NaN.
}
