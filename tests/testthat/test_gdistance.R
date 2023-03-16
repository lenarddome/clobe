library(plyr)
library(abind)
library(testthat)

discovered <- array(sample(0:1, size = 60, replace = TRUE), dim = c(3, 3, 20))
discovered <- alply(discovered, 3,
                    function(x) {y <- x; y[lower.tri(y)] <- 2; return(y)})
discovered <- simplify2array(discovered)
predicted <- array(sample(0:1, size = 12, replace = TRUE), dim = c(3, 3, 4))
predicted <- abind(list(predicted, discovered[, , 1:2]), along = 3)
dimnames(predicted) <- NULL
predicted <- alply(predicted, 3,
                   function(x) {y <- x; y[lower.tri(y)] <- 2; return(y)})
predicted <- simplify2array(predicted)

## simple g-distance
regular <- list('gdistance' = 0.731607431008,
                'alpha' = 0.238095238095,
                'beta' = 0.7)

normal <- gdistance(human = predicted, model = discovered,
          universal = 30, weight = 0.5, frequencies = matrix(rep(1, 6)/sum(1:6)),
          xtdo = TRUE)

test_that("gdistance calculation is correct for a simple example", {
    expect_equal(normal, regular)
})

# weighted accomodation
weighting <- list('gdistance' = 0.536016837036652,
                'alpha' = 0.709090909090909,
                'beta' = 0.7)

weights <- c(2, 31, 5, 32, 31, 9)
weighted <- gdistance(human = predicted, model = discovered,
          universal = 20, weight = 0.5, frequencies = matrix(weights/sum(weights)),
          xtdo = TRUE)

test_that("gdistance calculation is correct for a simple example", {
    expect_equal(weighted, weighting)
})

# weighted g-distance
curvature <- c(0.7, 0.706434630111885, 0.712811176429376, 0.719131184043597,
               0.725396130734967, 0.731607431008308, 0.737766439822178,
               0.743874456040269, 0.749932725629787, 0.755942444629111,
               0.761904761904762)

curve <- NULL
for (i in seq(0, 1, by = .10)) {
    out <- gdistance(human = predicted, model = discovered,
                     universal = 20, weight = i, frequencies = matrix(rep(1, 6)/sum(1:6)),
                     xtdo = TRUE)
    curve <- c(curve, out$gdistance)
}

test_that("gdistance calculation is correct for weighted gdistance", {
    expect_equal(curve, curvature)
})

## test warning messages

test_that("gdistance calculation is correct for a simple example", {
    expect_warning({
        normal <- gdistance(human = predicted, model = predicted,
          universal = 6, weight = 0.5, frequencies = matrix(rep(1, 6)/sum(1:6)),
          xtdo = TRUE)
          })
})
