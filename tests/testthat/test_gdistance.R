library(plyr)
library(abind)
library(testthat)
load('data/test_gdistance.RData')

# simple g-distance

normal <- gdistance(human = predicted, model = discovered,
          universal = 30, weight = 0.5, frequencies = matrix(rep(1, 6)/sum(1:6)),
          xtdo = TRUE)

test_that("gdistance calculation is correct for a simple example", {
    expect_equal(normal, regular)
})

# weighted accomodation

weights <- c(2, 31, 5, 32, 31, 9)
weighted <- gdistance(human = predicted, model = discovered,
          universal = 20, weight = 0.5, frequencies = matrix(weights/sum(weights)),
          xtdo = TRUE)

test_that("gdistance calculation is correct for a simple example", {
    expect_equal(weighted, weighting)
})

# weighted g-distance

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

test_that("gdistance warning checks", {
    expect_warning({
        normal <- gdistance(human = predicted, model = predicted,
          universal = 6, weight = 0.5, frequencies = matrix(rep(1, 6)/sum(1:6)),
          xtdo = TRUE)
          })
})
