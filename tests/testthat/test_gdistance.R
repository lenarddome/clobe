load("data/test_gdistance.RData")

# in the fixture, all 6 human patterns are accommodated by the model, and 11 of
# the 20 model slices are not observed in humans (comparing upper triangles)
novel <- 11

# simple g-distance

frequencies <- rep(1, 6) / sum(1:6)
normal <- gdistance(human = predicted, model = discovered,
          universal = 30, weight = 0.5, frequencies = frequencies,
          xtdo = TRUE)

test_that("gdistance calculation is correct for a simple example", {
    alpha <- sum(frequencies)
    beta <- novel / (30 - 6)
    expect_equal(normal$alpha, alpha)
    expect_equal(normal$beta, beta)
    expect_equal(normal$gdistance, sqrt(0.5 * (1 - alpha)^2 + 0.5 * beta^2))
    expect_equal(normal$accommodation, 1:6)
})

# weighted accommodation

weights <- c(2, 31, 5, 32, 31, 9)
weighted <- gdistance(human = predicted, model = discovered,
          universal = 20, weight = 0.5, frequencies = weights / sum(weights),
          xtdo = TRUE)

test_that("gdistance calculation is correct for weighted accommodation", {
    beta <- novel / (20 - 6)
    expect_equal(weighted$alpha, 1)
    expect_equal(weighted$beta, beta)
    expect_equal(weighted$gdistance, sqrt(0.5 * beta^2))
})

# weighted g-distance

curve <- NULL
for (i in seq(0, 1, by = .10)) {
    out <- gdistance(human = predicted, model = discovered,
                     universal = 20, weight = i, frequencies = frequencies)
    curve <- c(curve, out$gdistance)
}

test_that("gdistance calculation is correct for weighted gdistance", {
    w <- seq(0, 1, by = .10)
    alpha <- sum(frequencies)
    beta <- novel / (20 - 6)
    expect_equal(curve, sqrt(w * (1 - alpha)^2 + (1 - w) * beta^2))
})

test_that("accommodation returns 1-based indices of human patterns", {
    human <- simplify2array(list(imac(c(0.9, 0.1, 0.5), c(0.1, -0.1)),
                                 imac(c(0.1, 0.9, 0.5), c(0.1, -0.1))))
    model <- human[, , 2, drop = FALSE]
    out <- gdistance(human, model, universal = 13, weight = 0.5,
                     frequencies = c(0.5, 0.5), xtdo = TRUE)
    expect_equal(out$accommodation, 2)
    expect_equal(out$alpha, 0.5)
    expect_equal(out$beta, 0)
})

test_that("prediction ignores the diagonal and lower triangle", {
    human <- array(imac(c(0.9, 0.1, 0.5), c(0.1, -0.1)), c(3, 3, 1))
    model <- human
    model[, , 1][lower.tri(model[, , 1], diag = TRUE)] <- 0
    out <- gdistance(human, model, universal = 13, weight = 0.5,
                     frequencies = 1, xtdo = TRUE)
    expect_equal(out$alpha, 1)
    expect_equal(out$beta, 0)
})

## test warning messages

test_that("gdistance warning checks", {
    expect_warning({
        normal <- gdistance(human = predicted, model = predicted,
          universal = 6, weight = 0.5, frequencies = frequencies,
          xtdo = TRUE)
          })
})

test_that("gdistance rejects invalid input", {
    expect_error(gdistance(predicted, discovered, universal = 30, weight = 0.5,
                           frequencies = rep(1, 5)), "frequencies")
    expect_error(gdistance(predicted, discovered, universal = 5, weight = 0.5,
                           frequencies = frequencies), "universal")
    expect_error(gdistance(predicted, discovered, universal = 30, weight = 2,
                           frequencies = frequencies), "weight")
    expect_error(gdistance(predicted, discovered[1:2, 1:2, , drop = FALSE],
                           universal = 30, weight = 0.5,
                           frequencies = frequencies),
                 "dimensions")
})
