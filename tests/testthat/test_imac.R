load('data/test_imac.RData')

set.seed(1)
compared = imac(probabilities = runif(15), thresholds = c(0.10, -0.10))

test_that('Inequality matrix constructor passed', {
    expect_equal(stored, compared)
    }
)

test_that('differences equal to a threshold are classified consistently', {
    # every adjacent step is 0.1, but 0.3 - 0.2 is 0.0999... in floating point
    out <- imac(probabilities = c(0.4, 0.3, 0.2, 0.1), thresholds = c(0.10, -0.10))
    expect_equal(out[upper.tri(out)], rep(1, 6))
    out <- imac(probabilities = c(0.1, 0.2, 0.3, 0.4), thresholds = c(0.10, -0.10))
    expect_equal(out[upper.tri(out)], rep(-1, 6))
})

test_that('dependent measures with a range of 2 or more are classified', {
    out <- imac(probabilities = c(5, 1, 3), thresholds = c(0.10, -0.10))
    expected <- matrix(c(2, 2, 2,
                         1, 2, 2,
                         1, -1, 2), nrow = 3)
    expect_equal(out, expected)
})

test_that('approximate equality is classified as 0', {
    out <- imac(probabilities = c(0.50, 0.55), thresholds = c(0.10, -0.10))
    expect_equal(out[1, 2], 0)
})

test_that('invalid thresholds are rejected', {
    expect_error(imac(c(0.1, 0.2), thresholds = 0.1))
    expect_error(imac(c(0.1, 0.2), thresholds = c(-0.1, 0.1)))
})
