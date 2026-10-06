thresholds <- c(0.1, -0.1)
a <- imac(c(0.9, 0.5, 0.1), thresholds)  # 1 > 2 > 3
b <- imac(c(0.9, 0.9, 0.1), thresholds)  # 1 = 2 > 3
c <- imac(c(0.1, 0.5, 0.9), thresholds)  # 3 > 2 > 1

test_that("pattern_distance compares two matrices", {
    expect_equal(pattern_distance(a, a), 0)
    # one pair changes from strict to tied
    expect_equal(pattern_distance(a, b), 1)
    expect_equal(pattern_distance(a, b, method = "hamming"), 1)
    # every pair is reversed
    expect_equal(pattern_distance(a, c), 6)
    expect_equal(pattern_distance(a, c, method = "hamming"), 3)
})

test_that("pattern_distance ignores the diagonal and lower triangle", {
    d <- a
    d[lower.tri(d, diag = TRUE)] <- 0
    expect_equal(pattern_distance(a, d), 0)
})

test_that("pattern_distance compares every pair of slices", {
    x <- simplify2array(list(a, b, c))
    out <- pattern_distance(x)
    expect_equal(out, matrix(c(0, 1, 6,
                               1, 0, 5,
                               6, 5, 0), 3))
    out <- pattern_distance(x, x[, , 2:3, drop = FALSE], method = "hamming")
    expect_equal(out, matrix(c(1, 0, 3,
                               3, 3, 0), 3))
    expect_equal(pattern_distance(a, x), matrix(c(0, 1, 6), 1))
})

test_that("pattern_distance rejects invalid input", {
    expect_error(pattern_distance(a, imac(1:4, thresholds)), "dimensions")
    expect_error(pattern_distance(a, b, method = "euclid"))
    expect_error(pattern_distance(1:3, a), "square")
})
