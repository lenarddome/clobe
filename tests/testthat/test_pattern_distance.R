thresholds <- c(0.1, -0.1)
a <- imac(c(0.9, 0.5, 0.1), thresholds)  # 1 > 2 > 3
b <- imac(c(0.9, 0.9, 0.1), thresholds)  # 1 = 2 > 3
c <- imac(c(0.1, 0.5, 0.9), thresholds)  # 3 > 2 > 1

test_that("pattern_distance compares two matrices as one-slice arrays", {
    expect_equal(pattern_distance(a, a), matrix(0, 1, 1))
    # one pair changes from strict to tied
    expect_equal(pattern_distance(a, b), matrix(1, 1, 1))
    expect_equal(pattern_distance(a, b, method = "hamming"), matrix(1, 1, 1))
    # every pair is reversed
    expect_equal(pattern_distance(a, c), matrix(6, 1, 1))
    expect_equal(pattern_distance(a, c, method = "hamming"), matrix(3, 1, 1))
})

test_that("pattern_distance ignores the diagonal and lower triangle", {
    d <- a
    d[lower.tri(d, diag = TRUE)] <- 0
    expect_equal(pattern_distance(a, d), matrix(0, 1, 1))
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

test_that("pattern_distance works with graded codes", {
    # e.g. -2 (much smaller) to 2 (much larger)
    x <- matrix(c(2, 2, 2, 2, 2, 2, -2, 1, 2), 3)
    y <- matrix(c(2, 2, 2, 0, 2, 2, 2, -1, 2), 3)
    expect_equal(pattern_distance(x, y), matrix(2 + 4 + 2, 1, 1))
    expect_equal(pattern_distance(x, y, method = "hamming"), matrix(3, 1, 1))
})

test_that("pattern_distance rejects missing values in the upper triangle", {
    broken <- a
    broken[1, 3] <- NA
    expect_error(pattern_distance(broken, b), "x")
    expect_error(pattern_distance(a, broken), "y")
    lower <- a
    lower[3, 1] <- NA
    expect_equal(pattern_distance(lower, a), matrix(0, 1, 1))
})

test_that("pattern_distance always returns a matrix", {
    expect_true(is.matrix(pattern_distance(a, b)))
    expect_equal(dim(pattern_distance(a, simplify2array(list(a, b)))), c(1, 2))
})
