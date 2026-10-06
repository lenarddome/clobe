thresholds <- c(0.1, -0.1)
data <- rbind(c(0.9, 0.5, 0.1),
              c(0.1, 0.5, 0.9),
              c(0.8, 0.5, 0.2),  # same pattern as the first row
              c(0.5, 0.5, 0.5),
              c(0.9, 0.4, 0.0))  # same pattern as the first row

test_that("patterns finds the unique patterns in order of appearance", {
    out <- patterns(data, thresholds)
    expect_equal(out$n, 3)
    expect_equal(out$counts, c(3, 1, 1))
    expect_equal(out$frequencies, c(3, 1, 1) / 5)
    expect_equal(out$pattern, c(1, 2, 1, 3, 1))
    expect_equal(out$inequality_matrices[, , 1], imac(data[1, ], thresholds))
    expect_equal(out$inequality_matrices[, , 2], imac(data[2, ], thresholds))
    expect_equal(out$inequality_matrices[, , 3], imac(data[4, ], thresholds))
})

test_that("patterns accepts data frames and the ordering", {
    expect_equal(patterns(as.data.frame(data), thresholds),
                 patterns(data, thresholds))
    # 0.50 and 0.45 are approximately equal in a semiorder, but not in a
    # weak ordering
    expect_equal(patterns(rbind(c(0.50, 0.45), c(0.50, 0.50)), thresholds)$n,
                 1)
    out <- patterns(rbind(c(0.50, 0.45), c(0.50, 0.50)), thresholds,
                    ordering = "weak")
    expect_equal(out$n, 2)
})

test_that("patterns output works with gdistance", {
    human <- patterns(data, thresholds)
    model <- patterns(data[c(1, 2), ], thresholds)
    out <- gdistance(human$inequality_matrices, model$inequality_matrices,
                     universal = semiorders(3), weight = 0.5,
                     frequencies = human$frequencies, xtdo = TRUE)
    expect_equal(out$alpha, 0.8)
    expect_equal(out$beta, 0)
})

test_that("patterns rejects invalid input", {
    expect_error(patterns(c(0.1, 0.2), thresholds), "matrix")
    expect_error(patterns(rbind(c(0.1, NA)), thresholds), "missing")
    expect_error(patterns(data.frame(a = "x", b = "y"), thresholds),
                 "numeric")
})
