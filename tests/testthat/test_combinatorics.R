test_that("stirling2 gives Stirling numbers of the second kind", {
    expect_equal(stirling2(0, 0), 1)
    expect_equal(stirling2(5, 0), 0)
    expect_equal(stirling2(3, 5), 0)
    expect_equal(stirling2(5, 2), 15)
    expect_equal(stirling2(10, 3), 9330)
    expect_equal(stirling2(4, 1:4), c(1, 7, 6, 1))
})

test_that("fubini gives the number of weak orderings", {
    expect_equal(fubini(0:8),
                 c(1, 1, 3, 13, 75, 541, 4683, 47293, 545835))
    # at most 3 tied groups: 1 + 14 + 36
    expect_equal(fubini(4, levels = 3), 51)
    expect_equal(fubini(4, levels = 1), 1)
    expect_equal(fubini(3, levels = 10), fubini(3))
})

test_that("semiorders gives the number of labelled semiorders", {
    expect_equal(semiorders(0:8),
                 c(1, 1, 3, 19, 183, 2371, 38703, 763099, 17648823))
})

test_that("counts agree with brutes", {
    for (k in 1:4) {
        expect_equal(brutes(0:10, k, c(1, -1), ordering = "weak")$n, fubini(k))
        expect_equal(brutes(0:10, k, c(1, -1))$n, semiorders(k))
    }
    expect_equal(brutes(0:2, 4, c(1, -1), ordering = "weak")$n,
                 fubini(4, levels = 3))
})

test_that("counts warn when they are not exact", {
    expect_silent(fubini(16))
    expect_warning(fubini(17), "not exact")
    expect_silent(semiorders(13))
    expect_warning(semiorders(14), "not exact")
    expect_warning(stirling2(40, 20), "not exact")
})

test_that("counts reject invalid input", {
    expect_error(stirling2(-1, 0), "non-negative")
    expect_error(fubini(2.5), "non-negative")
    expect_error(semiorders(NA), "non-negative")
    expect_error(fubini(3, levels = -1), "non-negative")
})
