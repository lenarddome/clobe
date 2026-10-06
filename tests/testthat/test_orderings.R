test_that("orderings gives as many patterns as the counting functions", {
    for (k in 1:6) {
        expect_equal(orderings(k, "weak")$n, fubini(k))
        expect_equal(orderings(k)$n, semiorders(k))
    }
})

test_that("orderings gives the same matrices as brutes", {
    # 11 values with a threshold of one step realise every pattern of up to
    # 4 stimuli
    for (k in 1:4) {
        for (ordering in c("semiorder", "weak")) {
            expect_identical(
                orderings(k, ordering)$inequality_matrices,
                brutes(0:10, k, c(1, -1), ordering)$inequality_matrices)
        }
    }
    # 5 stimuli need a finer grid for every semiorder
    expect_identical(orderings(5)$inequality_matrices,
                     brutes(0:50, 5, c(5, -5))$inequality_matrices)
    expect_identical(orderings(5, "weak")$inequality_matrices,
                     brutes(0:10, 5, c(1, -1), "weak")$inequality_matrices)
})

test_that("orderings handles a single stimulus", {
    out <- orderings(1)
    expect_equal(out$n, 1)
    expect_equal(out$inequality_matrices, array(2, c(1, 1, 1)))
})

test_that("orderings rejects invalid input", {
    expect_error(orderings(0), "length")
    expect_error(orderings(3, "total"), "ordering")
    expect_error(orderings(12), "too many")
})
