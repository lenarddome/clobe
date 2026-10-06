# with 11 equally spaced values and thresholds of one step, neighbouring
# values are approximately equal; for up to 4 stimuli, every semiorder can be
# realised, giving the number of labelled semiorders: 1, 3, 19, 183
results <- c(1, 3, 19, 183)

combinatorics <- NULL
for (i in seq(4)) {
   out <- brutes(dependent = seq(0, 1, by = 0.10), length = i,
                 thresholds = c(0.10, -0.10))
   combinatorics <- c(combinatorics, out$n)
}

test_that("brutes reproduce a quick calculation", {
    expect_equal(combinatorics, results)
})

test_that("weak ordering gives the Fubini numbers", {
    # only ties are approximately equal, so every weak ordering is a pattern
    fubini <- sapply(seq(5), function(i) {
        brutes(dependent = seq(0, 1, by = 0.10), length = i,
               thresholds = c(0.10, -0.10), ordering = "weak")$n
    })
    expect_equal(fubini, c(1, 3, 13, 75, 541))
    # with fewer values than stimuli, at most 3 tied groups: 1 + 14 + 36
    expect_equal(brutes(0:2, 4, c(0.5, -0.5), ordering = "weak")$n, 51)
})

test_that("brutes agree with an exact computation on integers", {
    # integers have no rounding error, so this is an independent reference
    reference <- function(dependent, length, thresholds, ordering) {
        grid <- as.matrix(expand.grid(rep(list(dependent), length)))
        keys <- apply(grid, 1, function(row) {
            out <- imac(row, thresholds, ordering)
            paste(out[upper.tri(out)], collapse = ",")
        })
        length(unique(keys))
    }
    for (ordering in c("semiorder", "weak")) {
        for (k in 2:4) {
            exact <- reference(0:4, k, c(2, -2), ordering)
            expect_equal(brutes(0:4, k, c(2, -2), ordering)$n, exact)
            expect_equal(brutes(seq(0, 0.4, by = 0.1), k, c(0.2, -0.2),
                                ordering)$n, exact)
        }
    }
})

test_that("brutes returns each unique inequality matrix once", {
    out <- brutes(seq(0, 1, by = 0.25), 3, c(0.10, -0.10))
    keys <- apply(out$inequality_matrices, 3,
                  function(m) paste(m[upper.tri(m)], collapse = ","))
    expect_equal(dim(out$inequality_matrices), c(3, 3, out$n))
    expect_false(any(duplicated(keys)))
    m <- out$inequality_matrices[, , 1]
    expect_true(all(m[lower.tri(m, diag = TRUE)] == 2))
})

test_that("permutations are only returned on request, in the documented order", {
    dependent <- c(0.3, 0.1, 0.2)
    expect_null(brutes(dependent, 2, c(0.1, -0.1))$permutations)
    out <- brutes(dependent, 2, c(0.1, -0.1), return_permutations = TRUE)
    # every combination, with the last column changing fastest
    expected <- as.matrix(rev(expand.grid(rep(list(dependent), 2))))
    dimnames(expected) <- NULL
    expect_equal(out$permutations, expected)
})

test_that("brutes handles a single stimulus", {
    out <- brutes(seq(0, 1, by = 0.1), 1, c(0.1, -0.1))
    expect_equal(out$n, 1)
    expect_equal(dim(out$inequality_matrices), c(1, 1, 1))
})

test_that("brutes rejects invalid input", {
    expect_error(brutes(seq(0, 1, by = 0.1), 0, c(0.1, -0.1)), "length")
    expect_error(brutes(numeric(0), 2, c(0.1, -0.1)), "dependent")
    expect_error(brutes(seq(0, 1, by = 0.1), 2, 0.1), "thresholds")
    expect_error(brutes(seq(0, 1, by = 0.1), 2, c(0.1, -0.1), "total"),
                 "ordering")
    expect_error(brutes(seq(0, 1, length.out = 1000), 10, c(0.1, -0.1)),
                 "too many")
})

test_that("brutes handles more than 40 cells per matrix", {
    # 10 stimuli have 45 cells; with 2 values, the patterns are all ties or a
    # split into two tied groups: 1 + 2^10 - 2
    expect_equal(brutes(c(0, 1), 10, c(0.5, -0.5))$n, 1023)
})

test_that("brutes gives the same result on one or several threads", {
    for (ordering in c("semiorder", "weak")) {
        one <- brutes(seq(0, 1, by = 0.1), 5, c(0.1, -0.1), ordering,
                      threads = 1)
        several <- brutes(seq(0, 1, by = 0.1), 5, c(0.1, -0.1), ordering,
                          threads = 4)
        expect_identical(several, one)
    }
    expect_error(brutes(seq(0, 1, by = 0.1), 2, c(0.1, -0.1), threads = 0),
                 "threads")
})
