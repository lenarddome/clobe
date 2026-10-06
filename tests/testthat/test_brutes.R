# with 11 equally spaced values and thresholds of one step, only exact ties
# are approximately equal, so the universal set is every weak ordering of the
# stimuli: the Fubini numbers 1, 3, 13, 75, 541
results <- c(1, 3, 13, 75, 541)

combinatorics <- NULL
for (i in seq(5)) {
   out <- brutes(dependent = seq(0, 1, by = 0.10), length = i,
                 thresholds = c(0.10, -0.10))
   combinatorics <- c(combinatorics, out$n)
}

test_that("brutes reproduce a quick calculation", {
    expect_equal(combinatorics, results)
})

test_that("brutes agree with an exact computation on integers", {
    # integers have no rounding error, so this is an independent reference
    reference <- function(dependent, length, thresholds) {
        grid <- as.matrix(expand.grid(rep(list(dependent), length)))
        keys <- apply(grid, 1, function(row) {
            out <- imac(row, thresholds)
            paste(out[upper.tri(out)], collapse = ",")
        })
        length(unique(keys))
    }
    for (k in 2:4) {
        exact <- reference(0:4, k, c(2, -2))
        expect_equal(brutes(0:4, k, c(2, -2))$n, exact)
        expect_equal(brutes(seq(0, 0.4, by = 0.1), k, c(0.2, -0.2))$n, exact)
    }
})
