results <- c(1, 3, 19, 183)

combinatorics <- NULL
for (i in seq(4)) {
   out <- brutes(dependent = seq(0, 1, by = 0.10), length = i,
                 threshold = c(0.10, -0.10))
   combinatorics <- c(combinatorics, out$n)
}

test_that("brutes reproduce a quick calculation", {
    expect_equal(combinatorics, results)
})
