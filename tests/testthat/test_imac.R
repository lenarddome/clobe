context('imac')
load('data/test_imac.RData')

set.seed(1)
compared = imac(probabilities = runif(15), thresholds = c(0.10, -0.10))

test_that('Inequality matrix constructor passed', {
    expect_equal(stored, compared)
    }
)
