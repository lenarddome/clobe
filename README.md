# clobe

Global Benchmark and Evaluation Toolkit for Formal Computational Models.

Currently incorporates tools used in comparing model and human heterogeneity, see Dome and Wills (2023).

To install, please use:

```r
remotes::install_github('lenarddome/clobe')
```

## Usage

```r
library(clobe)

thresholds <- c(0.1, -0.1)

# discretise a result into an inequality matrix of ordinal relationships
imac(c(0.9, 0.5, 0.45), thresholds)

# size of the universal set: every pattern observable for 4 stimuli
# measured on a 0.1 grid (runs in parallel)
brutes(seq(0, 1, by = 0.1), length = 4, thresholds = thresholds)$n

# compare model and human heterogeneity
# gdistance(human, model, universal, weight = 0.5, frequencies, xtdo = TRUE)
```

By default, differences within the thresholds count as approximately equal,
so patterns are semiorders. Use `ordering = "weak"` to treat only ties as
equal (weak orderings). See `?imac`, `?brutes` and `?gdistance`.

## Paper

Dome, L., & Wills, A. (2023, April 11). g-distance: On the comparison of model and human heterogeneity. https://doi.org/10.31234/osf.io/ygmcj
