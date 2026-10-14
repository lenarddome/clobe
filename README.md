# clobe

Global Benchmark and Evaluation Toolkit for Formal Computational Models.

Currently incorporates tools used in comparing model and human heterogeneity, see Dome and Wills (2025).

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

# unique patterns in results, one row per participant or model run
human <- patterns(rbind(c(0.9, 0.5, 0.1),
                        c(0.8, 0.5, 0.2),
                        c(0.1, 0.5, 0.9)), thresholds)
model <- patterns(rbind(c(0.9, 0.5, 0.1),
                        c(0.5, 0.5, 0.5)), thresholds)

# size of the universal set for 3 stimuli
universal <- semiorders(3)

# compare model and human heterogeneity
gdistance(human$inequality_matrices, model$inequality_matrices,
          universal = universal, weight = 0.5,
          frequencies = human$frequencies, xtdo = TRUE)
```

By default, differences within the thresholds count as approximately equal,
so patterns are semiorders. Use `ordering = "weak"` to treat only ties as
equal (weak orderings).

To work with the universal set:

- `semiorders()`, `fubini()` and `stirling2()` count patterns.
- `orderings()` lists every pattern.
- `brutes()` finds the patterns that a given set of measurable values can
  produce, in parallel. This matters when the measurement scale is too coarse
  to realise every semiorder.
- `pattern_distance()` measures how far apart two patterns are.

See the help page of each function for details, and the vignettes for
explanations of the methods and their mathematics:

- `vignette("clobe")`: getting started, from raw data to g-distance.
- `vignette("inequality-matrices")`: ordinal patterns, semiorders and weak
  orderings.
- `vignette("universal-set")`: counting every possible pattern, and when the
  measurement scale matters.
- `vignette("gdistance")`: accommodation, excess flexibility and g-distance.

To install the package with its vignettes, use
`remotes::install_github('lenarddome/clobe', build_vignettes = TRUE)`.

## Paper

Dome, L., & Wills, A. J. (2025). g-Distance: On the comparison of model and human heterogeneity. Psychological Review, 132(3), 632-655. https://doi.org/10.1037/rev0000550
