# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
While the major version is 0, breaking changes bump the minor version.

# clobe (development version)

## Breaking changes

- `gdistance(xtdo = TRUE)` returns the accommodated patterns as
  `accommodation` (was misspelled `accomodation`), as 1-based indices into
  `human` (were 0-based), in a plain integer vector (was a one-column matrix).

## Fixed

- `imac()` compared differences against the thresholds without a tolerance, so
  equal steps were classified inconsistently (e.g. `0.3 - 0.2` is `0.0999...`
  and was treated as approximately equal, while `0.2 - 0.1` was not). This
  changes the results of `brutes()` for the documented example: the universal
  set for 11 equally spaced values and 4 stimuli is 75, not 183.
- `imac()` returned wrong cells (e.g. `4` or `2`) when the dependent measure
  spans 2 or more, because `2` was also used to mark the lower triangle.
- `gdistance()` compared patterns on the upper triangle when computing
  accommodation (alpha) but on the whole matrix when computing predictions
  (beta), so the same model pattern could count as both accommodating a human
  pattern and as a novel prediction. Both now compare the strict upper
  triangle.

## Changed

- `imac()` now errors if `thresholds` is not `c(upper, lower)`.
- `gdistance()` now validates its input: matrix dimensions, the length of
  `frequencies`, `universal >= dim(human)[3]` and `0 <= weight <= 1`.
- `gdistance()` documentation no longer claims `weight` defaults to 0.5.

- CI: update `actions/checkout` to v4.
- Ignore macOS `.DS_Store` files.
