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
- `brutes()` no longer returns the matrix of permutations by default, as it
  has `length(dependent)^length` rows; use `return_permutations = TRUE`.
- `brutes()` returns the unique inequality matrices sorted by pattern rather
  than in order of discovery.
- `imac()` treats a difference exactly equal to a threshold as approximately
  equal (it was classified as larger or smaller, apart from rounding errors).
  Together with the tolerance fix below, this keeps the previous results of
  `brutes()` for 11 equally spaced values and thresholds of one step (19 and
  183 patterns for 3 and 4 stimuli), which are now computed consistently
  rather than by accident of rounding.

## Added

- `ordering` argument to `imac()` and `brutes()`: `"semiorder"` (default)
  treats differences within the thresholds as approximately equal; `"weak"`
  treats only ties as approximately equal, giving weak orderings, which
  `brutes()` counts as the Fubini numbers.
- `return_permutations` argument to `brutes()`.

## Fixed

- `imac()` compared differences against the thresholds without a tolerance, so
  equal steps were classified inconsistently (e.g. `0.3 - 0.2` is `0.0999...`
  and was treated as approximately equal, while `0.2 - 0.1` was not).
  Differences are now compared with a tolerance of `sqrt(.Machine$double.eps)`.
- `imac()` returned wrong cells (e.g. `4` or `2`) when the dependent measure
  spans 2 or more, because `2` was also used to mark the lower triangle.
- `gdistance()` compared patterns on the upper triangle when computing
  accommodation (alpha) but on the whole matrix when computing predictions
  (beta), so the same model pattern could count as both accommodating a human
  pattern and as a novel prediction. Both now compare the strict upper
  triangle.

## Changed

- `brutes()` is much faster: matrices are deduplicated with a hash set instead
  of being compared against every matrix found so far, and permutations are
  generated on the fly instead of being stored. For 11 values and 5 stimuli
  it takes milliseconds instead of ~23 seconds.
- `imac()` now errors if `thresholds` is not `c(upper, lower)`.
- `gdistance()` now validates its input: matrix dimensions, the length of
  `frequencies`, `universal >= dim(human)[3]` and `0 <= weight <= 1`.
- `brutes()` validates `dependent`, `length` and `thresholds`.
- `gdistance()` documentation no longer claims `weight` defaults to 0.5.

## Removed

- Dependency on RcppAlgos.

## Changed (infrastructure)

- C++ sources follow the Google C++ Style Guide and are formatted with
  `clang-format` (`.clang-format`: `BasedOnStyle: Google`).
- CI: update `actions/checkout` to v4.
- Ignore macOS `.DS_Store` files.
