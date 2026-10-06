# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
While the major version is 0, breaking changes increase the minor version.
Breaking changes are marked **Breaking**.

## [Unreleased]

## [0.7.0] - 2026-10-06

### Added

- `ordering` argument to `imac()` and `brutes()`: `"semiorder"` (the default)
  treats differences within the thresholds as approximately equal; `"weak"`
  treats only ties as approximately equal, giving weak orderings, which
  `brutes()` counts as the Fubini numbers.
- `brutes()` runs in parallel using RcppParallel, on all available cores by
  default. Set the number of threads with the new `threads` argument or with
  `RcppParallel::setThreadOptions()`. It can now be interrupted.
- `return_permutations` argument to `brutes()`.
- `return_matrices` argument to `brutes()`: with `FALSE`, only the number of
  patterns is returned. For 8 stimuli, this halves the running time and avoids
  returning an array of about 3.7 GB.
- `stirling2()`, `fubini()` and `semiorders()` count ordinal patterns without
  enumerating them: Stirling numbers of the second kind, weak orderings
  (optionally with a limited number of tied groups) and labelled semiorders.
  They warn when a count exceeds 2^53 and may not be exact.
- `orderings()` lists every semiorder or weak ordering of a number of stimuli
  directly, without trying every assignment of values. For 6 stimuli it takes
  0.01 seconds, against 88 seconds for `brutes()` with values fine enough to
  realise every semiorder.
- `pattern_distance()` measures the distance between inequality matrices,
  either as the Kemeny-Snell distance, where a reversed pair counts twice as
  much as a tied one, or as the number of pairs that differ.
- `patterns()` turns results, one row per participant or model run, into the
  unique inequality matrices with their counts and frequencies, ready for
  `gdistance()`.
- `inst/CITATION`, so that `citation("clobe")` works as documented.
- Examples for `brutes()` and `gdistance()`, and a usage section in the README.
- `.clang-format` file; the C++ sources follow the Google C++ Style Guide.

### Changed

- **Breaking:** `imac()` treats a difference exactly equal to a threshold as
  approximately equal; it was classified as larger or smaller, apart from
  rounding errors. Together with the tolerance fix below, this keeps the
  previous results of `brutes()` for 11 equally spaced values and thresholds of
  one step (19 and 183 patterns for 3 and 4 stimuli), which are now computed
  consistently rather than by accident of rounding.
- **Breaking:** `gdistance(xtdo = TRUE)` returns the accommodated patterns as
  `accommodation` (was misspelled `accomodation`), as 1-based indices into
  `human` (were 0-based), in an integer vector (was a one-column matrix).
- **Breaking:** `brutes()` returns the matrix of permutations only with
  `return_permutations = TRUE`, as it has `length(dependent)^length` rows.
- **Breaking:** `brutes()` returns the unique inequality matrices sorted by
  pattern rather than in order of discovery.
- **Breaking:** `gdistance()` computes beta over unique patterns, as defined
  in Dome and Wills (2025): duplicated slices in `model` count as one novel
  prediction, and the denominator is `universal` minus the number of unique
  patterns in `human`. Beta was inflated when either array contained a pattern
  more than once.
- **Breaking:** only `brutes()`, `gdistance()` and `imac()` are exported,
  instead of every object in the package namespace.
- `brutes()` is much faster: each matrix is encoded as a compact key and
  deduplicated, instead of being compared against every matrix found so far,
  and permutations are generated on the fly instead of being stored. For 11
  values and 5 stimuli it takes milliseconds instead of about 23 seconds.
- `imac()` and `brutes()` check that `thresholds` is `c(upper, lower)`, and
  `brutes()` checks `dependent` and `length`.
- `gdistance()` checks the matrix dimensions, the length of `frequencies`,
  that `universal` is not smaller than the number of unique human patterns, and that
  `weight` is between 0 and 1.
- Cite the published version of Dome and Wills (2025), *Psychological
  Review*, <https://doi.org/10.1037/rev0000550>, instead of the 2023 preprint.
- `DESCRIPTION` points `URL` and `BugReports` to the clobe repository (they
  pointed to globe).
- CI uses `actions/checkout` v4.

### Removed

- Dependency on RcppAlgos.
- The unused suggested packages plyr and abind.
- The redundant `Author` and `Maintainer` fields in `DESCRIPTION`, which are
  generated from `Authors@R`.
- `NEWS.md`, replaced by this changelog.

### Fixed

- `imac()` compared differences against the thresholds without a tolerance, so
  equal steps were classified inconsistently: `0.3 - 0.2` is `0.0999...` and
  was treated as approximately equal, while `0.2 - 0.1` was not. Differences
  are now compared with a tolerance of `sqrt(.Machine$double.eps)`.
- `imac()` returned wrong cells, such as `4` or `2`, when the dependent measure
  spans 2 or more, because `2` was also used to mark the lower triangle.
- `gdistance()` compared patterns on the upper triangle when computing
  accommodation (alpha) but on the whole matrix when computing predictions
  (beta), so the same model pattern could count both as accommodating a human
  pattern and as a novel prediction. Both now compare the strict upper
  triangle.
- The `gdistance()` formula in the documentation (beta is squared), the
  `imac()` formula and value description, a claim that `weight` defaults to
  0.5, and typos.
- Duplicate entries in `.gitignore`.

## [0.6.0] - 2023-04-20

Last version before this changelog was kept.

[Unreleased]: https://github.com/lenarddome/clobe/compare/v0.7.0...HEAD
[0.7.0]: https://github.com/lenarddome/clobe/compare/v0.6-beta...v0.7.0
[0.6.0]: https://github.com/lenarddome/clobe/releases/tag/v0.6-beta
