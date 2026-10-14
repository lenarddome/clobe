gdistance <- function(human, model, universal, weight, frequencies,
                      xtdo = FALSE) {
  .gdistance(.as_patterns(human, "human"), .as_patterns(model, "model"),
             universal, weight, frequencies, xtdo)
}
