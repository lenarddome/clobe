## run Bayesian tests
## it takes data.table x with the following columns: ppt, stim, n [count], total
inequality_matrix <- function (x, stim) {
    ## create ppt profiles
    ppt_table <- participant_profiles(x = x)
    ## reorder alphabetically
    ppt_table <- ppt_table[, c("stim", "success")]
    ppt_table <- ppt_table[order(c(stim), decreasing = FALSE), ]

    ## add mindpoint
    ppt_table <-
        rbind(cbind(stim = "∅", success = 6, total = 12), ppt_table)
    ## create comparisons
    comparison <- t(combn(ppt_table$stim, m = 2))

    ## set up environment
    out <- data.frame(matrix(0, nrow = nrow(ppt_table), ncol = nrow(ppt_table)))
    names <- c(as.character(stim), "∅")
    rownames(out) <- names[order(names)]
    colnames(out) <- names[order(names)]
    ## create inequality matrix
    for (m in seq(nrow(comparison))) {
        pair <- comparison[m, ]
        one <- ppt_table[stim == pair[1]]
        two <- ppt_table[stim == pair[2]]
        successes <- as.numeric(c(one$success, two$success))
        total <- as.numeric(c(one$total, two$total))
        bf <- data.frame(bayes.prop.test(x = successes,
                                         n = total)$stats)[5, ]
        ## find the location in matrix and insert result
        out[pair[1], pair[2]] <- bf$X..comp.1[1]
    }
    # if larger, set to 1
    out[out >= 0.75] <- 1
    ## if smaller, set to 2
    out[out <= 0.25 & out != 0] <- -1
    # if same, set to 0
    out[between(out, lower = 0.25, upper = 0.75, incbounds = FALSE)] <- 0
    return(out)
}
