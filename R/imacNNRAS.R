# params <- c(2.87, 2.48, 4.42, 4.42, 0.222, 1.13, 0.401)
## requires sourcing dome21train and ply207utility

## list of pattern sets corresponding to each step in the sequential analysis
## stimuli <- list(c("BC"),
##                 c("BC", "B", "C"),
##                 c("BC", "B", "C", "A"),
##                 c("BC", "B", "C", "A", "AB", "AC")
##                 c("BC", "B", "C", "A", "AB", "AC", "#AB", "#AC")
##                 )

dome21nnras <- function(params) {

    ## combine st params
    st <- list(P = params[1],        ## attentional normalization
           phi = params[2],          ## response consistency
           lambda = params[3],       ## learning rate parameter
           mu = params[4],           ## attentional learning rate parameter
           rho = params[5],          ## attentional shift rate
           colskip = 1,
           outcomes = 2,
           w = matrix(0, nrow = 2, ncol = 3),
           eta = rep(1, 3)         ## intial salience of cues
       )
    ## run simluation with given params and subset choice probabilities
    out <- slpNNCAG(st, data.matrix(tr[, c(1, 5, 6, 7, 9, 10)]),
                    xtdo = FALSE)
    tout <- out$p
    tout <- data.frame(cbind(0, 0, tout))
    tout[, 1] <- tr$stim
    tout[, 2] <- tr$ctrl
    colnames(tout) <- c("stim", "phase", "common", "rare")
    ## subset test phase
    training <- tout[tout$phase < 2, ]$stim
    training[training == "AB"] <- "#AB"
    training[training == "AC"] <- "#AC"
    tout[tout$phase < 2, ]$stim <- training
    tout <- tout[, -2]
    ## turn wide into long
    wide_out <- aggregate(tout[, 2:ncol(tout)], by = list(tout$stim), mean)
    long_out <- tidyr::gather(wide_out, key = category, value = prop, 2:3)
    long_out <- long_out[order(long_out[, 1]), ]
    colnames(long_out) <- c("stim", "cat", "prop")
    ## stim is taken from environment, because psp can't pass other
    ## arguments to function call as of 2021-06-02
    ibre <- long_out[long_out$stim %in% stim, ]
    ibre <- rbind(ibre, cbind(stim = "∅", cat = "rare", prop = 0.5))
    ibre <- ibre[ibre$cat == "rare", ]
    ibre <- type.convert(ibre[order(ibre$prop), ], as.is = TRUE)
    ## put together ppt table with direction of equality/un-equality
    diff <- NULL
    for (i in 2:nrow(ibre)) {
        diff <- c(diff, ibre[i - 1, 3] - ibre[i, 3])
    }
    diff[nrow(ibre)] <- 0
    direction <- sapply(diff, FUN = direction_class)
    punctuation <-
        sapply(direction,
               function(x) switch(x, end = "", same = "", smaller = "<"),
               USE.NAMES = FALSE)
    test_ibre <- cbind(ibre, diff, direction, punctuation)
    ## ordinal classification
    ordinal_ibre <- construct_ordinal(test_ibre)
    return(paste(ordinal_ibre, collapse = " "))
}
