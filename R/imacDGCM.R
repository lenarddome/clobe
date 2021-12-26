dome21dissgcm <- function(params) {
## declare initial state of the model
    st <- list(attentional_weights = params[1:3]/sum(abs(params[1:3])), # normalized
             c = params[4], # generalization parameter 0-Inf
             s = params[5], # similarity weighing 0-1
             b = params[6], # baseline similarity 0-1
             t = c(3, 1),
             beta = c(1, 1)/2, # category resonse bias 0-1
             gamma = 1, # response determinism 0-10
             theta = 1, # decay rate 0-Inf
             r = 1,
             colskip = 1,
             outcomes = 2,
             exemplars = matrix(c(1, 1, 0, 1,
                                  1, 0, 1, 2), ncol = 4, byrow = T))

## run model and extract probabilities
    out <- stdissGCM(st, data.matrix(test[, -1]), exemplar_decay = FALSE,
                         exemplar_mute = TRUE, dec = "NOISE")
    tout <- out$p
    tout <- data.frame(cbind(0, tout))
    tout[, 1] <- test$stim
    colnames(tout) <- c("stim", "common", "rare")

## turn wide data format into long
    wide_out <- aggregate(tout[, 2:ncol(tout)], by = list(tout$stim), mean)
    long_out <- tidyr::gather(wide_out, key = category, value = prop, 2:3)
    long_out <- long_out[order(long_out[, 1]), ]
    colnames(long_out) <- c("stim", "cat", "prop")

## stim is taken from environment, because psp can't pass other
## arguments to function call as of 2021-06-02
    ibre <- long_out[long_out$stim %in% c("BC", "A"), ]
    ibre <- rbind(ibre, cbind(stim = "∅", cat = "rare", prop = 0.5))
    ibre <- data.table(ibre)
    ibre <- ibre[cat == "rare", ]
    ibre <- type.convert(ibre[order(ibre$prop), ], as.is = TRUE)
    colnames(ibre) <- c("stim", "resp", "success")
    stim <- ibre$stim[order(ibre$stim)]

    comparison <- t(combn(stim, m = 2))
    ## set up environment
    out <- data.frame(matrix(0, nrow = nrow(ibre), ncol = nrow(ibre)))
    names <- c(as.character(ibre$stim))
    rownames(out) <- names[order(names)]
    colnames(out) <- names[order(names)]
    ## create inequality matrix
    for (m in seq(nrow(comparison))) {
        pair <- comparison[m, ]
        one <- ibre[stim == pair[1]]
        two <- ibre[stim == pair[2]]
        ## find the location in matrix and insert result
        out[pair[1], pair[2]] <- one$success - two$success
    }
    # if larger, set to 1
    out[out >= 0.10] <- 1
    ## if smaller, set to 2
    out[out <= -0.10 & out != 0] <- -1
    # if same, set to 0
    out[between(out, lower = -0.10, upper = 0.10, incbounds = FALSE)] <- 0
    out[!upper.tri(out)] <- NA
    return(out)
}
