## utility functions used in analysis and simulations of ply211
## amended to fit the approach taken in simulating with EXIT
## also, Plymouth HPC has R 3.6.1, while I have 4.1.0
## which means I have to add decreasing = FALSE for some reason on line 81
## seem to order special characters differently
## otherwise equalities in sequence 001 will have two different orderings

## checks which direction the difference is
.directionClass <- function(x) {
    if (is.na(x)) x <- 0
    if (x == 0) {
        "end"
    } else if (abs(x) >= 0.10) {
        "smaller"
    } else {
        "same"
    }
}

## if a value is missing, insert maximum value in opposite direction of missing
## value (if no rare response, insert -12 (0 rare - 12 common))
.grouped <- function(foo, bar) {
    if (length(foo) == 1) {
    switch(foo,
        rare = -12,
        common = 12)
    } else {
        bar[1] - bar[2]
    }
}

## construct ordinal patterns
## it takes a data.table output by participant_table
ostring <- function(x) {
    number <- nrow(x) # check how many members need to be ordered
    odin <- as.vector(t(x[, c("stim", "punctuation")]))
    ## look for equalities
    identities <- unique(c(which(odin == "") - 1,
                           which(odin == "") + 1))
    ## remove extra member at the end (usually an empty string: "")
    overwrite <- which(identities == (number * 2) + 1)
    if (length(overwrite) > 0) identities <- identities[-overwrite]
    ## cluster same response identities
    index <- sort(c(which(odin == ""), identities))
    clusters <- split(index, cumsum(c(0, diff(index) >= 2)))
    ## find unequal signs
    ## remove last two items (item and empty string) from search length
    search_length <- (number * 2) - 2
    unequal <- cbind(loc = c(1:search_length)[-index], cluster = 0)
    ## put everything into one object
    bins <- NULL
    for (j in seq(length(clusters))) {
        bins <- rbind(bins,
                      cbind(loc = clusters[[j]], cluster = j))
    }
    ## if everything is unequal, put everything into cluster 0
    if (length(index) == 0) {
        unequal <- cbind(loc = c(1:search_length), cluster = 0)
        bins <- NULL
    }
    locs <- NULL
    if (length(unequal) == 1) {
        locs <- data.table::as.data.table(bins)
    } else {
        locs <- data.table::as.data.table(rbind(bins, unequal))
    }
    ## paste everything together into one scale
    ## split non-equal identities into separate list elements
    locs <- locs[order(loc)]
    locs  <- split(locs, cumsum(c(0, diff(locs$cluster) != 0)))
    # amend each element of a list with the right brackets
    # then combine them into one scale
    out <- NULL
    odin[odin == ""] <- NA
    for (l in seq(length(locs))) {
        ordinal_convert <- paste(na.omit(odin[locs[[l]]$loc]), collapse = " ")
        if (locs[[l]]$cluster[1] > 0 && length(locs[[l]]$cluster) > 2) {
            ordinal_convert <-
                paste("[", paste(sort(na.omit(odin[locs[[l]]$loc])),
                                    collapse = ", "),
                      "]", sep = "")
        }
        out <- c(out, ordinal_convert)
    }
    return(out)
}
