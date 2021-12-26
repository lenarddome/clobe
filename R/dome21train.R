dome21train <-
    function(trial_order, phase_index, trials, ppt) {

   ## create test items for ibre
   stimuli <- rbind(
      ## A,   B,   C,  X,  Common,  Rare
       c(1,   0,   0,  1,  0,  0), # A
       c(0,   1,   0,  1,  0,  0), # B
       c(0,   0,   1,  1,  0,  0), # C
       c(1,   1,   0,  1,  1,  0), # AB
       c(1,   0,   1,  1,  0,  1), # AC
       c(0,   1,   1,  1,  0,  0) # BC
   )

   ## create test items for ice
   ## add stimuli names
   stimuli <- data.frame(cbind(0, stimuli))

   stimuli[, 1] <- c("A", "B", "C", "AB", "AC", "BC")

   bigtr <- NULL
   for (i in trial_order) {
       bigtr <- rbind(bigtr, stimuli[stimuli$X1 == i, ])
   }

   phase_index <- as.numeric(phase_index)
   train_blocks <- length(phase_index[phase_index == 0])/8
   test_blocks <- length(phase_index[phase_index == 2])/12
   bigtr <- cbind(ctrl = phase_index, ppt, trials, bigtr)
   bigtr[1, 1] <- 1
   # tidy up and return output
   rownames(bigtr) <- NULL
   colnames(bigtr) <- c("ctrl", "subj", "trials", "stim",
                        "x1", "x2", "x3", "x4",
                        "t1", "t2")
   return(bigtr)
}
