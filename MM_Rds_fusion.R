### Script to average down the files and make an averaged Fmax across the montecarlo sims ###

# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/MMA");

cnames1 <- read.csv("Burst/Elbow ABAD Burst.csv", header = TRUE, sep = ",", quote =
                               "\"",dec = ".", fill = TRUE, comment.char = "")

r1 <- readRDS("MM/0_67/Burst/Elbow_ABAD_MM1_m1_MC_raw_replicates.Rds")
r1_2 <- readRDS("MM/0_67/Burst/Elbow_ABAD_MM2_m1_MC_raw_replicates.Rds")

cnames2 <- read.csv("Biped/Elbow ABAD Biped.csv", header = TRUE, sep = ",", quote =
                      "\"",dec = ".", fill = TRUE, comment.char = "")
r2 <- readRDS("MM/0_67/Biped/Elbow_ABAD_MM1_m1_MC_raw_replicates.Rds")
r2_2 <- readRDS("MM/0_67/Biped/Elbow_ABAD_MM2_m1_MC_raw_replicates.Rds")

cnames3 <- read.csv("Quad/Elbow ABAD Quad.csv", header = TRUE, sep = ",", quote =
                      "\"",dec = ".", fill = TRUE, comment.char = "")
r3 <- readRDS("MM/0_67/Quad/Elbow_ABAD_MM1_m1_MC_raw_replicates.Rds")
r3_2 <- readRDS("MM/0_67/Quad/Elbow_ABAD_MM2_m1_MC_raw_replicates.Rds")

cnames1$SUM <- rowSums(cnames1[2:ncol(cnames1)]);

#### Function to iterate second half of code n times ####
polymerization <- function(m1, m2, cn, name.output) {
  
  raw <- c(r1,r2)

  ## restructure output to 12 matrices with 1000 columns (ignoring time)
  RSraw <- lapply(2:ncol(raw[[1]]), function(x) {
    out <- sapply(1:length(raw), function(y) {
      data <- raw[[y]][,x]
    })
  })
  
  # now to summarise - get mean values for each row of 1000 replicates
  mean <- sapply(1:length(RSraw), function(a) {
    by.row <- sapply(1:nrow(RSraw[[1]]), function(b){
      out <- mean(RSraw[[a]][b,])
    })
  })
  # Standard deviation
  stdev <- sapply(1:length(RSraw), function(a) {
    by.row <- sapply(1:nrow(RSraw[[1]]), function(b){
      out <- sd(RSraw[[a]][b,])
    })
  })
  ## prepare for export
  mean.out <- cbind(cn[,1], mean)
  colnames(mean.out) <- colnames(cn)
  stdev.out <- cbind(cn[,1], stdev)
  colnames(stdev.out) <- colnames(cn)
    ## export
  saveRDS(raw, paste0(name.output, "_combined_replicates.Rds"))
  write.csv(mean, paste0(name.output, "_mean.csv"))
  write.csv(stdev, paste0(name.output, "_stdev.csv"))
}

polymerization(m1 = r1, m2 = r1_2, cn= cnames1, name.output = "MM/Combined/0_67/Quad/Elbow_ABAD_m1_MC")