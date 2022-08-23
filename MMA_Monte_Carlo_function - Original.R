# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/MMA");

#Load and Setup#
{
  # read in raw data
  mma_raw <- read.csv("Quad/hip ABAD quad.csv", header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  # create SUM column
  mma_raw$SUM <- rowSums(mma_raw[2:12]);
}

#### Function to iterate second half of code n times ####
MMA.MC.loop <- function(mma, n, name.input) {
  # get list of n length of modified matrices
  raw <- lapply(1:n, function(all) {
    mma$ADD     <- mma$ADD * runif(1,min=0.79,max=1.21)
    mma$AMB     <- mma$AMB * runif(1,min=0.79,max=1.21)
    mma$CFB     <- mma$CFB * runif(1,min=0.79,max=1.21)
    mma$FTE     <- mma$FTE * runif(1,min=0.79,max=1.21)
    mma$FTI.Fan <- mma$FTI.Fan * runif(1,min=0.79,max=1.21)
    mma$FTI2    <- mma$FTI2 * runif(1,min=0.79,max=1.21)
    mma$IFM     <- mma$IFM * runif(1,min=0.79,max=1.21)
    mma$ILF     <- mma$ILF * runif(1,min=0.79,max=1.21)
    mma$ITB     <- mma$ITB * runif(1,min=0.79,max=1.21)
    mma$PIFE    <- mma$PIFE * runif(1,min=0.79,max=1.21)
    mma$PIFI    <- mma$PIFI * runif(1,min=0.79,max=1.21)
    # new sum
    mma$SUM <- rowSums(mma[2:12]);
    return(mma)
  })
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
  # quartiles - set to 0.25
  quart0.25 <- sapply(1:length(RSraw), function(a) {
    by.row <- sapply(1:nrow(RSraw[[1]]), function(b){
      out <- quantile(RSraw[[a]][b,], probs = 0.25)
    })
  })
  # quartiles - 0.5
  quart0.5 <- sapply(1:length(RSraw), function(a) {
    by.row <- sapply(1:nrow(RSraw[[1]]), function(b){
      out <- quantile(RSraw[[a]][b,], probs = 0.5)
    })
  })
  # quartiles - 0.75
  quart0.75 <- sapply(1:length(RSraw), function(a) {
    by.row <- sapply(1:nrow(RSraw[[1]]), function(b){
      out <- quantile(RSraw[[a]][b,], probs = 0.75)
    })
  })
  ## prepare for export
  mean.out <- cbind(mma[,1], mean)
  colnames(mean.out) <- colnames(mma)
  quart0.25.out <- cbind(mma[,1], quart0.25)
  colnames(quart0.25.out) <- colnames(mma)
  quart0.5.out <- cbind(mma[,1], quart0.5)
  colnames(quart0.5.out) <- colnames(mma)
  quart0.75.out <- cbind(mma[,1], quart0.75)
  colnames(quart0.75.out) <- colnames(mma)
  ## export
  saveRDS(raw, paste0(name.input, "_raw_replicates.Rds"))
  write.csv(mean.out, paste0(name.input, "_mean.csv"))
  write.csv(quart0.25.out, paste0(name.input, "_quart0.25.csv"))
  write.csv(quart0.5.out, paste0(name.input, "_quart0.5.csv"))
  write.csv(quart0.75.out, paste0(name.input, "_quart0.75.csv"))
}

#### Run the function ####
MMA.MC.loop(mma = mma_raw, n = 1000, name.input = "MMA_MC")
