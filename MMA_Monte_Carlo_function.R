# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/MMA");

#Load and Setup#
{
  # read in raw data
  mma_raw <- read.csv("Fmax/Fmax1 Median.csv", header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
#  mma_raw <- read.csv("Fmax/Fmax Mean.csv", header = TRUE, sep = ",", quote =
#                        "\"",dec = ".", fill = TRUE, comment.char = "")
  # create SUM column
  mma_raw$SUM <- rowSums(mma_raw[2:ncol(mma_raw)]);
}

#### Function to iterate second half of code n times ####
MMA.MC.loop <- function(mma, n, name.input) {
  # get list of n length of modified matrices
  raw <- lapply(1:n, function(all) {
    mma$SC      <- mma$SC * runif(1,min=0.79,max=1.21)
    mma$LD      <- mma$LD * runif(1,min=0.79,max=1.21)
    mma$TM      <- mma$TM * runif(1,min=0.79,max=1.21)
    mma$DS      <- mma$DS * runif(1,min=0.79,max=1.21)
    mma$SHA     <- mma$SHA * runif(1,min=0.79,max=1.21)
    mma$SHP     <- mma$SHP * runif(1,min=0.79,max=1.21)
    mma$SUBS    <- mma$SUBS * runif(1,min=0.79,max=1.21)
    mma$TR.S    <- mma$TR.S * runif(1,min=0.79,max=1.21)
    mma$TR.C    <- mma$TR.C * runif(1,min=0.79,max=1.21)
    mma$TR.M    <- mma$TR.M * runif(1,min=0.79,max=1.21)
    mma$TR.L    <- mma$TR.L * runif(1,min=0.79,max=1.21)
    mma$PECT     <- mma$PECT * runif(1,min=0.79,max=1.21)
    mma$SUPC    <- mma$SUPC * runif(1,min=0.79,max=1.21)
    mma$CB      <- mma$CB * runif(1,min=0.79,max=1.21)
    mma$BI      <- mma$BI * runif(1,min=0.79,max=1.21)
    mma$BR      <- mma$BR * runif(1,min=0.79,max=1.21)
    mma$HR      <- mma$HR * runif(1,min=0.79,max=1.21)
    mma$FDL     <- mma$FDL * runif(1,min=0.79,max=1.21)
    mma$FDL.U   <- mma$FDL.U * runif(1,min=0.79,max=1.21)
    mma$EDL     <- mma$EDL * runif(1,min=0.79,max=1.21)
    mma$FCU     <- mma$FCU * runif(1,min=0.79,max=1.21)
    mma$FCR     <- mma$FCR * runif(1,min=0.79,max=1.21)
    mma$ECU     <- mma$ECU * runif(1,min=0.79,max=1.21)
    mma$ECR     <- mma$ECR * runif(1,min=0.79,max=1.21)
    mma$SUP     <- mma$SUP * runif(1,min=0.79,max=1.21)
    mma$PT      <- mma$PT * runif(1,min=0.79,max=1.21)
    mma$PQ      <- mma$PQ * runif(1,min=0.79,max=1.21)
  
    mma$ADD     <- mma$ADD * runif(1,min=0.79,max=1.21)
    mma$AMB     <- mma$AMB * runif(1,min=0.79,max=1.21)
    mma$CFB     <- mma$CFB * runif(1,min=0.79,max=1.21)
    mma$FMTE    <- mma$FMTE * runif(1,min=0.79,max=1.21)
    mma$FMTI    <- mma$FMTI * runif(1,min=0.79,max=1.21)
    mma$FTE     <- mma$FTE * runif(1,min=0.79,max=1.21)
    mma$FTI.Fan <- mma$FTI.Fan * runif(1,min=0.79,max=1.21)
    mma$FTI2    <- mma$FTI2 * runif(1,min=0.79,max=1.21)
    mma$IFM     <- mma$IFM * runif(1,min=0.79,max=1.21)
    mma$ILF     <- mma$ILF * runif(1,min=0.79,max=1.21)
    mma$ITB     <- mma$ITB * runif(1,min=0.79,max=1.21)
    mma$PIFE    <- mma$PIFE * runif(1,min=0.79,max=1.21)
    mma$PIFI    <- mma$PIFI * runif(1,min=0.79,max=1.21)

        # new sum
    mma$SUM <- rowSums(mma[2:ncol(mma_raw)]);
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
  # Standard deviation
  stdev <- sapply(1:length(RSraw), function(a) {
    by.row <- sapply(1:nrow(RSraw[[1]]), function(b){
      out <- sd(RSraw[[a]][b,])
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
  stdev.out <- cbind(mma[,1], stdev)
  colnames(stdev.out) <- colnames(mma)
  ## export
  saveRDS(raw, paste0(name.input, "_raw_replicates.Rds"))
  write.csv(mean.out, paste0(name.input, "_mean.csv"))
  write.csv(quart0.25.out, paste0(name.input, "_quart0.25.csv"))
  write.csv(quart0.5.out, paste0(name.input, "_quart0.5.csv"))
  write.csv(quart0.75.out, paste0(name.input, "_quart0.75.csv"))
  write.csv(stdev.out, paste0(name.input, "_stdev.csv"))
}

#### Run the function ####
MMA.MC.loop(mma = mma_raw, n = 1000, name.input = "Fmax1_Median_MMA_MC")
