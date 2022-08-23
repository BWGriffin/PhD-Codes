# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/MMA");

#Load and Setup#
{
  # read in raw data
  mma_raw <- read.csv("Hip ABAD burst.csv", header = TRUE, sep = ",", quote = 
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  # create SUM column
  mma_raw$SUM <- rowSums(mma_raw[2:12]);
}

# create duplicate and replace data need to do this 1000 times
{
  # duplicate data set from the raw data
  mma <- mma_raw
  # replace each column with base * uniform random number between 0.8 and 1.2
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
}

