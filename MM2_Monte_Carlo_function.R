# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/MMA");

#Load and Setup#
{
  # read in raw data
  {
  mma_raw <- read.csv("Quad/Elbow ABAD Quad.csv", header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  mma_raw2 <- read.csv("Quad/Elbow LAR Quad.csv", header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  mma_raw3 <- read.csv("Quad/Elbow FE Quad.csv", header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  
  mma_rawbi <- read.csv("Biped/Elbow ABAD Biped.csv", header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  mma_rawbi2 <- read.csv("Biped/Elbow LAR Biped.csv", header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  mma_rawbi3 <- read.csv("Biped/Elbow FE Biped.csv", header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  
  mma_rawbu <- read.csv("Burst/Elbow ABAD Burst.csv", header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  mma_rawbu2 <- read.csv("Burst/Elbow LAR Burst.csv", header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  mma_rawbu3 <- read.csv("Burst/Elbow FE Burst.csv", header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  }
  #change to _67.csv to do isometric
  fmax_raw <- read.csv("Fmax/mean_m1_bird.csv", header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_raw2 <- read.csv("Fmax/median_m1_bird.csv", header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_raw3 <- read.csv("Fmax/mean_m2_bird.csv", header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_raw4 <- read.csv("Fmax/median_m2_bird.csv", header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_raw5 <- read.csv("Fmax/mean_m3_bird.csv", header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_raw6 <- read.csv("Fmax/median_m3_bird.csv", header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  
  # create SUM column
  mma_raw$SUM <- rowSums(mma_raw[2:ncol(mma_raw)]);
  mma_raw2$SUM <- rowSums(mma_raw2[2:ncol(mma_raw2)]);
  mma_raw3$SUM <- rowSums(mma_raw3[2:ncol(mma_raw3)]);
  
  mma_rawbi$SUM <- rowSums(mma_rawbi[2:ncol(mma_rawbi)]);
  mma_rawbi2$SUM <- rowSums(mma_rawbi2[2:ncol(mma_rawbi2)]);
  mma_rawbi3$SUM <- rowSums(mma_rawbi3[2:ncol(mma_rawbi3)]);
  
  mma_rawbu$SUM <- rowSums(mma_rawbu[2:ncol(mma_rawbu)]);
  mma_rawbu2$SUM <- rowSums(mma_rawbu2[2:ncol(mma_rawbu2)]);
  mma_rawbu3$SUM <- rowSums(mma_rawbu3[2:ncol(mma_rawbu3)]);
}

#### Function to iterate second half of code n times ####
MMA.MC.loop <- function(mma, fmax, n, name.input) {
  # get list of n length of modified matrices
  raw <- lapply(1:n, function(all) {
#    mma$SC      <- (mma$SC * (fmax$SC * runif(1,min=0.79,max=1.21)))           #
#    mma$LD      <- (mma$LD * (fmax$LD * runif(1,min=0.79,max=1.21)))            #s
#    mma$TM      <- (mma$TM * (fmax$TM * runif(1,min=0.79,max=1.21)))            #s
#    mma$DS      <- (mma$DS * (fmax$DS * runif(1,min=0.79,max=1.21)))            #s
#    mma$SHA     <- (mma$SHA * (fmax$SHA * runif(1,min=0.79,max=1.21)))          #s
#    mma$SHP     <- (mma$SHP * (fmax$SHP * runif(1,min=0.79,max=1.21)))          #s
#    mma$SUBS    <- (mma$SUBS * (fmax$SUBS * runif(1,min=0.79,max=1.21)))        #s
    mma$TR.S    <- (mma$TR.S * (fmax$TR.S * runif(1,min=0.79,max=1.21)))        #s,e
    mma$TR.C    <- (mma$TR.C * (fmax$TR.C * runif(1,min=0.79,max=1.21)))        #s,e
    mma$TR.M    <- (mma$TR.M * (fmax$TR.M * runif(1,min=0.79,max=1.21)))        # ,e
    mma$TR.L    <- (mma$TR.L * (fmax$TR.L * runif(1,min=0.79,max=1.21)))        # ,e
#    mma$PECT    <- (mma$PECT * (fmax$PECT * runif(1,min=0.79,max=1.21)))        #s
#    mma$SUPC    <- (mma$SUPC * (fmax$SUPC * runif(1,min=0.79,max=1.21)))        #s
#    mma$CB      <- (mma$CB * (fmax$CB * runif(1,min=0.79,max=1.21)))            #s
    mma$BI      <- (mma$BI * (fmax$BI * runif(1,min=0.79,max=1.21)))            # ,e
    mma$BR      <- (mma$BR * (fmax$BR * runif(1,min=0.79,max=1.21)))            # ,e
    mma$HR      <- (mma$HR * (fmax$HR * runif(1,min=0.79,max=1.21)))            # ,e
    mma$FDL     <- (mma$FDL * (fmax$FDL * runif(1,min=0.79,max=1.21)))          # ,e,wr,wm,wp
#    mma$FDL.U   <- (mma$FDL.U * (fmax$FDL.U * runif(1,min=0.79,max=1.21)))      # , ,wr,wm,wp
    mma$EDL     <- (mma$EDL * (fmax$EDL * runif(1,min=0.79,max=1.21)))          # ,e,wr,wm,wp
    mma$FCU     <- (mma$FCU * (fmax$FCU * runif(1,min=0.79,max=1.21)))          # ,e,wr,wm
    mma$FCR     <- (mma$FCR * (fmax$FCR * runif(1,min=0.79,max=1.21)))          # ,e,wr
    mma$ECU     <- (mma$ECU * (fmax$ECU * runif(1,min=0.79,max=1.21)))          # ,e,wr,wm
    mma$ECR     <- (mma$ECR * (fmax$ECR * runif(1,min=0.79,max=1.21)))          # ,e,wr
    mma$SUP     <- (mma$SUP * (fmax$SUP * runif(1,min=0.79,max=1.21)))          # ,e
    mma$PT      <- (mma$PT * (fmax$PT * runif(1,min=0.79,max=1.21)))            # ,e
#    mma$PQ      <- (mma$PQ * (fmax$PQ * runif(1,min=0.79,max=1.21)))
    
#    mma$ADD     <- (mma$ADD * (fmax$ADD * runif(1,min=0.79,max=1.21)))          #h
#    mma$AMB     <- (mma$AMB * (fmax$AMB * runif(1,min=0.79,max=1.21)))          #h,k
#    mma$CFB     <- (mma$CFB * (fmax$CFB * runif(1,min=0.79,max=1.21)))          #h
#    mma$FMTE    <- (mma$FMTE * (fmax$FMTE * runif(1,min=0.79,max=1.21)))        # ,K
#    mma$FMTI    <- (mma$FMTI * (fmax$FMTI * runif(1,min=0.79,max=1.21)))        # ,k
#    mma$FTE     <- (mma$FTE * (fmax$FTE * runif(1,min=0.79,max=1.21)))          #h,k
#    mma$FTI.Fan <- (mma$FTI.Fan * (fmax$FTI.Fan * runif(1,min=0.79,max=1.21)))  #h,k
#    mma$FTI2    <- (mma$FTI2 * (fmax$FTI2 * runif(1,min=0.79,max=1.21)))        #h,k
#    mma$IFM     <- (mma$IFM * (fmax$IFM * runif(1,min=0.79,max=1.21)))          #h
#    mma$ILF     <- (mma$ILF * (fmax$ILF * runif(1,min=0.79,max=1.21)))          #h,k
#    mma$ITB     <- (mma$ITB * (fmax$ITB * runif(1,min=0.79,max=1.21)))          #h,k
#    mma$PIFE    <- (mma$PIFE * (fmax$PIFE * runif(1,min=0.79,max=1.21)))        #h
#    mma$PIFI    <- (mma$PIFI * (fmax$PIFI * runif(1,min=0.79,max=1.21)))        #h

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
{
MMA.MC.loop(mma = mma_raw, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Quad/Elbow_ABAD_MM2_m1_MC")
MMA.MC.loop(mma = mma_raw2, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Quad/Elbow_LAR_MM2_m1_MC")
MMA.MC.loop(mma = mma_raw3, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Quad/Elbow_FE_MM2_m1_MC")

MMA.MC.loop(mma = mma_rawbi, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Biped/Elbow_ABAD_MM2_m1_MC")
MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Biped/Elbow_LAR_MM2_m1_MC")
MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Biped/Elbow_FE_MM2_m1_MC")

MMA.MC.loop(mma = mma_rawbu, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Burst/Elbow_ABAD_MM2_m1_MC")
MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Burst/Elbow_LAR_MM2_m1_MC")
MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Burst/Elbow_FE_MM2_m1_MC")
} #m1

{
  MMA.MC.loop(mma = mma_raw, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Quad/Elbow_ABAD_MM2_md1_MC")
  MMA.MC.loop(mma = mma_raw2, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Quad/Elbow_LAR_MM2_md1_MC")
  MMA.MC.loop(mma = mma_raw3, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Quad/Elbow_FE_MM2_md1_MC")
  
  MMA.MC.loop(mma = mma_rawbi, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Biped/Elbow_ABAD_MM2_md1_MC")
  MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Biped/Elbow_LAR_MM2_md1_MC")
  MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Biped/Elbow_FE_MM2_md1_MC")
  
  MMA.MC.loop(mma = mma_rawbu, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Burst/Elbow_ABAD_MM2_md1_MC")
  MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Burst/Elbow_LAR_MM2_md1_MC")
  MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Burst/Elbow_FE_MM2_md1_MC")
} #md1

{
  MMA.MC.loop(mma = mma_raw, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Quad/Elbow_ABAD_MM2_m2_MC")
  MMA.MC.loop(mma = mma_raw2, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Quad/Elbow_LAR_MM2_m2_MC")
  MMA.MC.loop(mma = mma_raw3, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Quad/Elbow_FE_MM2_m2_MC")
  
  MMA.MC.loop(mma = mma_rawbi, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Biped/Elbow_ABAD_MM2_m2_MC")
  MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Biped/Elbow_LAR_MM2_m2_MC")
  MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Biped/Elbow_FE_MM2_m2_MC")
  
  MMA.MC.loop(mma = mma_rawbu, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Burst/Elbow_ABAD_MM2_m2_MC")
  MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Burst/Elbow_LAR_MM2_m2_MC")
  MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Burst/Elbow_FE_MM2_m2_MC")
} #m2

{
  MMA.MC.loop(mma = mma_raw, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Quad/Elbow_ABAD_MM2_md2_MC")
  MMA.MC.loop(mma = mma_raw2, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Quad/Elbow_LAR_MM2_md2_MC")
  MMA.MC.loop(mma = mma_raw3, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Quad/Elbow_FE_MM2_md2_MC")
  
  MMA.MC.loop(mma = mma_rawbi, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Biped/Elbow_ABAD_MM2_md2_MC")
  MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Biped/Elbow_LAR_MM2_md2_MC")
  MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Biped/Elbow_FE_MM2_md2_MC")
  
  MMA.MC.loop(mma = mma_rawbu, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Burst/Elbow_ABAD_MM2_md2_MC")
  MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Burst/Elbow_LAR_MM2_md2_MC")
  MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Burst/Elbow_FE_MM2_md2_MC")
} #md2

{
  MMA.MC.loop(mma = mma_raw, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Quad/Elbow_ABAD_MM2_m3_MC")
  MMA.MC.loop(mma = mma_raw2, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Quad/Elbow_LAR_MM2_m3_MC")
  MMA.MC.loop(mma = mma_raw3, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Quad/Elbow_FE_MM2_m3_MC")
  
  MMA.MC.loop(mma = mma_rawbi, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Biped/Elbow_ABAD_MM2_m3_MC")
  MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Biped/Elbow_LAR_MM2_m3_MC")
  MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Biped/Elbow_FE_MM2_m3_MC")
  
  MMA.MC.loop(mma = mma_rawbu, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Burst/Elbow_ABAD_MM2_m3_MC")
  MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Burst/Elbow_LAR_MM2_m3_MC")
  MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Burst/Elbow_FE_MM2_m3_MC")
} #m3

{
  MMA.MC.loop(mma = mma_raw, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Quad/Elbow_ABAD_MM2_md3_MC")
  MMA.MC.loop(mma = mma_raw2, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Quad/Elbow_LAR_MM2_md3_MC")
  MMA.MC.loop(mma = mma_raw3, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Quad/Elbow_FE_MM2_md3_MC")
  
  MMA.MC.loop(mma = mma_rawbi, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Biped/Elbow_ABAD_MM2_md3_MC")
  MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Biped/Elbow_LAR_MM2_md3_MC")
  MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Biped/Elbow_FE_MM2_md3_MC")
  
  MMA.MC.loop(mma = mma_rawbu, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Burst/Elbow_ABAD_MM2_md3_MC")
  MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Burst/Elbow_LAR_MM2_md3_MC")
  MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Burst/Elbow_FE_MM2_md3_MC")
} #md3
#Done. stop goofing off and run another