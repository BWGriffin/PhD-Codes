# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/MMA");

#Load and Setup#
{
  # read in raw data
  {
    mma_raw <- read.csv("Quad/WP1 ABAD Quad.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
    mma_raw2 <- read.csv("Quad/WP1 LAR Quad.csv", header = TRUE, sep = ",", quote =
                           "\"",dec = ".", fill = TRUE, comment.char = "")
    mma_raw3 <- read.csv("Quad/WP1 FE Quad.csv", header = TRUE, sep = ",", quote =
                           "\"",dec = ".", fill = TRUE, comment.char = "")
    
    mma_rawbi <- read.csv("Biped/WP1 ABAD Biped.csv", header = TRUE, sep = ",", quote =
                            "\"",dec = ".", fill = TRUE, comment.char = "")
    mma_rawbi2 <- read.csv("Biped/WP1 LAR Biped.csv", header = TRUE, sep = ",", quote =
                             "\"",dec = ".", fill = TRUE, comment.char = "")
    mma_rawbi3 <- read.csv("Biped/WP1 FE Biped.csv", header = TRUE, sep = ",", quote =
                             "\"",dec = ".", fill = TRUE, comment.char = "")
    
    mma_rawbu <- read.csv("Burst/WP1 ABAD Burst.csv", header = TRUE, sep = ",", quote =
                            "\"",dec = ".", fill = TRUE, comment.char = "")
    mma_rawbu2 <- read.csv("Burst/WP1 LAR Burst.csv", header = TRUE, sep = ",", quote =
                             "\"",dec = ".", fill = TRUE, comment.char = "")
    mma_rawbu3 <- read.csv("Burst/WP1 FE Burst.csv", header = TRUE, sep = ",", quote =
                             "\"",dec = ".", fill = TRUE, comment.char = "")
  }
  
  fmax_raw <- read.csv("Fmax/mean_m1.csv", header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_raw2 <- read.csv("Fmax/median_m1.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_raw3 <- read.csv("Fmax/mean_m2.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_raw4 <- read.csv("Fmax/median_m2.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_raw5 <- read.csv("Fmax/mean_m3.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_raw6 <- read.csv("Fmax/median_m3.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  
  fmax_b_raw <- read.csv("fmax/mean_m1_bird.csv", header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_b_raw2 <- read.csv("fmax/median_m1_bird.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_b_raw3 <- read.csv("fmax/mean_m2_bird.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_b_raw4 <- read.csv("fmax/median_m2_bird.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_b_raw5 <- read.csv("fmax/mean_m3_bird.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_b_raw6 <- read.csv("fmax/median_m3_bird.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  
  fmax_c_raw <- read.csv("Fmax/mean_m1_croc.csv", header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_c_raw2 <- read.csv("Fmax/median_m1_croc.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_c_raw3 <- read.csv("Fmax/mean_m2_croc.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_c_raw4 <- read.csv("Fmax/median_m2_croc.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_c_raw5 <- read.csv("Fmax/mean_m3_croc.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  fmax_c_raw6 <- read.csv("Fmax/median_m3_croc.csv", header = TRUE, sep = ",", quote =
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
#    mma$SC      <- (mma$SC * runif(1,min=0.79,max=1.21)) * fmax$SC            #
#    mma$LD      <- (mma$LD * runif(1,min=0.79,max=1.21)) * fmax$LD            #s
#    mma$TM      <- (mma$TM * runif(1,min=0.79,max=1.21)) * fmax$TM            #s
#    mma$DS      <- (mma$DS * runif(1,min=0.79,max=1.21)) * fmax$DS            #s
#    mma$SHA     <- (mma$SHA * runif(1,min=0.79,max=1.21)) * fmax$SHA          #s
#    mma$SHP     <- (mma$SHP * runif(1,min=0.79,max=1.21)) * fmax$SHP          #s
#    mma$SUBS    <- (mma$SUBS * runif(1,min=0.79,max=1.21)) * fmax$SUBS        #s
#    mma$TR.S    <- (mma$TR.S * runif(1,min=0.79,max=1.21)) * fmax$TR.S        #s,e
#    mma$TR.C    <- (mma$TR.C * runif(1,min=0.79,max=1.21)) * fmax$TR.C        #s,e
#    mma$TR.M    <- (mma$TR.M * runif(1,min=0.79,max=1.21)) * fmax$TR.M        # ,e
#    mma$TR.L    <- (mma$TR.L * runif(1,min=0.79,max=1.21)) * fmax$TR.L        # ,e
#    mma$PECT    <- (mma$PECT * runif(1,min=0.79,max=1.21)) * fmax$PECT        #s
#    mma$SUPC    <- (mma$SUPC * runif(1,min=0.79,max=1.21)) * fmax$SUPC        #s
#    mma$CB      <- (mma$CB * runif(1,min=0.79,max=1.21)) * fmax$CB            #s
#    mma$BI      <- (mma$BI * runif(1,min=0.79,max=1.21))  * fmax$BI           # ,e
#    mma$BR      <- (mma$BR * runif(1,min=0.79,max=1.21)) * fmax$BR            # ,e
#    mma$HR      <- (mma$HR * runif(1,min=0.79,max=1.21)) * fmax$HR            # ,e
    mma$FDL     <- (mma$FDL * runif(1,min=0.79,max=1.21)) * fmax$FDL          # ,e,wr,wm,wp
    mma$FDL.U   <- (mma$FDL.U * runif(1,min=0.79,max=1.21)) * fmax$FDL.U      # , ,wr,wm,wp
    mma$EDL     <- (mma$EDL * runif(1,min=0.79,max=1.21)) * fmax$EDL          # ,e,wr,wm,wp
#    mma$FCU     <- (mma$FCU * runif(1,min=0.79,max=1.21)) * fmax$FCU          # ,e,wr,wm
#    mma$FCR     <- (mma$FCR * runif(1,min=0.79,max=1.21)) * fmax$FCR          # ,e,wr
#    mma$ECU     <- (mma$ECU * runif(1,min=0.79,max=1.21)) * fmax$ECU          # ,e,wr,wm
#    mma$ECR     <- (mma$ECR * runif(1,min=0.79,max=1.21)) * fmax$ECR          # ,e,wr
#    mma$SUP     <- (mma$SUP * runif(1,min=0.79,max=1.21)) * fmax$SUP          # ,e
#    mma$PT      <- (mma$PT * runif(1,min=0.79,max=1.21)) * fmax$PT            # ,e
#    mma$PQ      <- (mma$PQ * runif(1,min=0.79,max=1.21)) * fmax$PQ
    
#    mma$ADD     <- (mma$ADD * runif(1,min=0.79,max=1.21)) * fmax$ADD          #h
#    mma$AMB     <- (mma$AMB * runif(1,min=0.79,max=1.21)) * fmax$AMB          #h,k
#    mma$CFB     <- (mma$CFB * runif(1,min=0.79,max=1.21)) * fmax$CFB          #h
#    mma$FMTE    <- (mma$FMTE * runif(1,min=0.79,max=1.21)) * fmax$FMTE        # ,K
#    mma$FMTI    <- (mma$FMTI * runif(1,min=0.79,max=1.21)) * fmax$FMTI        # ,k
#    mma$FTE     <- (mma$FTE * runif(1,min=0.79,max=1.21)) * fmax$FTE          #h,k
#    mma$FTI.Fan <- (mma$FTI.Fan * runif(1,min=0.79,max=1.21)) * fmax$FTI.Fan  #h,k
#    mma$FTI2    <- (mma$FTI2 * runif(1,min=0.79,max=1.21)) * fmax$FTI2        #h,k
#    mma$IFM     <- (mma$IFM * runif(1,min=0.79,max=1.21)) * fmax$IFM          #h
#    mma$ILF     <- (mma$ILF * runif(1,min=0.79,max=1.21)) * fmax$ILF          #h,k
#    mma$ITB     <- (mma$ITB * runif(1,min=0.79,max=1.21)) * fmax$ITB          #h,k
#    mma$PIFE    <- (mma$PIFE * runif(1,min=0.79,max=1.21)) * fmax$PIFE        #h
#    mma$PIFI    <- (mma$PIFI * runif(1,min=0.79,max=1.21)) * fmax$PIFI        #h
    
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
## mean/median fmax ##
{
{
  MMA.MC.loop(mma = mma_raw, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_m1_MC")
  MMA.MC.loop(mma = mma_raw2, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_m1_MC")
  MMA.MC.loop(mma = mma_raw3, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_m1_MC")
  
  MMA.MC.loop(mma = mma_rawbi, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_m1_MC")
  MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_m1_MC")
  MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_m1_MC")
  
  MMA.MC.loop(mma = mma_rawbu, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_m1_MC")
  MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_m1_MC")
  MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_raw, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_m1_MC")
} #m1

{
  MMA.MC.loop(mma = mma_raw, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_md1_MC")
  MMA.MC.loop(mma = mma_raw2, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_md1_MC")
  MMA.MC.loop(mma = mma_raw3, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_md1_MC")
  
  MMA.MC.loop(mma = mma_rawbi, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_md1_MC")
  MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_md1_MC")
  MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_md1_MC")
  
  MMA.MC.loop(mma = mma_rawbu, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_md1_MC")
  MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_md1_MC")
  MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_raw2, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_md1_MC")
} #md1

{
  MMA.MC.loop(mma = mma_raw, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_m2_MC")
  MMA.MC.loop(mma = mma_raw2, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_m2_MC")
  MMA.MC.loop(mma = mma_raw3, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_m2_MC")
  
  MMA.MC.loop(mma = mma_rawbi, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_m2_MC")
  MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_m2_MC")
  MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_m2_MC")
  
  MMA.MC.loop(mma = mma_rawbu, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_m2_MC")
  MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_m2_MC")
  MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_raw3, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_m2_MC")
} #m2

{
  MMA.MC.loop(mma = mma_raw, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_md2_MC")
  MMA.MC.loop(mma = mma_raw2, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_md2_MC")
  MMA.MC.loop(mma = mma_raw3, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_md2_MC")
  
  MMA.MC.loop(mma = mma_rawbi, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_md2_MC")
  MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_md2_MC")
  MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_md2_MC")
  
  MMA.MC.loop(mma = mma_rawbu, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_md2_MC")
  MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_md2_MC")
  MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_raw4, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_md2_MC")
} #md2

{
  MMA.MC.loop(mma = mma_raw, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_m3_MC")
  MMA.MC.loop(mma = mma_raw2, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_m3_MC")
  MMA.MC.loop(mma = mma_raw3, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_m3_MC")
  
  MMA.MC.loop(mma = mma_rawbi, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_m3_MC")
  MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_m3_MC")
  MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_m3_MC")
  
  MMA.MC.loop(mma = mma_rawbu, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_m3_MC")
  MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_m3_MC")
  MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_raw5, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_m3_MC")
} #m3

{
  MMA.MC.loop(mma = mma_raw, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_md3_MC")
  MMA.MC.loop(mma = mma_raw2, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_md3_MC")
  MMA.MC.loop(mma = mma_raw3, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_md3_MC")
  
  MMA.MC.loop(mma = mma_rawbi, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_md3_MC")
  MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_md3_MC")
  MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_md3_MC")
  
  MMA.MC.loop(mma = mma_rawbu, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_md3_MC")
  MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_md3_MC")
  MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_raw6, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_md3_MC")
} #md3
} #averaged fmax
## bird fmax ##
{
  {
    MMA.MC.loop(mma = mma_raw, fmax = fmax_b_raw, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_m1b_MC")
    MMA.MC.loop(mma = mma_raw2, fmax = fmax_b_raw, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_m1b_MC")
    MMA.MC.loop(mma = mma_raw3, fmax = fmax_b_raw, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_m1b_MC")
    
    MMA.MC.loop(mma = mma_rawbi, fmax = fmax_b_raw, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_m1b_MC")
    MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_b_raw, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_m1b_MC")
    MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_b_raw, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_m1b_MC")
    
    MMA.MC.loop(mma = mma_rawbu, fmax = fmax_b_raw, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_m1b_MC")
    MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_b_raw, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_m1b_MC")
    MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_b_raw, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_m1b_MC")
  } #m1
  
  {
    MMA.MC.loop(mma = mma_raw, fmax = fmax_b_raw2, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_md1b_MC")
    MMA.MC.loop(mma = mma_raw2, fmax = fmax_b_raw2, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_md1b_MC")
    MMA.MC.loop(mma = mma_raw3, fmax = fmax_b_raw2, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_md1b_MC")
    
    MMA.MC.loop(mma = mma_rawbi, fmax = fmax_b_raw2, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_md1b_MC")
    MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_b_raw2, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_md1b_MC")
    MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_b_raw2, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_md1b_MC")
    
    MMA.MC.loop(mma = mma_rawbu, fmax = fmax_b_raw2, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_md1b_MC")
    MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_b_raw2, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_md1b_MC")
    MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_b_raw2, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_md1b_MC")
  } #md1
  
  {
    MMA.MC.loop(mma = mma_raw, fmax = fmax_b_raw3, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_m2b_MC")
    MMA.MC.loop(mma = mma_raw2, fmax = fmax_b_raw3, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_m2b_MC")
    MMA.MC.loop(mma = mma_raw3, fmax = fmax_b_raw3, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_m2b_MC")
    
    MMA.MC.loop(mma = mma_rawbi, fmax = fmax_b_raw3, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_m2b_MC")
    MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_b_raw3, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_m2b_MC")
    MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_b_raw3, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_m2b_MC")
    
    MMA.MC.loop(mma = mma_rawbu, fmax = fmax_b_raw3, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_m2b_MC")
    MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_b_raw3, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_m2b_MC")
    MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_b_raw3, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_m2b_MC")
  } #m2
  
  {
    MMA.MC.loop(mma = mma_raw, fmax = fmax_b_raw4, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_md2b_MC")
    MMA.MC.loop(mma = mma_raw2, fmax = fmax_b_raw4, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_md2b_MC")
    MMA.MC.loop(mma = mma_raw3, fmax = fmax_b_raw4, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_md2b_MC")
    
    MMA.MC.loop(mma = mma_rawbi, fmax = fmax_b_raw4, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_md2b_MC")
    MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_b_raw4, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_md2b_MC")
    MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_b_raw4, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_md2b_MC")
    
    MMA.MC.loop(mma = mma_rawbu, fmax = fmax_b_raw4, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_md2b_MC")
    MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_b_raw4, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_md2b_MC")
    MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_b_raw4, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_md2b_MC")
  } #md2
  
  {
    MMA.MC.loop(mma = mma_raw, fmax = fmax_b_raw5, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_m3b_MC")
    MMA.MC.loop(mma = mma_raw2, fmax = fmax_b_raw5, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_m3b_MC")
    MMA.MC.loop(mma = mma_raw3, fmax = fmax_b_raw5, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_m3b_MC")
    
    MMA.MC.loop(mma = mma_rawbi, fmax = fmax_b_raw5, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_m3b_MC")
    MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_b_raw5, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_m3b_MC")
    MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_b_raw5, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_m3b_MC")
    
    MMA.MC.loop(mma = mma_rawbu, fmax = fmax_b_raw5, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_m3b_MC")
    MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_b_raw5, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_m3b_MC")
    MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_b_raw5, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_m3b_MC")
  } #m3
  
  {
    MMA.MC.loop(mma = mma_raw, fmax = fmax_b_raw6, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_md3b_MC")
    MMA.MC.loop(mma = mma_raw2, fmax = fmax_b_raw6, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_md3b_MC")
    MMA.MC.loop(mma = mma_raw3, fmax = fmax_b_raw6, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_md3b_MC")
    
    MMA.MC.loop(mma = mma_rawbi, fmax = fmax_b_raw6, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_md3b_MC")
    MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_b_raw6, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_md3b_MC")
    MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_b_raw6, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_md3b_MC")
    
    MMA.MC.loop(mma = mma_rawbu, fmax = fmax_b_raw6, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_md3b_MC")
    MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_b_raw6, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_md3b_MC")
    MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_b_raw6, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_md3b_MC")
  } #md3
} #bird fmax
## croc fmax ##
{
  {
    MMA.MC.loop(mma = mma_raw, fmax = fmax_c_raw, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_m1c_MC")
    MMA.MC.loop(mma = mma_raw2, fmax = fmax_c_raw, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_m1c_MC")
    MMA.MC.loop(mma = mma_raw3, fmax = fmax_c_raw, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_m1c_MC")
    
    MMA.MC.loop(mma = mma_rawbi, fmax = fmax_c_raw, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_m1c_MC")
    MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_c_raw, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_m1c_MC")
    MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_c_raw, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_m1c_MC")
    
    MMA.MC.loop(mma = mma_rawbu, fmax = fmax_c_raw, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_m1c_MC")
    MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_c_raw, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_m1c_MC")
    MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_c_raw, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_m1c_MC")
  } #m1
  
  {
    MMA.MC.loop(mma = mma_raw, fmax = fmax_c_raw2, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_md1c_MC")
    MMA.MC.loop(mma = mma_raw2, fmax = fmax_c_raw2, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_md1c_MC")
    MMA.MC.loop(mma = mma_raw3, fmax = fmax_c_raw2, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_md1c_MC")
    
    MMA.MC.loop(mma = mma_rawbi, fmax = fmax_c_raw2, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_md1c_MC")
    MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_c_raw2, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_md1c_MC")
    MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_c_raw2, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_md1c_MC")
    
    MMA.MC.loop(mma = mma_rawbu, fmax = fmax_c_raw2, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_md1c_MC")
    MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_c_raw2, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_md1c_MC")
    MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_c_raw2, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_md1c_MC")
  } #md1
  
  {
    MMA.MC.loop(mma = mma_raw, fmax = fmax_c_raw3, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_m2c_MC")
    MMA.MC.loop(mma = mma_raw2, fmax = fmax_c_raw3, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_m2c_MC")
    MMA.MC.loop(mma = mma_raw3, fmax = fmax_c_raw3, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_m2c_MC")
    
    MMA.MC.loop(mma = mma_rawbi, fmax = fmax_c_raw3, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_m2c_MC")
    MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_c_raw3, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_m2c_MC")
    MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_c_raw3, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_m2c_MC")
    
    MMA.MC.loop(mma = mma_rawbu, fmax = fmax_c_raw3, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_m2c_MC")
    MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_c_raw3, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_m2c_MC")
    MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_c_raw3, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_m2c_MC")
  } #m2
  
  {
    MMA.MC.loop(mma = mma_raw, fmax = fmax_c_raw4, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_md2c_MC")
    MMA.MC.loop(mma = mma_raw2, fmax = fmax_c_raw4, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_md2c_MC")
    MMA.MC.loop(mma = mma_raw3, fmax = fmax_c_raw4, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_md2c_MC")
    
    MMA.MC.loop(mma = mma_rawbi, fmax = fmax_c_raw4, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_md2c_MC")
    MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_c_raw4, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_md2c_MC")
    MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_c_raw4, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_md2c_MC")
    
    MMA.MC.loop(mma = mma_rawbu, fmax = fmax_c_raw4, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_md2c_MC")
    MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_c_raw4, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_md2c_MC")
    MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_c_raw4, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_md2c_MC")
  } #md2
  
  {
    MMA.MC.loop(mma = mma_raw, fmax = fmax_c_raw5, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_m3c_MC")
    MMA.MC.loop(mma = mma_raw2, fmax = fmax_c_raw5, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_m3c_MC")
    MMA.MC.loop(mma = mma_raw3, fmax = fmax_c_raw5, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_m3c_MC")
    
    MMA.MC.loop(mma = mma_rawbi, fmax = fmax_c_raw5, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_m3c_MC")
    MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_c_raw5, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_m3c_MC")
    MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_c_raw5, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_m3c_MC")
    
    MMA.MC.loop(mma = mma_rawbu, fmax = fmax_c_raw5, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_m3c_MC")
    MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_c_raw5, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_m3c_MC")
    MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_c_raw5, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_m3c_MC")
  } #m3
  
  {
    MMA.MC.loop(mma = mma_raw, fmax = fmax_c_raw6, n= 1000, name.input = "MM/0_73/Quad/WP1_ABAD_MM1_md3c_MC")
    MMA.MC.loop(mma = mma_raw2, fmax = fmax_c_raw6, n= 1000, name.input = "MM/0_73/Quad/WP1_LAR_MM1_md3c_MC")
    MMA.MC.loop(mma = mma_raw3, fmax = fmax_c_raw6, n= 1000, name.input = "MM/0_73/Quad/WP1_FE_MM1_md3c_MC")
    
    MMA.MC.loop(mma = mma_rawbi, fmax = fmax_c_raw6, n= 1000, name.input = "MM/0_73/Biped/WP1_ABAD_MM1_md3c_MC")
    MMA.MC.loop(mma = mma_rawbi2, fmax = fmax_c_raw6, n= 1000, name.input = "MM/0_73/Biped/WP1_LAR_MM1_md3c_MC")
    MMA.MC.loop(mma = mma_rawbi3, fmax = fmax_c_raw6, n= 1000, name.input = "MM/0_73/Biped/WP1_FE_MM1_md3c_MC")
    
    MMA.MC.loop(mma = mma_rawbu, fmax = fmax_c_raw6, n= 1000, name.input = "MM/0_73/Burst/WP1_ABAD_MM1_md3c_MC")
    MMA.MC.loop(mma = mma_rawbu2, fmax = fmax_c_raw6, n= 1000, name.input = "MM/0_73/Burst/WP1_LAR_MM1_md3c_MC")
    MMA.MC.loop(mma = mma_rawbu3, fmax = fmax_c_raw6, n= 1000, name.input = "MM/0_73/Burst/WP1_FE_MM1_md3c_MC")
  } #md3
} #croc fmax

#Done. stop goofing off and run another