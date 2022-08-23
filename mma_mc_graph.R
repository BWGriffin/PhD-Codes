# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/MMA");
# to run this code use ctrl f to find and replace all of the instances of the joint name e.g. Elbow with another joint name e.g. Elbow
# will then have to manually correct for the columns
graph.loop <- function(j, df, ti)
{
##Load and Setup##
{

  buname <- (paste0("Burst/Outputs/"))
  biname <- (paste0("Biped/Outputs/"))
  quname <- (paste0("Quad/Outputs/"))
  buname2 <- (paste0(buname,j,df))
  biname2 <- (paste0(biname,j,df))
  quname2 <- (paste0(quname,j,df))
  
  # read in raw data

  mma_bu   <- read.csv((paste0(buname2,"_MMA_MC_mean.csv")), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  mma_bi   <- read.csv((paste0(biname2,"_MMA_MC_mean.csv")), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  mma_qu   <- read.csv((paste0(quname2,"_MMA_MC_mean.csv")), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  mma_sdbu <- read.csv((paste0(buname2,"_MMA_MC_stdev.csv")), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  mma_sdbi <- read.csv((paste0(biname2,"_MMA_MC_stdev.csv")), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  mma_sdqu <- read.csv((paste0(quname2,"_MMA_MC_stdev.csv")), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  zero <- read.csv("zero.csv", header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
#  Fmax_md <- read.csv("Fmax/Fmax_Median_MMA_MC_mean.csv", header = TRUE, sep = ",", quote =
#                    "\"",dec = ".", fill = TRUE, comment.char = "")
#  Fmax_mn <- read.csv("Fmax/Fmax_Mean_MMA_MC_mean.csv", header = TRUE, sep = ",", quote =
#                     "\"",dec = ".", fill = TRUE, comment.char = "")
  #make mean+/-sd
  mmap_bu <- mma_bu
  {

      mmap_bu$BI      <- mmap_bu$BI + mma_sdbu$BI               # ,e
      mmap_bu$BR      <- mmap_bu$BR + mma_sdbu$BR               # ,e
  #    mmap_bu$CB      <- mmap_bu$CB + mma_sdbu$CB               #s
  #    mmap_bu$DS      <- mmap_bu$DS + mma_sdbu$DS               #s
      mmap_bu$ECR     <- mmap_bu$ECR + mma_sdbu$ECR             # ,e,wr
      mmap_bu$ECU     <- mmap_bu$ECU + mma_sdbu$ECU             # ,e,wr,wm
      mmap_bu$EDL     <- mmap_bu$EDL + mma_sdbu$EDL             # ,e,wr,wm,wp
      mmap_bu$FCR     <- mmap_bu$FCR + mma_sdbu$FCR             # ,e,wr
      mmap_bu$FCU     <- mmap_bu$FCU + mma_sdbu$FCU             # ,e,wr,wm
      mmap_bu$FDL     <- mmap_bu$FDL + mma_sdbu$FDL             # ,e,wr,wm,wp
  #    mmap_bu$FDL.U   <- mmap_bu$FDL.U + mma_sdbu$FDL.U         # , ,wr,wm,wp
      mmap_bu$HR      <- mmap_bu$HR + mma_sdbu$HR               # ,e
  #    mmap_bu$LD      <- mmap_bu$LD + mma_sdbu$LD               #s
  #    mmap_bu$PECT     <- mmap_bu$PEC + mma_sdbu$PEC            #s
      mmap_bu$PT      <- mmap_bu$PT + mma_sdbu$PT               # ,e
  #    mmap_bu$SHA     <- mmap_bu$SHA + mma_sdbu$SHA             #s
  #    mmap_bu$SHP     <- mmap_bu$SHP + mma_sdbu$SHP             #s
  #    mmap_bu$SUBS    <- mmap_bu$SUBS + mma_sdbu$SUBS           #s
      mmap_bu$SUP     <- mmap_bu$SUP + mma_sdbu$SUP             # ,e
  #    mmap_bu$SUPC    <- mmap_bu$SUPC + mma_sdbu$SUPC           #s
  #    mmap_bu$TM      <- mmap_bu$TM + mma_sdbu$TM               #s
      mmap_bu$TR.S    <- mmap_bu$TR.S + mma_sdbu$TR.S           #s,e
      mmap_bu$TR.C    <- mmap_bu$TR.C + mma_sdbu$TR.C           #s,e
      mmap_bu$TR.M    <- mmap_bu$TR.M + mma_sdbu$TR.M           # ,e
      mmap_bu$TR.L    <- mmap_bu$TR.L + mma_sdbu$TR.L           # ,e

  #    mmap_bu$ADD     <- mmap_bu$ADD + mma_sdbu$ADD              #h
  #    mmap_bu$AMB     <- mmap_bu$AMB + mma_sdbu$AMB              #h,k
  #    mmap_bu$CFB     <- mmap_bu$CFB + mma_sdbu$CFB              #h
  #    mmap_bu$FMTE    <- mmap_bu$FMTE + mma_sdbu$FMTE           # ,k
  #    mmap_bu$FMTI    <- mmap_bu$FMTI + mma_sdbu$FMTI           # ,k
  #    mmap_bu$FTE     <- mmap_bu$FTE + mma_sdbu$FTE              #h,k
  #    mmap_bu$FTI.Fan <- mmap_bu$FTI.Fan + mma_sdbu$FTI.Fan      #h,k
  #    mmap_bu$FTI2    <- mmap_bu$FTI2 + mma_sdbu$FTI2            #h,k
  #    mmap_bu$IFM     <- mmap_bu$IFM + mma_sdbu$IFM              #h
  #    mmap_bu$ILF     <- mmap_bu$ILF + mma_sdbu$ILF              #h,k
  #    mmap_bu$ITB     <- mmap_bu$ITB + mma_sdbu$ITB              #h,k
  #    mmap_bu$PIFE    <- mmap_bu$PIFE + mma_sdbu$PIFE            #h
  #    mmap_bu$PIFI    <- mmap_bu$PIFI + mma_sdbu$PIFI            #h
      # new sum
      mmap_bu$SUM <- mmap_bu$SUM + mma_sdbu$SUM;
  }
  mmam_bu <- mma_bu
  {
    
        mmam_bu$BI      <- mmam_bu$BI - mma_sdbu$BI
        mmam_bu$BR      <- mmam_bu$BR - mma_sdbu$BR
    #    mmam_bu$CB      <- mmam_bu$CB - mma_sdbu$CB
    #    mmam_bu$DS      <- mmam_bu$DS - mma_sdbu$DS
        mmam_bu$ECR     <- mmam_bu$ECR - mma_sdbu$ECR
        mmam_bu$ECU     <- mmam_bu$ECU - mma_sdbu$ECU
        mmam_bu$EDL     <- mmam_bu$EDL - mma_sdbu$EDL
        mmam_bu$FCR     <- mmam_bu$FCR - mma_sdbu$FCR
        mmam_bu$FCU     <- mmam_bu$FCU - mma_sdbu$FCU
        mmam_bu$FDL     <- mmam_bu$FDL - mma_sdbu$FDL
    #    mmam_bu$FDL.U   <- mmam_bu$FDL.U - mma_sdbu$FDL.U
        mmam_bu$HR      <- mmam_bu$HR - mma_sdbu$HR
    #    mmam_bu$LD      <- mmam_bu$LD - mma_sdbu$LD
    #    mmam_bu$PECT     <- mmam_bu$PEC - mma_sdbu$PEC
        mmam_bu$PT      <- mmam_bu$PT - mma_sdbu$PT
    #    mmam_bu$SHA     <- mmam_bu$SHA - mma_sdbu$SHA
    #    mmam_bu$SHP     <- mmam_bu$SHP - mma_sdbu$SHP
    #    mmam_bu$SUBS    <- mmam_bu$SUBS - mma_sdbu$SUBS
        mmam_bu$SUP     <- mmam_bu$SUP - mma_sdbu$SUP
    #    mmam_bu$SUPC    <- mmam_bu$SUPC - mma_sdbu$SUPC
    #    mmam_bu$TM      <- mmam_bu$TM - mma_sdbu$TM
        mmam_bu$TR.S    <- mmam_bu$TR.S - mma_sdbu$TR.S
        mmam_bu$TR.C    <- mmam_bu$TR.C - mma_sdbu$TR.C
        mmam_bu$TR.M    <- mmam_bu$TR.M - mma_sdbu$TR.M
        mmam_bu$TR.L    <- mmam_bu$TR.L - mma_sdbu$TR.L
    
    #    mmam_bu$ADD     <- mmam_bu$ADD - mma_sdbu$ADD
    #    mmam_bu$AMB     <- mmam_bu$AMB - mma_sdbu$AMB
    #    mmam_bu$CFB     <- mmam_bu$CFB - mma_sdbu$CFB
    #    mmam_bu$FMTE    <- mmam_bu$FMTE - mma_sdbu$FMTE
    #    mmam_bu$FMTI    <- mmam_bu$FMTI - mma_sdbu$FMTI
    #    mmam_bu$FTE     <- mmam_bu$FTE - mma_sdbu$FTE
    #    mmam_bu$FTI.Fan <- mmam_bu$FTI.Fan - mma_sdbu$FTI.Fan
    #    mmam_bu$FTI2    <- mmam_bu$FTI2 - mma_sdbu$FTI2
    #    mmam_bu$IFM     <- mmam_bu$IFM - mma_sdbu$IFM
    #    mmam_bu$ILF     <- mmam_bu$ILF - mma_sdbu$ILF
    #    mmam_bu$ITB     <- mmam_bu$ITB - mma_sdbu$ITB
    #    mmam_bu$PIFE    <- mmam_bu$PIFE - mma_sdbu$PIFE
    #    mmam_bu$PIFI    <- mmam_bu$PIFI - mma_sdbu$PIFI
    #new sum
    mmam_bu$SUM <- mmam_bu$SUM - mma_sdbu$SUM;
  }
  mmap_bi <- mma_bi
  {
    
        mmap_bi$BI      <- mmap_bi$BI + mma_sdbi$BI
        mmap_bi$BR      <- mmap_bi$BR + mma_sdbi$BR
    #    mmap_bi$CB      <- mmap_bi$CB + mma_sdbi$CB
    #    mmap_bi$DS      <- mmap_bi$DS + mma_sdbi$DS
        mmap_bi$ECR     <- mmap_bi$ECR + mma_sdbi$ECR
        mmap_bi$ECU     <- mmap_bi$ECU + mma_sdbi$ECU
        mmap_bi$EDL     <- mmap_bi$EDL + mma_sdbi$EDL
        mmap_bi$FCR     <- mmap_bi$FCR + mma_sdbi$FCR
        mmap_bi$FCU     <- mmap_bi$FCU + mma_sdbi$FCU
        mmap_bi$FDL     <- mmap_bi$FDL + mma_sdbi$FDL
    #    mmap_bi$FDL.U   <- mmap_bi$FDL.U + mma_sdbi$FDL.U
        mmap_bi$HR      <- mmap_bi$HR + mma_sdbi$HR
    #    mmap_bi$LD      <- mmap_bi$LD + mma_sdbi$LD
    #    mmap_bi$PECT     <- mmap_bi$PEC + mma_sdbi$PEC
        mmap_bi$PT      <- mmap_bi$PT + mma_sdbi$PT
    #    mmap_bi$SHA     <- mmap_bi$SHA + mma_sdbi$SHA
    #    mmap_bi$SHP     <- mmap_bi$SHP + mma_sdbi$SHP
    #    mmap_bi$SUBS    <- mmap_bi$SUBS + mma_sdbi$SUBS
        mmap_bi$SUP     <- mmap_bi$SUP + mma_sdbi$SUP
    #    mmap_bi$SUPC    <- mmap_bi$SUPC + mma_sdbi$SUPC
    #    mmap_bi$TM      <- mmap_bi$TM + mma_sdbi$TM
        mmap_bi$TR.S    <- mmap_bi$TR.S + mma_sdbi$TR.S
        mmap_bi$TR.C    <- mmap_bi$TR.C + mma_sdbi$TR.C
        mmap_bi$TR.M    <- mmap_bi$TR.M + mma_sdbi$TR.M
        mmap_bi$TR.L    <- mmap_bi$TR.L + mma_sdbi$TR.L
    
    #    mmap_bi$ADD     <- mmap_bi$ADD + mma_sdbi$ADD
    #    mmap_bi$AMB     <- mmap_bi$AMB + mma_sdbi$AMB
    #    mmap_bi$CFB     <- mmap_bi$CFB + mma_sdbi$CFB
    #    mmap_bi$FMTE    <- mmap_bi$FMTE + mma_sdbi$FMTE
    #    mmap_bi$FMTI    <- mmap_bi$FMTI + mma_sdbi$FMTI
    #    mmap_bi$FTE     <- mmap_bi$FTE + mma_sdbi$FTE
    #    mmap_bi$FTI.Fan <- mmap_bi$FTI.Fan + mma_sdbi$FTI.Fan
    #    mmap_bi$FTI2    <- mmap_bi$FTI2 + mma_sdbi$FTI2
    #    mmap_bi$IFM     <- mmap_bi$IFM + mma_sdbi$IFM
    #    mmap_bi$ILF     <- mmap_bi$ILF + mma_sdbi$ILF
    #    mmap_bi$ITB     <- mmap_bi$ITB + mma_sdbi$ITB
    #    mmap_bi$PIFE    <- mmap_bi$PIFE + mma_sdbi$PIFE
    #    mmap_bi$PIFI    <- mmap_bi$PIFI + mma_sdbi$PIFI
    # new sum
    mmap_bi$SUM <- mmap_bi$SUM + mma_sdbi$SUM;
  }
  mmam_bi <- mma_bi
  {
    
        mmam_bi$BI      <- mmam_bi$BI - mma_sdbi$BI
        mmam_bi$BR      <- mmam_bi$BR - mma_sdbi$BR
    #    mmam_bi$CB      <- mmam_bi$CB - mma_sdbi$CB
    #    mmam_bi$DS      <- mmam_bi$DS - mma_sdbi$DS
        mmam_bi$ECR     <- mmam_bi$ECR - mma_sdbi$ECR
        mmam_bi$ECU     <- mmam_bi$ECU - mma_sdbi$ECU
        mmam_bi$EDL     <- mmam_bi$EDL - mma_sdbi$EDL
        mmam_bi$FCR     <- mmam_bi$FCR - mma_sdbi$FCR
        mmam_bi$FCU     <- mmam_bi$FCU - mma_sdbi$FCU
        mmam_bi$FDL     <- mmam_bi$FDL - mma_sdbi$FDL
    #    mmam_bi$FDL.U   <- mmam_bi$FDL.U - mma_sdbi$FDL.U
        mmam_bi$HR      <- mmam_bi$HR - mma_sdbi$HR
    #    mmam_bi$LD      <- mmam_bi$LD - mma_sdbi$LD
    #    mmam_bi$PECT     <- mmam_bi$PEC - mma_sdbi$PEC
        mmam_bi$PT      <- mmam_bi$PT - mma_sdbi$PT
    #    mmam_bi$SHA     <- mmam_bi$SHA - mma_sdbi$SHA
    #    mmam_bi$SHP     <- mmam_bi$SHP - mma_sdbi$SHP
    #    mmam_bi$SUBS    <- mmam_bi$SUBS - mma_sdbi$SUBS
        mmam_bi$SUP     <- mmam_bi$SUP - mma_sdbi$SUP
    #    mmam_bi$SUPC    <- mmam_bi$SUPC - mma_sdbi$SUPC
    #    mmam_bi$TM      <- mmam_bi$TM - mma_sdbi$TM
        mmam_bi$TR.S    <- mmam_bi$TR.S - mma_sdbi$TR.S
        mmam_bi$TR.C    <- mmam_bi$TR.C - mma_sdbi$TR.C
        mmam_bi$TR.M    <- mmam_bi$TR.M - mma_sdbi$TR.M
        mmam_bi$TR.L    <- mmam_bi$TR.L - mma_sdbi$TR.L
    
    #    mmam_bi$ADD     <- mmam_bi$ADD - mma_sdbi$ADD
    #    mmam_bi$AMB     <- mmam_bi$AMB - mma_sdbi$AMB
    #    mmam_bi$CFB     <- mmam_bi$CFB - mma_sdbi$CFB
    #    mmam_bi$FMTE    <- mmam_bi$FMTE - mma_sdbi$FMTE
    #    mmam_bi$FMTI    <- mmam_bi$FMTI - mma_sdbi$FMTI
    #    mmam_bi$FTE     <- mmam_bi$FTE - mma_sdbi$FTE
    #    mmam_bi$FTI.Fan <- mmam_bi$FTI.Fan - mma_sdbi$FTI.Fan
    #    mmam_bi$FTI2    <- mmam_bi$FTI2 - mma_sdbi$FTI2
    #    mmam_bi$IFM     <- mmam_bi$IFM - mma_sdbi$IFM
    #    mmam_bi$ILF     <- mmam_bi$ILF - mma_sdbi$ILF
    #    mmam_bi$ITB     <- mmam_bi$ITB - mma_sdbi$ITB
    #    mmam_bi$PIFE    <- mmam_bi$PIFE - mma_sdbi$PIFE
    #    mmam_bi$PIFI    <- mmam_bi$PIFI - mma_sdbi$PIFI
    #new sum
    mmam_bi$SUM <- mmam_bi$SUM - mma_sdbi$SUM;
  }
  mmap_qu <- mma_qu
  {
    
        mmap_qu$BI      <- mmap_qu$BI + mma_sdqu$BI
        mmap_qu$BR      <- mmap_qu$BR + mma_sdqu$BR
    #    mmap_qu$CB      <- mmap_qu$CB + mma_sdqu$CB
    #    mmap_qu$DS      <- mmap_qu$DS + mma_sdqu$DS
        mmap_qu$ECR     <- mmap_qu$ECR + mma_sdqu$ECR
        mmap_qu$ECU     <- mmap_qu$ECU + mma_sdqu$ECU
        mmap_qu$EDL     <- mmap_qu$EDL + mma_sdqu$EDL
        mmap_qu$FCR     <- mmap_qu$FCR + mma_sdqu$FCR
        mmap_qu$FCU     <- mmap_qu$FCU + mma_sdqu$FCU
        mmap_qu$FDL     <- mmap_qu$FDL + mma_sdqu$FDL
    #    mmap_qu$FDL.U   <- mmap_qu$FDL.U + mma_sdqu$FDL.U
        mmap_qu$HR      <- mmap_qu$HR + mma_sdqu$HR
    #    mmap_qu$LD      <- mmap_qu$LD + mma_sdqu$LD
    #    mmap_qu$PECT     <- mmap_qu$PEC + mma_sdqu$PEC
        mmap_qu$PT      <- mmap_qu$PT + mma_sdqu$PT
    #    mmap_qu$SHA     <- mmap_qu$SHA + mma_sdqu$SHA
    #    mmap_qu$SHP     <- mmap_qu$SHP + mma_sdqu$SHP
    #    mmap_qu$SUBS    <- mmap_qu$SUBS + mma_sdqu$SUBS
        mmap_qu$SUP     <- mmap_qu$SUP + mma_sdqu$SUP
    #    mmap_qu$SUPC    <- mmap_qu$SUPC + mma_sdqu$SUPC
    #    mmap_qu$TM      <- mmap_qu$TM + mma_sdqu$TM
        mmap_qu$TR.S    <- mmap_qu$TR.S + mma_sdqu$TR.S
        mmap_qu$TR.C    <- mmap_qu$TR.C + mma_sdqu$TR.C
        mmap_qu$TR.M    <- mmap_qu$TR.M + mma_sdqu$TR.M
        mmap_qu$TR.L    <- mmap_qu$TR.L + mma_sdqu$TR.L
    
    #    mmap_qu$ADD     <- mmap_qu$ADD + mma_sdqu$ADD
    #    mmap_qu$AMB     <- mmap_qu$AMB + mma_sdqu$AMB
    #    mmap_qu$CFB     <- mmap_qu$CFB + mma_sdqu$CFB
    #    mmap_qu$FMTE    <- mmap_qu$FMTE + mma_sdqu$FMTE
    #    mmap_qu$FMTI    <- mmap_qu$FMTI + mma_sdqu$FMTI
    #    mmap_qu$FTE     <- mmap_qu$FTE + mma_sdqu$FTE
    #    mmap_qu$FTI.Fan <- mmap_qu$FTI.Fan + mma_sdqu$FTI.Fan
    #    mmap_qu$FTI2    <- mmap_qu$FTI2 + mma_sdqu$FTI2
    #    mmap_qu$IFM     <- mmap_qu$IFM + mma_sdqu$IFM
    #    mmap_qu$ILF     <- mmap_qu$ILF + mma_sdqu$ILF
    #    mmap_qu$ITB     <- mmap_qu$ITB + mma_sdqu$ITB
    #    mmap_qu$PIFE    <- mmap_qu$PIFE + mma_sdqu$PIFE
    #    mmap_qu$PIFI    <- mmap_qu$PIFI + mma_sdqu$PIFI
    # new sum
    mmap_qu$SUM <- mmap_qu$SUM + mma_sdqu$SUM;
  }
  mmam_qu <- mma_qu
  {
    
        mmam_qu$BI      <- mmam_qu$BI - mma_sdqu$BI
        mmam_qu$BR      <- mmam_qu$BR - mma_sdqu$BR
    #    mmam_qu$CB      <- mmam_qu$CB - mma_sdqu$CB
    #    mmam_qu$DS      <- mmam_qu$DS - mma_sdqu$DS
        mmam_qu$ECR     <- mmam_qu$ECR - mma_sdqu$ECR
        mmam_qu$ECU     <- mmam_qu$ECU - mma_sdqu$ECU
        mmam_qu$EDL     <- mmam_qu$EDL - mma_sdqu$EDL
        mmam_qu$FCR     <- mmam_qu$FCR - mma_sdqu$FCR
        mmam_qu$FCU     <- mmam_qu$FCU - mma_sdqu$FCU
        mmam_qu$FDL     <- mmam_qu$FDL - mma_sdqu$FDL
    #    mmam_qu$FDL.U   <- mmam_qu$FDL.U - mma_sdqu$FDL.U
        mmam_qu$HR      <- mmam_qu$HR - mma_sdqu$HR
    #    mmam_qu$LD      <- mmam_qu$LD - mma_sdqu$LD
    #    mmam_qu$PECT     <- mmam_qu$PEC - mma_sdqu$PEC
        mmam_qu$PT      <- mmam_qu$PT - mma_sdqu$PT
    #    mmam_qu$SHA     <- mmam_qu$SHA - mma_sdqu$SHA
    #    mmam_qu$SHP     <- mmam_qu$SHP - mma_sdqu$SHP
    #    mmam_qu$SUBS    <- mmam_qu$SUBS - mma_sdqu$SUBS
        mmam_qu$SUP     <- mmam_qu$SUP - mma_sdqu$SUP
    #    mmam_qu$SUPC    <- mmam_qu$SUPC - mma_sdqu$SUPC
    #    mmam_qu$TM      <- mmam_qu$TM - mma_sdqu$TM
        mmam_qu$TR.S    <- mmam_qu$TR.S - mma_sdqu$TR.S
        mmam_qu$TR.C    <- mmam_qu$TR.C - mma_sdqu$TR.C
        mmam_qu$TR.M    <- mmam_qu$TR.M - mma_sdqu$TR.M
        mmam_qu$TR.L    <- mmam_qu$TR.L - mma_sdqu$TR.L
    
    #    mmam_qu$ADD     <- mmam_qu$ADD - mma_sdqu$ADD
    #    mmam_qu$AMB     <- mmam_qu$AMB - mma_sdqu$AMB
    #    mmam_qu$CFB     <- mmam_qu$CFB - mma_sdqu$CFB
    #    mmam_qu$FMTE    <- mmam_qu$FMTE - mma_sdqu$FMTE
    #    mmam_qu$FMTI    <- mmam_qu$FMTI - mma_sdqu$FMTI
    #    mmam_qu$FTE     <- mmam_qu$FTE - mma_sdqu$FTE
    #    mmam_qu$FTI.Fan <- mmam_qu$FTI.Fan - mma_sdqu$FTI.Fan
    #    mmam_qu$FTI2    <- mmam_qu$FTI2 - mma_sdqu$FTI2
    #    mmam_qu$IFM     <- mmam_qu$IFM - mma_sdqu$IFM
    #    mmam_qu$ILF     <- mmam_qu$ILF - mma_sdqu$ILF
    #    mmam_qu$ITB     <- mmam_qu$ITB - mma_sdqu$ITB
    #    mmam_qu$PIFE    <- mmam_qu$PIFE - mma_sdqu$PIFE
    #    mmam_qu$PIFI    <- mmam_qu$PIFI - mma_sdqu$PIFI
    #new sum
    mmam_qu$SUM <- mmam_qu$SUM - mma_sdqu$SUM;
  }
}

##Plotting##
library(ggplot2)
Time <- mmap_bu$time
Moment_Arm_bu    <- mma_bu$SUM
Moment_Arm_bu2   <- mmam_bu$SUM
Moment_Arm_bu3   <- mmap_bu$SUM
Moment_Arm_bi    <- mma_bi$SUM
Moment_Arm_bi2   <- mmam_bi$SUM
Moment_Arm_bi3   <- mmap_bi$SUM
Moment_Arm_qu    <- mma_qu$SUM
Moment_Arm_qu2   <- mmam_qu$SUM
Moment_Arm_qu3   <- mmap_qu$SUM
zero_line        <- zero$SUM
exname <- "Moment Arm Figures/"
data <- data.frame (Time,Moment_Arm_bu,Moment_Arm_bu2,Moment_Arm_bu3,
                    Moment_Arm_bi,Moment_Arm_bi2,Moment_Arm_bi3,
                    Moment_Arm_qu,Moment_Arm_qu2,Moment_Arm_qu3)

ggplot(data,aes(x=Time)) +
    geom_line(aes(y=zero_line,colour="black"), size=1) +
    geom_ribbon(aes(ymin=Moment_Arm_bu2,ymax=Moment_Arm_bu3),fill="red",alpha=0.5) +
    geom_line(aes(y=Moment_Arm_bu,colour="red"), size=2) +
    geom_ribbon(aes(ymin=Moment_Arm_bi2,ymax=Moment_Arm_bi3),fill="blue",alpha=0.5) +
    geom_line(aes(y=Moment_Arm_bi,colour="blue"), size=2) +
    geom_ribbon(aes(ymin=Moment_Arm_qu2,ymax=Moment_Arm_qu3),fill="yellow",alpha=0.5) +
    geom_line(aes(y=Moment_Arm_qu,colour="yellow"), size=2) +
    labs(x="Time", y="Summed Moment Arms") +
    ggtitle(paste0(ti))+
  theme(plot.title=element_text(hjust=0.5))+
    scale_fill_identity(name='Legend',guide='legend',labels=c('error'))+
    scale_color_manual(name='Launch',values=c('red'='#D81B60','blue'='#1E88E5','yellow'='#FFC107'), 
                       labels=c('B-B','B-C','Q'))
ggsave((paste0(exname,ti," Moment Arms.pdf")), plot = last_plot(), device = NULL, path = NULL, scale = 1, dpi = 300)
}

graph.loop (j = "Elbow", df = "_ABAD", ti= "Elbow Abduction") 
graph.loop (j = "Elbow", df = "_LAR", ti= "Elbow Long Axis Rotation")   
graph.loop (j = "Elbow", df = "_FE", ti= "Elbow Extension")   
