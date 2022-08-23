### Script to average down the files and make an averaged Fmax across the montecarlo sims ###

# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/MMA/MM");
Polymerization <- function (sc,l,ot)
{
##load files##
#ABAD Hip
{
aMM1 <- read.csv(paste0(sc,l,"Hip_ABAD_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                "\"",dec = ".", fill = TRUE, comment.char = "")
aMM2 <- read.csv(paste0(sc,l,"Hip_ABAD_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                      "\"",dec = ".", fill = TRUE, comment.char = "")
aMM3 <- read.csv(paste0(sc,l,"Hip_ABAD_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
aMM4 <- read.csv(paste0(sc,l,"Hip_ABAD_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")



aSD1 <- read.csv(paste0(sc,l,"Hip_ABAD_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                  "\"",dec = ".", fill = TRUE, comment.char = "")
aSD2 <- read.csv(paste0(sc,l,"Hip_ABAD_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                  "\"",dec = ".", fill = TRUE, comment.char = "")
aSD3 <- read.csv(paste0(sc,l,"Hip_ABAD_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
aSD4 <- read.csv(paste0(sc,l,"Hip_ABAD_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
##convert##
dfaMM <- Reduce('+',mget(paste0("aMM",1:4)))/4
dfaSD <- Reduce('+',mget(paste0("aSD",1:4)))/4

output<-paste0(ot,sc,l,"Hip_ABAD_m3c_")

write.csv(dfaMM,paste0(output, "mean.csv"))
write.csv(dfaSD,paste0(output, "stdev.csv"))
} #ABAD
#LAR
{
lMM1 <- read.csv(paste0(sc,l,"Hip_LAR_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
lMM2 <- read.csv(paste0(sc,l,"Hip_LAR_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
lMM3 <- read.csv(paste0(sc,l,"Hip_LAR_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
lMM4 <- read.csv(paste0(sc,l,"Hip_LAR_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")


lSD1 <- read.csv(paste0(sc,l,"Hip_LAR_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
lSD2 <- read.csv(paste0(sc,l,"Hip_LAR_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
lSD3 <- read.csv(paste0(sc,l,"Hip_LAR_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
lSD4 <- read.csv(paste0(sc,l,"Hip_LAR_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
##convert##
dflMM <- Reduce('+',mget(paste0("lMM",1:4)))/4
dflSD <- Reduce('+',mget(paste0("lSD",1:4)))/4

output<-paste0(ot,sc,l,"Hip_LAR_m3c_")

write.csv(dflMM,paste0(output, "mean.csv"))
write.csv(dflSD,paste0(output, "stdev.csv"))
} #LAR
#FE
{
fMM1 <- read.csv(paste0(sc,l,"Hip_FE_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
fMM2 <- read.csv(paste0(sc,l,"Hip_FE_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
fMM3 <- read.csv(paste0(sc,l,"Hip_FE_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
fMM4 <- read.csv(paste0(sc,l,"Hip_FE_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")


fSD1 <- read.csv(paste0(sc,l,"Hip_FE_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
fSD2 <- read.csv(paste0(sc,l,"Hip_FE_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
fSD3 <- read.csv(paste0(sc,l,"Hip_FE_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
fSD4 <- read.csv(paste0(sc,l,"Hip_FE_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                   "\"",dec = ".", fill = TRUE, comment.char = "")
##convert##
dffMM <- Reduce('+',mget(paste0("fMM",1:4)))/4
dffSD <- Reduce('+',mget(paste0("fSD",1:4)))/4

output<-paste0(ot,sc,l,"Hip_FE_m3c_")

write.csv(dffMM,paste0(output, "mean.csv"))
write.csv(dffSD,paste0(output, "stdev.csv"))
}#FE

#ABAD Knee
{
  aMM1 <- read.csv(paste0(sc,l,"Knee_ABAD_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM2 <- read.csv(paste0(sc,l,"Knee_ABAD_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM3 <- read.csv(paste0(sc,l,"Knee_ABAD_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM4 <- read.csv(paste0(sc,l,"Knee_ABAD_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  aSD1 <- read.csv(paste0(sc,l,"Knee_ABAD_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD2 <- read.csv(paste0(sc,l,"Knee_ABAD_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD3 <- read.csv(paste0(sc,l,"Knee_ABAD_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD4 <- read.csv(paste0(sc,l,"Knee_ABAD_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dfaMM <- Reduce('+',mget(paste0("aMM",1:4)))/4
  dfaSD <- Reduce('+',mget(paste0("aSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"Knee_ABAD_m3c_")
  
  write.csv(dfaMM,paste0(output, "mean.csv"))
  write.csv(dfaSD,paste0(output, "stdev.csv"))
} #ABAD
#LAR
{
  lMM1 <- read.csv(paste0(sc,l,"Knee_LAR_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM2 <- read.csv(paste0(sc,l,"Knee_LAR_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM3 <- read.csv(paste0(sc,l,"Knee_LAR_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM4 <- read.csv(paste0(sc,l,"Knee_LAR_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  lSD1 <- read.csv(paste0(sc,l,"Knee_LAR_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD2 <- read.csv(paste0(sc,l,"Knee_LAR_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD3 <- read.csv(paste0(sc,l,"Knee_LAR_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD4 <- read.csv(paste0(sc,l,"Knee_LAR_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dflMM <- Reduce('+',mget(paste0("lMM",1:4)))/4
  dflSD <- Reduce('+',mget(paste0("lSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"Knee_LAR_m3c_")
  
  write.csv(dflMM,paste0(output, "mean.csv"))
  write.csv(dflSD,paste0(output, "stdev.csv"))
} #LAR
#FE
{
  fMM1 <- read.csv(paste0(sc,l,"Knee_FE_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM2 <- read.csv(paste0(sc,l,"Knee_FE_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM3 <- read.csv(paste0(sc,l,"Knee_FE_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM4 <- read.csv(paste0(sc,l,"Knee_FE_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  fSD1 <- read.csv(paste0(sc,l,"Knee_FE_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD2 <- read.csv(paste0(sc,l,"Knee_FE_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD3 <- read.csv(paste0(sc,l,"Knee_FE_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD4 <- read.csv(paste0(sc,l,"Knee_FE_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dffMM <- Reduce('+',mget(paste0("fMM",1:4)))/4
  dffSD <- Reduce('+',mget(paste0("fSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"Knee_FE_m3c_")
  
  write.csv(dffMM,paste0(output, "mean.csv"))
  write.csv(dffSD,paste0(output, "stdev.csv"))
}#FE

#ABAD Shoulder
{
  aMM1 <- read.csv(paste0(sc,l,"Shoulder_ABAD_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM2 <- read.csv(paste0(sc,l,"Shoulder_ABAD_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM3 <- read.csv(paste0(sc,l,"Shoulder_ABAD_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM4 <- read.csv(paste0(sc,l,"Shoulder_ABAD_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  aSD1 <- read.csv(paste0(sc,l,"Shoulder_ABAD_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD2 <- read.csv(paste0(sc,l,"Shoulder_ABAD_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD3 <- read.csv(paste0(sc,l,"Shoulder_ABAD_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD4 <- read.csv(paste0(sc,l,"Shoulder_ABAD_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dfaMM <- Reduce('+',mget(paste0("aMM",1:4)))/4
  dfaSD <- Reduce('+',mget(paste0("aSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"Shoulder_ABAD_m3c_")
  
  write.csv(dfaMM,paste0(output, "mean.csv"))
  write.csv(dfaSD,paste0(output, "stdev.csv"))
} #ABAD
#LAR
{
  lMM1 <- read.csv(paste0(sc,l,"Shoulder_LAR_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM2 <- read.csv(paste0(sc,l,"Shoulder_LAR_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM3 <- read.csv(paste0(sc,l,"Shoulder_LAR_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM4 <- read.csv(paste0(sc,l,"Shoulder_LAR_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  lSD1 <- read.csv(paste0(sc,l,"Shoulder_LAR_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD2 <- read.csv(paste0(sc,l,"Shoulder_LAR_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD3 <- read.csv(paste0(sc,l,"Shoulder_LAR_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD4 <- read.csv(paste0(sc,l,"Shoulder_LAR_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dflMM <- Reduce('+',mget(paste0("lMM",1:4)))/4
  dflSD <- Reduce('+',mget(paste0("lSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"Shoulder_LAR_m3c_")
  
  write.csv(dflMM,paste0(output, "mean.csv"))
  write.csv(dflSD,paste0(output, "stdev.csv"))
} #LAR
#FE
{
  fMM1 <- read.csv(paste0(sc,l,"Shoulder_FE_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM2 <- read.csv(paste0(sc,l,"Shoulder_FE_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM3 <- read.csv(paste0(sc,l,"Shoulder_FE_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM4 <- read.csv(paste0(sc,l,"Shoulder_FE_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  fSD1 <- read.csv(paste0(sc,l,"Shoulder_FE_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD2 <- read.csv(paste0(sc,l,"Shoulder_FE_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD3 <- read.csv(paste0(sc,l,"Shoulder_FE_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD4 <- read.csv(paste0(sc,l,"Shoulder_FE_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dffMM <- Reduce('+',mget(paste0("fMM",1:4)))/4
  dffSD <- Reduce('+',mget(paste0("fSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"Shoulder_FE_m3c_")
  
  write.csv(dffMM,paste0(output, "mean.csv"))
  write.csv(dffSD,paste0(output, "stdev.csv"))
}#FE

#ABAD Elbow
{
  aMM1 <- read.csv(paste0(sc,l,"Elbow_ABAD_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM2 <- read.csv(paste0(sc,l,"Elbow_ABAD_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM3 <- read.csv(paste0(sc,l,"Elbow_ABAD_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM4 <- read.csv(paste0(sc,l,"Elbow_ABAD_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  aSD1 <- read.csv(paste0(sc,l,"Elbow_ABAD_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD2 <- read.csv(paste0(sc,l,"Elbow_ABAD_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD3 <- read.csv(paste0(sc,l,"Elbow_ABAD_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD4 <- read.csv(paste0(sc,l,"Elbow_ABAD_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dfaMM <- Reduce('+',mget(paste0("aMM",1:4)))/4
  dfaSD <- Reduce('+',mget(paste0("aSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"Elbow_ABAD_m3c_")
  
  write.csv(dfaMM,paste0(output, "mean.csv"))
  write.csv(dfaSD,paste0(output, "stdev.csv"))
} #ABAD
#LAR
{
  lMM1 <- read.csv(paste0(sc,l,"Elbow_LAR_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM2 <- read.csv(paste0(sc,l,"Elbow_LAR_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM3 <- read.csv(paste0(sc,l,"Elbow_LAR_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM4 <- read.csv(paste0(sc,l,"Elbow_LAR_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  lSD1 <- read.csv(paste0(sc,l,"Elbow_LAR_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD2 <- read.csv(paste0(sc,l,"Elbow_LAR_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD3 <- read.csv(paste0(sc,l,"Elbow_LAR_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD4 <- read.csv(paste0(sc,l,"Elbow_LAR_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dflMM <- Reduce('+',mget(paste0("lMM",1:4)))/4
  dflSD <- Reduce('+',mget(paste0("lSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"Elbow_LAR_m3c_")
  
  write.csv(dflMM,paste0(output, "mean.csv"))
  write.csv(dflSD,paste0(output, "stdev.csv"))
} #LAR
#FE
{
  fMM1 <- read.csv(paste0(sc,l,"Elbow_FE_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM2 <- read.csv(paste0(sc,l,"Elbow_FE_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM3 <- read.csv(paste0(sc,l,"Elbow_FE_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM4 <- read.csv(paste0(sc,l,"Elbow_FE_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  fSD1 <- read.csv(paste0(sc,l,"Elbow_FE_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD2 <- read.csv(paste0(sc,l,"Elbow_FE_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD3 <- read.csv(paste0(sc,l,"Elbow_FE_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD4 <- read.csv(paste0(sc,l,"Elbow_FE_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dffMM <- Reduce('+',mget(paste0("fMM",1:4)))/4
  dffSD <- Reduce('+',mget(paste0("fSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"Elbow_FE_m3c_")
  
  write.csv(dffMM,paste0(output, "mean.csv"))
  write.csv(dffSD,paste0(output, "stdev.csv"))
}#FE

#ABAD Wrist
{
  aMM1 <- read.csv(paste0(sc,l,"Wrist_ABAD_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM2 <- read.csv(paste0(sc,l,"Wrist_ABAD_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM3 <- read.csv(paste0(sc,l,"Wrist_ABAD_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM4 <- read.csv(paste0(sc,l,"Wrist_ABAD_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  aSD1 <- read.csv(paste0(sc,l,"Wrist_ABAD_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD2 <- read.csv(paste0(sc,l,"Wrist_ABAD_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD3 <- read.csv(paste0(sc,l,"Wrist_ABAD_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD4 <- read.csv(paste0(sc,l,"Wrist_ABAD_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dfaMM <- Reduce('+',mget(paste0("aMM",1:4)))/4
  dfaSD <- Reduce('+',mget(paste0("aSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"Wrist_ABAD_m3c_")
  
  write.csv(dfaMM,paste0(output, "mean.csv"))
  write.csv(dfaSD,paste0(output, "stdev.csv"))
} #ABAD
#LAR
{
  lMM1 <- read.csv(paste0(sc,l,"Wrist_LAR_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM2 <- read.csv(paste0(sc,l,"Wrist_LAR_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM3 <- read.csv(paste0(sc,l,"Wrist_LAR_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM4 <- read.csv(paste0(sc,l,"Wrist_LAR_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  lSD1 <- read.csv(paste0(sc,l,"Wrist_LAR_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD2 <- read.csv(paste0(sc,l,"Wrist_LAR_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD3 <- read.csv(paste0(sc,l,"Wrist_LAR_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD4 <- read.csv(paste0(sc,l,"Wrist_LAR_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dflMM <- Reduce('+',mget(paste0("lMM",1:4)))/4
  dflSD <- Reduce('+',mget(paste0("lSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"Wrist_LAR_m3c_")
  
  write.csv(dflMM,paste0(output, "mean.csv"))
  write.csv(dflSD,paste0(output, "stdev.csv"))
} #LAR
#FE
{
  fMM1 <- read.csv(paste0(sc,l,"Wrist_FE_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM2 <- read.csv(paste0(sc,l,"Wrist_FE_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM3 <- read.csv(paste0(sc,l,"Wrist_FE_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM4 <- read.csv(paste0(sc,l,"Wrist_FE_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  fSD1 <- read.csv(paste0(sc,l,"Wrist_FE_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD2 <- read.csv(paste0(sc,l,"Wrist_FE_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD3 <- read.csv(paste0(sc,l,"Wrist_FE_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD4 <- read.csv(paste0(sc,l,"Wrist_FE_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dffMM <- Reduce('+',mget(paste0("fMM",1:4)))/4
  dffSD <- Reduce('+',mget(paste0("fSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"Wrist_FE_m3c_")
  
  write.csv(dffMM,paste0(output, "mean.csv"))
  write.csv(dffSD,paste0(output, "stdev.csv"))
}#FE

#ABAD WMC
{
  aMM1 <- read.csv(paste0(sc,l,"WMC_ABAD_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM2 <- read.csv(paste0(sc,l,"WMC_ABAD_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM3 <- read.csv(paste0(sc,l,"WMC_ABAD_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM4 <- read.csv(paste0(sc,l,"WMC_ABAD_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  aSD1 <- read.csv(paste0(sc,l,"WMC_ABAD_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD2 <- read.csv(paste0(sc,l,"WMC_ABAD_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD3 <- read.csv(paste0(sc,l,"WMC_ABAD_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD4 <- read.csv(paste0(sc,l,"WMC_ABAD_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dfaMM <- Reduce('+',mget(paste0("aMM",1:4)))/4
  dfaSD <- Reduce('+',mget(paste0("aSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"WMC_ABAD_m3c_")
  
  write.csv(dfaMM,paste0(output, "mean.csv"))
  write.csv(dfaSD,paste0(output, "stdev.csv"))
} #ABAD
#LAR
{
  lMM1 <- read.csv(paste0(sc,l,"WMC_LAR_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM2 <- read.csv(paste0(sc,l,"WMC_LAR_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM3 <- read.csv(paste0(sc,l,"WMC_LAR_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM4 <- read.csv(paste0(sc,l,"WMC_LAR_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  lSD1 <- read.csv(paste0(sc,l,"WMC_LAR_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD2 <- read.csv(paste0(sc,l,"WMC_LAR_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD3 <- read.csv(paste0(sc,l,"WMC_LAR_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD4 <- read.csv(paste0(sc,l,"WMC_LAR_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dflMM <- Reduce('+',mget(paste0("lMM",1:4)))/4
  dflSD <- Reduce('+',mget(paste0("lSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"WMC_LAR_m3c_")
  
  write.csv(dflMM,paste0(output, "mean.csv"))
  write.csv(dflSD,paste0(output, "stdev.csv"))
} #LAR
#FE
{
  fMM1 <- read.csv(paste0(sc,l,"WMC_FE_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM2 <- read.csv(paste0(sc,l,"WMC_FE_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM3 <- read.csv(paste0(sc,l,"WMC_FE_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM4 <- read.csv(paste0(sc,l,"WMC_FE_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  fSD1 <- read.csv(paste0(sc,l,"WMC_FE_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD2 <- read.csv(paste0(sc,l,"WMC_FE_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD3 <- read.csv(paste0(sc,l,"WMC_FE_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD4 <- read.csv(paste0(sc,l,"WMC_FE_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dffMM <- Reduce('+',mget(paste0("fMM",1:4)))/4
  dffSD <- Reduce('+',mget(paste0("fSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"WMC_FE_m3c_")
  
  write.csv(dffMM,paste0(output, "mean.csv"))
  write.csv(dffSD,paste0(output, "stdev.csv"))
}#FE

#ABAD WP1
{
  aMM1 <- read.csv(paste0(sc,l,"WP1_ABAD_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM2 <- read.csv(paste0(sc,l,"WP1_ABAD_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM3 <- read.csv(paste0(sc,l,"WP1_ABAD_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aMM4 <- read.csv(paste0(sc,l,"WP1_ABAD_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  aSD1 <- read.csv(paste0(sc,l,"WP1_ABAD_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD2 <- read.csv(paste0(sc,l,"WP1_ABAD_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD3 <- read.csv(paste0(sc,l,"WP1_ABAD_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  aSD4 <- read.csv(paste0(sc,l,"WP1_ABAD_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dfaMM <- Reduce('+',mget(paste0("aMM",1:4)))/4
  dfaSD <- Reduce('+',mget(paste0("aSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"WP1_ABAD_m3c_")
  
  write.csv(dfaMM,paste0(output, "mean.csv"))
  write.csv(dfaSD,paste0(output, "stdev.csv"))
} #ABAD
#LAR
{
  lMM1 <- read.csv(paste0(sc,l,"WP1_LAR_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM2 <- read.csv(paste0(sc,l,"WP1_LAR_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM3 <- read.csv(paste0(sc,l,"WP1_LAR_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lMM4 <- read.csv(paste0(sc,l,"WP1_LAR_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  lSD1 <- read.csv(paste0(sc,l,"WP1_LAR_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD2 <- read.csv(paste0(sc,l,"WP1_LAR_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD3 <- read.csv(paste0(sc,l,"WP1_LAR_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  lSD4 <- read.csv(paste0(sc,l,"WP1_LAR_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  ##convert##
  dflMM <- Reduce('+',mget(paste0("lMM",1:4)))/4
  dflSD <- Reduce('+',mget(paste0("lSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"WP1_LAR_m3c_")
  
  write.csv(dflMM,paste0(output, "mean.csv"))
  write.csv(dflSD,paste0(output, "stdev.csv"))
} #LAR
#FE
{
  fMM1 <- read.csv(paste0(sc,l,"WP1_FE_MM1_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM2 <- read.csv(paste0(sc,l,"WP1_FE_MM2_m3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM3 <- read.csv(paste0(sc,l,"WP1_FE_MM1_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fMM4 <- read.csv(paste0(sc,l,"WP1_FE_MM2_md3c_MC_mean.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  
  fSD1 <- read.csv(paste0(sc,l,"WP1_FE_MM1_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD2 <- read.csv(paste0(sc,l,"WP1_FE_MM2_m3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD3 <- read.csv(paste0(sc,l,"WP1_FE_MM1_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  fSD4 <- read.csv(paste0(sc,l,"WP1_FE_MM2_md3c_MC_stdev.csv"), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  ##convert##
  dffMM <- Reduce('+',mget(paste0("fMM",1:4)))/4
  dffSD <- Reduce('+',mget(paste0("fSD",1:4)))/4
  
  output<-paste0(ot,sc,l,"WP1_FE_m3c_")
  
  write.csv(dffMM,paste0(output, "mean.csv"))
  write.csv(dffSD,paste0(output, "stdev.csv"))
}#FE
}  
Polymerization (sc="0_73", l= "/Quad/" , ot="Combined/")
Polymerization (sc="0_73", l= "/Biped/" , ot="Combined/")
Polymerization (sc="0_73", l= "/Burst/" , ot="Combined/")
Polymerization (sc="0_67", l= "/Quad/" , ot="Combined/")
Polymerization (sc="0_67", l= "/Biped/" , ot="Combined/")
Polymerization (sc="0_67", l= "/Burst/" , ot="Combined/")
