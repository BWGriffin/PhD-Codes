# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/MMA");

##Function to load and graph the muscle moments. Need to modify the column names 1-6 manually
## sc = scaling factor, j = the joint (column names depend on this), df = the degree of freedom being graphed
## mass = which mass is being graphed, ti = what you want the graph and file named
graph.loop <- function(sc1, sc2, j, df, mass, ti, vary, l1, l2, l3)
  {
##Load and Setup##
  dirname <- "MM/Combined/"
  quname <- (paste0(dirname,(paste0(sc1,"Quad/"))))
  quname2 <- (paste0(dirname,(paste0(sc2,"Quad/"))))
  buname2 <- (paste0(quname, j,df,mass))
  biname2 <- (paste0(quname2, j,df,mass))
  

  # read in raw data
  mm_bu   <- read.csv((paste0(buname2,"_mean.csv")), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  mm_bi   <- read.csv((paste0(biname2,"_mean.csv")), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  mm_sdbu <- read.csv((paste0(buname2,"_stdev.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")
  mm_sdbi <- read.csv((paste0(biname2,"_stdev.csv")), header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  zero <- read.csv("zero.csv", header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")

  #make mean+/-sd
  mm_p_bu <- mm_bu
  {
    ##column names 1
    #    mm_p_bu$BI      <- mm_p_bu$BI + mm_sdbu$BI            # ,e
    #    mm_p_bu$BR      <- mm_p_bu$BR + mm_sdbu$BR            # ,e
    #    mm_p_bu$CB      <- mm_p_bu$CB + mm_sdbu$CB            #s
    #    mm_p_bu$DS      <- mm_p_bu$DS + mm_sdbu$DS            #s
    #    mm_p_bu$ECR     <- mm_p_bu$ECR + mm_sdbu$ECR          # ,e,wr
    #    mm_p_bu$ECU     <- mm_p_bu$ECU + mm_sdbu$ECU          # ,e,wr,wm
    #    mm_p_bu$EDL     <- mm_p_bu$EDL + mm_sdbu$EDL          # ,e,wr,wm,wp
    #    mm_p_bu$FCR     <- mm_p_bu$FCR + mm_sdbu$FCR          # ,e,wr
    #    mm_p_bu$FCU     <- mm_p_bu$FCU + mm_sdbu$FCU          # ,e,wr,wm
    #    mm_p_bu$FDL     <- mm_p_bu$FDL + mm_sdbu$FDL          # ,e,wr,wm,wp
    #    mm_p_bu$FDL.U   <- mm_p_bu$FDL.U + mm_sdbu$FDL.U      # , ,wr,wm,wp
    #    mm_p_bu$HR      <- mm_p_bu$HR + mm_sdbu$HR            # ,e
    #    mm_p_bu$LD      <- mm_p_bu$LD + mm_sdbu$LD            #s
    #    mm_p_bu$PECT     <- mm_p_bu$PECT + mm_sdbu$PECT       #s
    #    mm_p_bu$PT      <- mm_p_bu$PT + mm_sdbu$PT            # ,e
    #    mm_p_bu$SHA     <- mm_p_bu$SHA + mm_sdbu$SHA          #s
    #    mm_p_bu$SHP     <- mm_p_bu$SHP + mm_sdbu$SHP          #s
    #    mm_p_bu$SUBS    <- mm_p_bu$SUBS + mm_sdbu$SUBS        #s
    #    mm_p_bu$SUP     <- mm_p_bu$SUP + mm_sdbu$SUP          # ,e
    #    mm_p_bu$SUPC    <- mm_p_bu$SUPC + mm_sdbu$SUPC        #s
    #    mm_p_bu$TM      <- mm_p_bu$TM + mm_sdbu$TM            #s
    #    mm_p_bu$TR.S    <- mm_p_bu$TR.S + mm_sdbu$TR.S        #s,e
    #    mm_p_bu$TR.C    <- mm_p_bu$TR.C + mm_sdbu$TR.C        #s,e
    #    mm_p_bu$TR.M    <- mm_p_bu$TR.M + mm_sdbu$TR.M        # ,e
    #    mm_p_bu$TR.L    <- mm_p_bu$TR.L + mm_sdbu$TR.L        # ,e
    
        mm_p_bu$ADD     <- mm_p_bu$ADD + mm_sdbu$ADD          #h
        mm_p_bu$AMB     <- mm_p_bu$AMB + mm_sdbu$AMB          #h,k
        mm_p_bu$CFB     <- mm_p_bu$CFB + mm_sdbu$CFB          #h
    #    mm_p_bu$FMTE    <- mm_p_bu$FMTE + mm_sdbu$FMTE        # ,k
    #    mm_p_bu$FMTI    <- mm_p_bu$FMTI + mm_sdbu$FMTI        # ,k
        mm_p_bu$FTE     <- mm_p_bu$FTE + mm_sdbu$FTE          #h,k
        mm_p_bu$FTI.Fan <- mm_p_bu$FTI.Fan + mm_sdbu$FTI.Fan  #h,k
        mm_p_bu$FTI2    <- mm_p_bu$FTI2 + mm_sdbu$FTI2        #h,k
        mm_p_bu$IFM     <- mm_p_bu$IFM + mm_sdbu$IFM          #h
        mm_p_bu$ILF     <- mm_p_bu$ILF + mm_sdbu$ILF          #h,k
        mm_p_bu$ITB     <- mm_p_bu$ITB + mm_sdbu$ITB          #h,k
        mm_p_bu$PIFE    <- mm_p_bu$PIFE + mm_sdbu$PIFE        #h
        mm_p_bu$PIFI    <- mm_p_bu$PIFI + mm_sdbu$PIFI        #h

    # new sum
    mm_p_bu$SUM <- mm_p_bu$SUM + mm_sdbu$SUM;
  }
  
  mm_m_bu <- mm_bu
  {
    ##column names 2    
    #    mm_m_bu$BI      <- mm_m_bu$BI - mm_sdbu$BI
    #    mm_m_bu$BR      <- mm_m_bu$BR - mm_sdbu$BR
    #    mm_m_bu$CB      <- mm_m_bu$CB - mm_sdbu$CB
    #    mm_m_bu$DS      <- mm_m_bu$DS - mm_sdbu$DS
    #    mm_m_bu$ECR     <- mm_m_bu$ECR - mm_sdbu$ECR
    #    mm_m_bu$ECU     <- mm_m_bu$ECU - mm_sdbu$ECU
    #    mm_m_bu$EDL     <- mm_m_bu$EDL - mm_sdbu$EDL
    #    mm_m_bu$FCR     <- mm_m_bu$FCR - mm_sdbu$FCR
    #    mm_m_bu$FCU     <- mm_m_bu$FCU - mm_sdbu$FCU
    #    mm_m_bu$FDL     <- mm_m_bu$FDL - mm_sdbu$FDL
    #    mm_m_bu$FDL.U   <- mm_m_bu$FDL.U - mm_sdbu$FDL.U
    #    mm_m_bu$HR      <- mm_m_bu$HR - mm_sdbu$HR
    #    mm_m_bu$LD      <- mm_m_bu$LD - mm_sdbu$LD
    #    mm_m_bu$PECT     <- mm_m_bu$PECT - mm_sdbu$PECT 
    #    mm_m_bu$PT      <- mm_m_bu$PT - mm_sdbu$PT
    #    mm_m_bu$SHA     <- mm_m_bu$SHA - mm_sdbu$SHA
    #    mm_m_bu$SHP     <- mm_m_bu$SHP - mm_sdbu$SHP
    #    mm_m_bu$SUBS    <- mm_m_bu$SUBS - mm_sdbu$SUBS
    #    mm_m_bu$SUP     <- mm_m_bu$SUP - mm_sdbu$SUP
    #    mm_m_bu$SUPC    <- mm_m_bu$SUPC - mm_sdbu$SUPC
    #    mm_m_bu$TM      <- mm_m_bu$TM - mm_sdbu$TM
    #    mm_m_bu$TR.S    <- mm_m_bu$TR.S - mm_sdbu$TR.S
    #    mm_m_bu$TR.C    <- mm_m_bu$TR.C - mm_sdbu$TR.C
    #    mm_m_bu$TR.M    <- mm_m_bu$TR.M - mm_sdbu$TR.M
    #    mm_m_bu$TR.L    <- mm_m_bu$TR.L - mm_sdbu$TR.L
    
        mm_m_bu$ADD     <- mm_m_bu$ADD - mm_sdbu$ADD
        mm_m_bu$AMB     <- mm_m_bu$AMB - mm_sdbu$AMB
        mm_m_bu$CFB     <- mm_m_bu$CFB - mm_sdbu$CFB
    #    mm_m_bu$FMTE    <- mm_m_bu$FMTE - mm_sdbu$FMTE
    #    mm_m_bu$FMTI    <- mm_m_bu$FMTI - mm_sdbu$FMTI
        mm_m_bu$FTE     <- mm_m_bu$FTE - mm_sdbu$FTE
        mm_m_bu$FTI.Fan <- mm_m_bu$FTI.Fan - mm_sdbu$FTI.Fan
        mm_m_bu$FTI2    <- mm_m_bu$FTI2 - mm_sdbu$FTI2
        mm_m_bu$IFM     <- mm_m_bu$IFM - mm_sdbu$IFM
        mm_m_bu$ILF     <- mm_m_bu$ILF - mm_sdbu$ILF
        mm_m_bu$ITB     <- mm_m_bu$ITB - mm_sdbu$ITB
        mm_m_bu$PIFE    <- mm_m_bu$PIFE - mm_sdbu$PIFE
        mm_m_bu$PIFI    <- mm_m_bu$PIFI - mm_sdbu$PIFI

    #new sum
    mm_m_bu$SUM <- mm_m_bu$SUM - mm_sdbu$SUM;
  }
  
  mm_p_bi <- mm_bi
  {
    ##column names 3    
    #    mm_p_bi$BI      <- mm_p_bi$BI + mm_sdbi$BI
    #    mm_p_bi$BR      <- mm_p_bi$BR + mm_sdbi$BR
    #    mm_p_bi$CB      <- mm_p_bi$CB + mm_sdbi$CB
    #    mm_p_bi$DS      <- mm_p_bi$DS + mm_sdbi$DS
    #    mm_p_bi$ECR     <- mm_p_bi$ECR + mm_sdbi$ECR
    #    mm_p_bi$ECU     <- mm_p_bi$ECU + mm_sdbi$ECU
    #    mm_p_bi$EDL     <- mm_p_bi$EDL + mm_sdbi$EDL
    #    mm_p_bi$FCR     <- mm_p_bi$FCR + mm_sdbi$FCR
    #    mm_p_bi$FCU     <- mm_p_bi$FCU + mm_sdbi$FCU
    #    mm_p_bi$FDL     <- mm_p_bi$FDL + mm_sdbi$FDL
    #    mm_p_bi$FDL.U   <- mm_p_bi$FDL.U + mm_sdbi$FDL.U
    #    mm_p_bi$HR      <- mm_p_bi$HR + mm_sdbi$HR
    #    mm_p_bi$LD      <- mm_p_bi$LD + mm_sdbi$LD
    #    mm_p_bi$PECT     <- mm_p_bi$PECT + mm_sdbi$PECT 
    #    mm_p_bi$PT      <- mm_p_bi$PT + mm_sdbi$PT
    #    mm_p_bi$SHA     <- mm_p_bi$SHA + mm_sdbi$SHA
    #    mm_p_bi$SHP     <- mm_p_bi$SHP + mm_sdbi$SHP
    #    mm_p_bi$SUBS    <- mm_p_bi$SUBS + mm_sdbi$SUBS
    #    mm_p_bi$SUP     <- mm_p_bi$SUP + mm_sdbi$SUP
    #    mm_p_bi$SUPC    <- mm_p_bi$SUPC + mm_sdbi$SUPC
    #    mm_p_bi$TM      <- mm_p_bi$TM + mm_sdbi$TM
    #    mm_p_bi$TR.S    <- mm_p_bi$TR.S + mm_sdbi$TR.S
    #    mm_p_bi$TR.C    <- mm_p_bi$TR.C + mm_sdbi$TR.C
    #    mm_p_bi$TR.M    <- mm_p_bi$TR.M + mm_sdbi$TR.M
    #    mm_p_bi$TR.L    <- mm_p_bi$TR.L + mm_sdbi$TR.L
    
        mm_p_bi$ADD     <- mm_p_bi$ADD + mm_sdbi$ADD
        mm_p_bi$AMB     <- mm_p_bi$AMB + mm_sdbi$AMB
        mm_p_bi$CFB     <- mm_p_bi$CFB + mm_sdbi$CFB
    #    mm_p_bi$FMTE    <- mm_p_bi$FMTE + mm_sdbi$FMTE
    #    mm_p_bi$FMTI    <- mm_p_bi$FMTI + mm_sdbi$FMTI
        mm_p_bi$FTE     <- mm_p_bi$FTE + mm_sdbi$FTE
        mm_p_bi$FTI.Fan <- mm_p_bi$FTI.Fan + mm_sdbi$FTI.Fan
        mm_p_bi$FTI2    <- mm_p_bi$FTI2 + mm_sdbi$FTI2
        mm_p_bi$IFM     <- mm_p_bi$IFM + mm_sdbi$IFM
        mm_p_bi$ILF     <- mm_p_bi$ILF + mm_sdbi$ILF
        mm_p_bi$ITB     <- mm_p_bi$ITB + mm_sdbi$ITB
        mm_p_bi$PIFE    <- mm_p_bi$PIFE + mm_sdbi$PIFE
        mm_p_bi$PIFI    <- mm_p_bi$PIFI + mm_sdbi$PIFI
    
    # new sum
    mm_p_bi$SUM <- mm_p_bi$SUM + mm_sdbi$SUM;
  }
  
  mm_m_bi <- mm_bi
  {
    ##column names 4    
    #    mm_m_bi$BI      <- mm_m_bi$BI - mm_sdbi$BI
    #    mm_m_bi$BR      <- mm_m_bi$BR - mm_sdbi$BR
    #    mm_m_bi$CB      <- mm_m_bi$CB - mm_sdbi$CB
    #    mm_m_bi$DS      <- mm_m_bi$DS - mm_sdbi$DS
    #    mm_m_bi$ECR     <- mm_m_bi$ECR - mm_sdbi$ECR
    #    mm_m_bi$ECU     <- mm_m_bi$ECU - mm_sdbi$ECU
    #    mm_m_bi$EDL     <- mm_m_bi$EDL - mm_sdbi$EDL
    #    mm_m_bi$FCR     <- mm_m_bi$FCR - mm_sdbi$FCR
    #    mm_m_bi$FCU     <- mm_m_bi$FCU - mm_sdbi$FCU
    #    mm_m_bi$FDL     <- mm_m_bi$FDL - mm_sdbi$FDL
    #    mm_m_bi$FDL.U   <- mm_m_bi$FDL.U - mm_sdbi$FDL.U
    #    mm_m_bi$HR      <- mm_m_bi$HR - mm_sdbi$HR
    #    mm_m_bi$LD      <- mm_m_bi$LD - mm_sdbi$LD
    #    mm_m_bi$PECT     <- mm_m_bi$PECT - mm_sdbi$PECT 
    #    mm_m_bi$PT      <- mm_m_bi$PT - mm_sdbi$PT
    #    mm_m_bi$SHA     <- mm_m_bi$SHA - mm_sdbi$SHA
    #    mm_m_bi$SHP     <- mm_m_bi$SHP - mm_sdbi$SHP
    #    mm_m_bi$SUBS    <- mm_m_bi$SUBS - mm_sdbi$SUBS
    #    mm_m_bi$SUP     <- mm_m_bi$SUP - mm_sdbi$SUP
    #    mm_m_bi$SUPC    <- mm_m_bi$SUPC - mm_sdbi$SUPC
    #    mm_m_bi$TM      <- mm_m_bi$TM - mm_sdbi$TM
    #    mm_m_bi$TR.S    <- mm_m_bi$TR.S - mm_sdbi$TR.S
    #    mm_m_bi$TR.C    <- mm_m_bi$TR.C - mm_sdbi$TR.C
    #    mm_m_bi$TR.M    <- mm_m_bi$TR.M - mm_sdbi$TR.M
    #    mm_m_bi$TR.L    <- mm_m_bi$TR.L - mm_sdbi$TR.L
    
        mm_m_bi$ADD     <- mm_m_bi$ADD - mm_sdbi$ADD
        mm_m_bi$AMB     <- mm_m_bi$AMB - mm_sdbi$AMB
        mm_m_bi$CFB     <- mm_m_bi$CFB - mm_sdbi$CFB
    #    mm_m_bi$FMTE    <- mm_m_bi$FMTE - mm_sdbi$FMTE
    #    mm_m_bi$FMTI    <- mm_m_bi$FMTI - mm_sdbi$FMTI
        mm_m_bi$FTE     <- mm_m_bi$FTE - mm_sdbi$FTE
        mm_m_bi$FTI.Fan <- mm_m_bi$FTI.Fan - mm_sdbi$FTI.Fan
        mm_m_bi$FTI2    <- mm_m_bi$FTI2 - mm_sdbi$FTI2
        mm_m_bi$IFM     <- mm_m_bi$IFM - mm_sdbi$IFM
        mm_m_bi$ILF     <- mm_m_bi$ILF - mm_sdbi$ILF
        mm_m_bi$ITB     <- mm_m_bi$ITB - mm_sdbi$ITB
        mm_m_bi$PIFE    <- mm_m_bi$PIFE - mm_sdbi$PIFE
        mm_m_bi$PIFI    <- mm_m_bi$PIFI - mm_sdbi$PIFI
    
    #new sum
    mm_m_bi$SUM <- mm_m_bi$SUM - mm_sdbi$SUM;
  }
  

##Plotting##
library(ggplot2)
Time <- mm_bu$time
Moment_bu    <- mm_bu$SUM
Moment_bu2   <- mm_m_bu$SUM
Moment_bu3   <- mm_p_bu$SUM
Moment_bi    <- mm_bi$SUM
Moment_bi2   <- mm_m_bi$SUM
Moment_bi3   <- mm_p_bi$SUM
zero_line    <- zero$SUM
exname <- "Moment Figures/Sensitivity/"
data <- data.frame (Time,Moment_bu,Moment_bu2,Moment_bu3,
                    Moment_bi,Moment_bi2,Moment_bi3)

ggplot(data,aes(x=Time)) +
    geom_line(aes(y=zero_line,colour="black"), size=1) +
    geom_ribbon(aes(ymin=Moment_bu2,ymax=Moment_bu3),fill="red",alpha=0.5) +
    geom_line(aes(y=Moment_bu,colour="red"), size=2) +
    geom_ribbon(aes(ymin=Moment_bi2,ymax=Moment_bi3),fill="blue",alpha=0.5) +
    geom_line(aes(y=Moment_bi,colour="blue"), size=2) +
    labs(x="Time", y="Summed Muscle Moments") +
    ggtitle(paste0(ti))+
  theme(plot.title=element_text(hjust=0.5))+
    scale_fill_identity(name='Legend',guide='legend',labels=c('error'))+
    scale_color_manual(name='Launch',values=c('red'='#D81B60','blue'='#1E88E5'), 
                       labels=c(l1,l2))
ggsave((paste0(exname,ti,vary,".pdf")), plot = last_plot(), device = NULL, path = NULL, scale = 1, width = 11.2, height = 7.29, dpi = 300)
}

## loops change the joint using ctrl +f to rename them all will also have to manually adjust the column names 1-6
## e.g. to change from Knee to the Hip use ctrl+f and replace Knee with Hip do not use " or "_ just use the word

graph.loop (sc1 = "0_67/", sc2 = "0_73/", j = "Hip", df = "_ABAD", mass = "_m1", ti= "Hip Abduction", 
            vary =" scaling",l1 = 'Isometric', l2 = 'Allometric')
