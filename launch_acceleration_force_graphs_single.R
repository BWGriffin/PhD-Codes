# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/Impulse");


#script to graph the acceleration forces of launches

accelgraphloop <- function(sc,mas){

#read in data
#m1
quad1    <- read.csv((paste0("Forces/",sc,"_Quad_",mas,"_forces.csv")), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")

count1   <- read.csv((paste0("Forces/",sc,"_Biped_",mas,"_forces.csv")), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")

burst1   <- read.csv((paste0("Forces/",sc,"_Burst_",mas,"_forces.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")

quad2    <- read.csv((paste0("Forces/",sc,"_Quad_",mas,"_2x_forces.csv")), header = TRUE, sep = ",", quote =
                      "\"",dec = ".", fill = TRUE, comment.char = "")

count2   <- read.csv((paste0("Forces/",sc,"_Biped_",mas,"_2x_forces.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")

burst2   <- read.csv((paste0("Forces/",sc,"_Burst_",mas,"_2x_forces.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")

quad2_5    <- read.csv((paste0("Forces/",sc,"_Quad_",mas,"_2.5x_forces.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")

count2_5   <- read.csv((paste0("Forces/",sc,"_Biped_",mas,"_2.5x_forces.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")

burst2_5   <- read.csv((paste0("Forces/",sc,"_Burst_",mas,"_2.5x_forces.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")

quad3    <- read.csv((paste0("Forces/",sc,"_Quad_",mas,"_3x_forces.csv")), header = TRUE, sep = ",", quote =
                      "\"",dec = ".", fill = TRUE, comment.char = "")

count3   <- read.csv((paste0("Forces/",sc,"_Biped_",mas,"_3x_forces.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")

burst3   <- read.csv((paste0("Forces/",sc,"_Burst_",mas,"_3x_forces.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")

#m1b


##Plotting##
library(ggplot2)

#CREATE GRAPH DATAFRAMES

df1 <- data.frame(quad1$angles.time,quad1$angles.accel_force_bird,quad1$angles.accel_force_croc,
                 count1$angles.accel_force_bird,count1$angles.accel_force_croc,
                 burst1$angles.accel_force_bird,burst1$angles.accel_force_croc)

df2 <- data.frame(quad2$angles.time,quad2$angles.accel_force_bird,quad2$angles.accel_force_croc,
                  count2$angles.accel_force_bird,count2$angles.accel_force_croc,
                  burst2$angles.accel_force_bird,burst2$angles.accel_force_croc)

df2_5 <- data.frame(quad2_5$angles.time,quad2_5$angles.accel_force_bird,quad2_5$angles.accel_force_croc,
                  count2_5$angles.accel_force_bird,count2_5$angles.accel_force_croc,
                  burst2_5$angles.accel_force_bird,burst2_5$angles.accel_force_croc)

df3 <- data.frame(quad3$angles.time,quad3$angles.accel_force_bird,quad3$angles.accel_force_croc,
                  count3$angles.accel_force_bird,count3$angles.accel_force_croc,
                  burst3$angles.accel_force_bird,burst3$angles.accel_force_croc)
#plot df1
ggplot(data = df1, aes(x=quad1.angles.time))+
  geom_hline(yintercept = 0, size=1) +
  geom_line(aes(y=quad1.angles.accel_force_bird,colour="Quad, bird ankle"), size=1) +
  geom_line(aes(y=quad1.angles.accel_force_croc,colour="Quad, croc ankle"), size=1, linetype="dashed") +
  geom_line(aes(y=count1.angles.accel_force_bird,colour="Counter, bird ankle"), size=1) +
  geom_line(aes(y=count1.angles.accel_force_croc,colour="Counter, croc ankle"), size=1, linetype="dashed") +
  geom_line(aes(y=burst1.angles.accel_force_bird,colour="Burst, bird ankle"), size=1) +
  geom_line(aes(y=burst1.angles.accel_force_croc,colour="Burst, croc ankle"), size=1, linetype="dashed") +
  labs(title = (paste0("Launch Accecleration Forces (assumptions: ",sc," ",mas," aerobic)")), x="Time (s)", y="Acceleration Force (N)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave((paste0("Outputs/figures/acceleration forces/",sc,"_",mas,".pdf")), plot = last_plot(), device = NULL, path = NULL, scale = 1, dpi = 600)
  
#plet df2
ggplot(data = df2, aes(x=quad2.angles.time))+
  geom_hline(yintercept = 0, size=1) +
  geom_line(aes(y=quad2.angles.accel_force_bird,colour="Quad, bird ankle"), size=1) +
  geom_line(aes(y=quad2.angles.accel_force_croc,colour="Quad, croc ankle"), size=1, linetype="dashed") +
  geom_line(aes(y=count2.angles.accel_force_bird,colour="Counter, bird ankle"), size=1) +
  geom_line(aes(y=count2.angles.accel_force_croc,colour="Counter, croc ankle"), size=1, linetype="dashed") +
  geom_line(aes(y=burst2.angles.accel_force_bird,colour="Burst, bird ankle"), size=1) +
  geom_line(aes(y=burst2.angles.accel_force_croc,colour="Burst, croc ankle"), size=1, linetype="dashed") +
  labs(title = (paste0("Launch Accecleration Forces (assumptions: ",sc," ",mas," 2x anaerobic)")), x="Time (s)", y="Acceleration Force (N)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave((paste0("Outputs/figures/acceleration forces/",sc,"_",mas,"_2x.pdf")), plot = last_plot(), device = NULL, path = NULL, scale = 1, dpi = 600)

#plet df2_5
ggplot(data = df2_5, aes(x=quad2_5.angles.time))+
  geom_hline(yintercept = 0, size=1) +
  geom_line(aes(y=quad2_5.angles.accel_force_bird,colour="Quad, bird ankle"), size=1) +
  geom_line(aes(y=quad2_5.angles.accel_force_croc,colour="Quad, croc ankle"), size=1, linetype="dashed") +
  geom_line(aes(y=count2_5.angles.accel_force_bird,colour="Counter, bird ankle"), size=1) +
  geom_line(aes(y=count2_5.angles.accel_force_croc,colour="Counter, croc ankle"), size=1, linetype="dashed") +
  geom_line(aes(y=burst2_5.angles.accel_force_bird,colour="Burst, bird ankle"), size=1) +
  geom_line(aes(y=burst2_5.angles.accel_force_croc,colour="Burst, croc ankle"), size=1, linetype="dashed") +
  labs(title = (paste0("Launch Accecleration Forces (assumptions: ",sc," ",mas," 2.5x anaerobic)")), x="Time (s)", y="Acceleration Force (N)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave((paste0("Outputs/figures/acceleration forces/",sc,"_",mas,"_2x.pdf")), plot = last_plot(), device = NULL, path = NULL, scale = 1, dpi = 600)


#plot df3
ggplot(data = df3, aes(x=quad3.angles.time))+
  geom_hline(yintercept = 0, size=1) +
  geom_line(aes(y=quad3.angles.accel_force_bird,colour="Quad, bird ankle"), size=1) +
  geom_line(aes(y=quad3.angles.accel_force_croc,colour="Quad, croc ankle"), size=1, linetype="dashed") +
  geom_line(aes(y=count3.angles.accel_force_bird,colour="Counter, bird ankle"), size=1) +
  geom_line(aes(y=count3.angles.accel_force_croc,colour="Counter, croc ankle"), size=1, linetype="dashed") +
  geom_line(aes(y=burst3.angles.accel_force_bird,colour="Burst, bird ankle"), size=1) +
  geom_line(aes(y=burst3.angles.accel_force_croc,colour="Burst, croc ankle"), size=1, linetype="dashed") +
  labs(title = (paste0("Launch Accecleration Forces (assumptions: ",sc," ",mas," 3x anaerobic)")), x="Time (s)", y="Acceleration Force (N)") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave((paste0("Outputs/figures/acceleration forces/",sc,"_",mas,"_3x.pdf")), plot = last_plot(), device = NULL, path = NULL, scale = 1, dpi = 600)
}

#run all the things
accelgraphloop(sc = "0_73", mas = "m1")
accelgraphloop(sc = "0_73", mas = "m1b")
accelgraphloop(sc = "0_73", mas = "m1c")
accelgraphloop(sc = "0_73", mas = "m2")
accelgraphloop(sc = "0_73", mas = "m2b")
accelgraphloop(sc = "0_73", mas = "m2c")
accelgraphloop(sc = "0_73", mas = "m3")
accelgraphloop(sc = "0_73", mas = "m3b")
accelgraphloop(sc = "0_73", mas = "m3c")

accelgraphloop(sc = "0_67", mas = "m1")
accelgraphloop(sc = "0_67", mas = "m1b")
accelgraphloop(sc = "0_67", mas = "m1c")
accelgraphloop(sc = "0_67", mas = "m2")
accelgraphloop(sc = "0_67", mas = "m2b")
accelgraphloop(sc = "0_67", mas = "m2c")
accelgraphloop(sc = "0_67", mas = "m3")
accelgraphloop(sc = "0_67", mas = "m3b")
accelgraphloop(sc = "0_67", mas = "m3c")