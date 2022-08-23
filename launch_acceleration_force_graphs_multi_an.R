# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/Impulse");


#script to graph the acceleration forces of launches

accelgraphloop <- function(sc, mas, an, ti, yl){

#read in data
#m1
quad1    <- read.csv((paste0("Forces/",sc,"_Quad_",mas,"_",an,"_forces.csv")), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")

count1   <- read.csv((paste0("Forces/",sc,"_Biped_",mas,"_",an,"_forces.csv")), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")

burst1   <- read.csv((paste0("Forces/",sc,"_Burst_",mas,"_",an,"_forces.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")

quad1b    <- read.csv((paste0("Forces/",sc,"_Quad_",mas,"b_",an,"_forces.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")

count1b   <- read.csv((paste0("Forces/",sc,"_Biped_",mas,"b_",an,"_forces.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")

burst1b   <- read.csv((paste0("Forces/",sc,"_Burst_",mas,"b_",an,"_forces.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")

quad1c    <- read.csv((paste0("Forces/",sc,"_Quad_",mas,"c_",an,"_forces.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")

count1c   <- read.csv((paste0("Forces/",sc,"_Biped_",mas,"c_",an,"_forces.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")

burst1c   <- read.csv((paste0("Forces/",sc,"_Burst_",mas,"c_",an,"_forces.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")




##Plotting##
library(ggplot2)
library(patchwork)

#CREATE GRAPH DATAFRAMES

df1 <- data.frame(quad1$angles.time,quad1$angles.accel_force_bird,quad1$angles.accel_force_croc,
                 count1$angles.accel_force_bird,count1$angles.accel_force_croc,
                 burst1$angles.accel_force_bird,burst1$angles.accel_force_croc)

df1b <- data.frame(quad1b$angles.time,quad1b$angles.accel_force_bird,quad1b$angles.accel_force_croc,
                  count1b$angles.accel_force_bird,count1b$angles.accel_force_croc,
                  burst1b$angles.accel_force_bird,burst1b$angles.accel_force_croc)

df1c <- data.frame(quad1c$angles.time,quad1c$angles.accel_force_bird,quad1c$angles.accel_force_croc,
                  count1c$angles.accel_force_bird,count1c$angles.accel_force_croc,
                  burst1c$angles.accel_force_bird,burst1c$angles.accel_force_croc)


#plot df1
p1 <- ggplot(data = df1, aes(x=quad1.angles.time))+
  geom_hline(yintercept = 0, size=1) +
  geom_line(aes(y=quad1.angles.accel_force_bird,colour="Quad, bird ankle"), size=1) +
  geom_line(aes(y=quad1.angles.accel_force_croc,colour="Quad, croc ankle"), size=1, linetype="dashed") +
  geom_line(aes(y=count1.angles.accel_force_bird,colour="Counter, bird ankle"), size=1) +
  geom_line(aes(y=count1.angles.accel_force_croc,colour="Counter, croc ankle"), size=1, linetype="dashed") +
  geom_line(aes(y=burst1.angles.accel_force_bird,colour="Burst, bird ankle"), size=1) +
  geom_line(aes(y=burst1.angles.accel_force_croc,colour="Burst, croc ankle"), size=1, linetype="dashed") +
  labs(title = (paste0(ti," ",an)), x="", y="Acceleration Force (N)", colour = "Launch") +
  coord_cartesian(ylim=c(-500,yl),xlim=c(0,1)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
p1b <- ggplot(data = df1b, aes(x=quad1b.angles.time))+
  geom_hline(yintercept = 0, size=1) +
  geom_line(aes(y=quad1b.angles.accel_force_bird,colour="Quad, bird ankle"), size=1) +
  geom_line(aes(y=quad1b.angles.accel_force_croc,colour="Quad, croc ankle"), size=1, linetype="dashed") +
  geom_line(aes(y=count1b.angles.accel_force_bird,colour="Counter, bird ankle"), size=1) +
  geom_line(aes(y=count1b.angles.accel_force_croc,colour="Counter, croc ankle"), size=1, linetype="dashed") +
  geom_line(aes(y=burst1b.angles.accel_force_bird,colour="Burst, bird ankle"), size=1) +
  geom_line(aes(y=burst1b.angles.accel_force_croc,colour="Burst, croc ankle"), size=1, linetype="dashed") +
  labs(title = (paste0(ti," Bird EPB ",an)), x="Time (s)", y="", colour = "Launch") +
  coord_cartesian(ylim=c(-500,yl),xlim=c(0,1)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
p1c <- ggplot(data = df1c, aes(x=quad1c.angles.time))+
  geom_hline(yintercept = 0, size=1) +
  geom_line(aes(y=quad1c.angles.accel_force_bird,colour="Quad, bird ankle"), size=1) +
  geom_line(aes(y=quad1c.angles.accel_force_croc,colour="Quad, croc ankle"), size=1, linetype="dashed") +
  geom_line(aes(y=count1c.angles.accel_force_bird,colour="Counter, bird ankle"), size=1) +
  geom_line(aes(y=count1c.angles.accel_force_croc,colour="Counter, croc ankle"), size=1, linetype="dashed") +
  geom_line(aes(y=burst1c.angles.accel_force_bird,colour="Burst, bird ankle"), size=1) +
  geom_line(aes(y=burst1c.angles.accel_force_croc,colour="Burst, croc ankle"), size=1, linetype="dashed") +
  labs(title = (paste0(ti," Crocodile EPB ",an)), x="", y="", colour = "Launch") +
  coord_cartesian(ylim=c(-500,yl),xlim=c(0,1)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none",axis.title.x = element_blank())


combined <- p1 + p1b + p1c & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect") + plot_annotation( title = "Launch Acceleration Forces")

ggsave((paste0("Outputs/figures/acceleration forces/",sc,"_",mas,"_",an,"_combo.pdf")), plot = last_plot(), device = NULL, path = NULL, scale = 1, dpi = 600)

}

#run all the things
accelgraphloop(sc = "0_73", mas = "m1", an = "2x", ti = "Allometric 29.5kg", yl = 4500)
accelgraphloop(sc = "0_73", mas = "m2", an = "2x", ti = "Allometric 30.9kg", yl = 4500)
accelgraphloop(sc = "0_73", mas = "m3", an = "2x", ti = "Allometric 23.6kg", yl = 4500)

accelgraphloop(sc = "0_67", mas = "m1", an = "2x", ti = "Isometric 29.5kg", yl = 4500)
accelgraphloop(sc = "0_67", mas = "m2", an = "2x", ti = "Isometric 30.9kg", yl = 4500)
accelgraphloop(sc = "0_67", mas = "m3", an = "2x", ti = "Isometric 23.6kg", yl = 4500)

accelgraphloop(sc = "0_73", mas = "m1", an = "2.5x", ti = "Allometric 29.5kg", yl = 5500)
accelgraphloop(sc = "0_73", mas = "m2", an = "2.5x", ti = "Allometric 30.9kg", yl = 5500)
accelgraphloop(sc = "0_73", mas = "m3", an = "2.5x", ti = "Allometric 23.6kg", yl = 5500)

accelgraphloop(sc = "0_67", mas = "m1", an = "2.5x", ti = "Isometric 29.5kg", yl = 5500)
accelgraphloop(sc = "0_67", mas = "m2", an = "2.5x", ti = "Isometric 30.9kg", yl = 5500)
accelgraphloop(sc = "0_67", mas = "m3", an = "2.5x", ti = "Isometric 23.6kg", yl = 5500)

accelgraphloop(sc = "0_73", mas = "m1", an = "3x", ti = "Allometric 29.5kg", yl = 6500)
accelgraphloop(sc = "0_73", mas = "m2", an = "3x", ti = "Allometric 30.9kg", yl = 6500)
accelgraphloop(sc = "0_73", mas = "m3", an = "3x", ti = "Allometric 23.6kg", yl = 6500)

accelgraphloop(sc = "0_67", mas = "m1", an = "3x", ti = "Isometric 29.5kg", yl = 6500)
accelgraphloop(sc = "0_67", mas = "m2", an = "3x", ti = "Isometric 30.9kg", yl = 6500)
accelgraphloop(sc = "0_67", mas = "m3", an = "3x", ti = "Isometric 23.6kg", yl = 6500)