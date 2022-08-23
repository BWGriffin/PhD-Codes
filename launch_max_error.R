# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/Impulse");

oeribbon <- function(lal, t1, lh){

#load averaged csv  
    
  o1   <- read.csv((paste0("Outputs/variable_speed/Overall/",lal,"o1.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")
  b1   <- read.csv((paste0("Outputs/variable_speed/Overall/",lal,"b1.csv")), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  c1   <- read.csv((paste0("Outputs/variable_speed/Overall/",lal,"c1.csv")), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  o2   <- read.csv((paste0("Outputs/variable_speed/Overall/",lal,"o2.csv")), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  b2   <- read.csv((paste0("Outputs/variable_speed/Overall/",lal,"b2.csv")), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  c2   <- read.csv((paste0("Outputs/variable_speed/Overall/",lal,"c2.csv")), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  o25   <- read.csv((paste0("Outputs/variable_speed/Overall/",lal,"o2.5.csv")), header = TRUE, sep = ",", quote =
                    "\"",dec = ".", fill = TRUE, comment.char = "")
  b25   <- read.csv((paste0("Outputs/variable_speed/Overall/",lal,"b2.5.csv")), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  c25   <- read.csv((paste0("Outputs/variable_speed/Overall/",lal,"c2.5.csv")), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  
  o3   <- read.csv((paste0("Outputs/variable_speed/Overall/",lal,"o3.csv")), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  b3   <- read.csv((paste0("Outputs/variable_speed/Overall/",lal,"b3.csv")), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")
  c3   <- read.csv((paste0("Outputs/variable_speed/Overall/",lal,"c3.csv")), header = TRUE, sep = ",", quote =
                     "\"",dec = ".", fill = TRUE, comment.char = "")

  ##Plotting##
  library(ggplot2)

  ggplot() +
    geom_ribbon(data = o1, aes(x=mean.t,   ymax=max.h,   ymin=min.h,  colour="Overall EPB Aerobic", fill="Overall EPB Aerobic"), size=1, alpha = 0.25, colour = NA) +
    geom_line(data = o1, aes(x=mean.t,   y=mean.h,  colour="Overall EPB Aerobic"), size=1) +
    geom_ribbon(data = b1, aes(x=mean.t,   ymax=max.h,   ymin=min.h,  colour="Bird EPB Aerobic",    fill="Bird EPB Aerobic"), size=1, alpha = 0.25, colour = NA) +
    geom_line(data = b1, aes(x=mean.t,   y=mean.h,  colour="Bird EPB Aerobic"), size=1) +
    geom_ribbon(data = c1, aes(x=mean.t,   ymax=max.h,   ymin=min.h,  colour="Croc EPB Aerobic", fill="Croc EPB Aerobic"), size=1, alpha = 0.25, colour = NA) +
    geom_line(data = c1, aes(x=mean.t,   y=mean.h,  colour="Croc EPB Aerobic"), size=1) +
    geom_ribbon(data = o2, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Overall EPB 2x",      fill="Overall EPB 2x"), size=1, linetype = "twodash", alpha = 0.25, colour = NA) +
    geom_line(data = o2, aes(x=mean.t,   y=mean.h, colour="Overall EPB 2x"), size=1, linetype = "twodash") +
    geom_ribbon(data = b2, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Bird EPB 2x",         fill="Bird EPB 2x"), size=1, linetype = "twodash", alpha = 0.25, colour = NA) +
    geom_line(data = b2, aes(x=mean.t,   y=mean.h, colour="Bird EPB 2x"), size=1, linetype = "twodash") +
    geom_ribbon(data = c2, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Croc EPB 2x",       fill="Croc EPB 2x"), size=1, linetype = "twodash", alpha = 0.25, colour = NA) +
    geom_line(data = c2, aes(x=mean.t,   y=mean.h, colour="Croc EPB 2x"), size=1, linetype = "twodash") +
    geom_ribbon(data = o25, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Overall EPB 2.5x",    fill="Overall EPB 2.5x"), size=1, linetype = "dashed", alpha = 0.25, colour = NA) +
    geom_line(data = o25, aes(x=mean.t,   y=mean.h, colour="Overall EPB 2.5x"), size=1, linetype = "dashed") +
    geom_ribbon(data = b25, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Bird EPB 2.5x",       fill="Bird EPB 2.5x"), size=1, linetype = "dashed", alpha = 0.25, colour = NA) +
    geom_line(data = b25, aes(x=mean.t,   y=mean.h, colour="Bird EPB 2.5x"), size=1, linetype = "dashed") +
    geom_ribbon(data = c25, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Croc EPB 2.5x",     fill="Croc EPB 2.5x"), size=1, linetype = "dashed", alpha = 0.25, colour = NA) +
    geom_line(data = c25, aes(x=mean.t,   y=mean.h, colour="Croc EPB 2.5x"), size=1, linetype = "dashed") +
    geom_ribbon(data = o3, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Overall EPB 3x",      fill="Overall EPB 3x"), size=1, linetype = "dotted", alpha = 0.25, colour = NA) +
    geom_line(data = o3, aes(x=mean.t,   y=mean.h, colour="Overall EPB 3x"), size=1, linetype = "dotted") +
    geom_ribbon(data = b3, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Bird EPB 3x",         fill="Bird EPB 3x"), size=1, linetype = "dotted", alpha = 0.25, colour = NA) +
    geom_line(data = b3, aes(x=mean.t,   y=mean.h, colour="Bird EPB 3x"), size=1, linetype = "dotted") +
    geom_ribbon(data = c3, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Croc EPB 3x",       fill="Croc EPB 3x"), size=1, linetype = "dotted", alpha = 0.25, colour = NA) +
    geom_line(data = c3, aes(x=mean.t,   y=mean.h, colour="Croc EPB 3x"), size=1, linetype = "dotted") +
    geom_hline(yintercept = 1.56, linetype = "longdash", alpha = 0.25) +
    geom_hline(yintercept = lh) +
    coord_cartesian(ylim=c(0,6),xlim=c(0,1.5)) +
    labs(title = (paste0(t1," Launch")), x="Time", y="Glenoid Height", colour = "Model", fill = "Model") +
    theme(plot.title = element_text(hjust = 0.25))

  ggsave((paste0("Outputs/figures/launch lines/",t1," max error launch ribbon.pdf")), plot = last_plot(), device = NULL, path = NULL, scale = 1, dpi = 600)
  
  ggplot() +
    geom_ribbon(data = o1, aes(x=mean.t,   ymax=max.h,   ymin=min.h,  colour="Overall EPB Aerobic", fill="Overall EPB Aerobic"), size=1, alpha = 0.25, colour = NA) +
    geom_line(data = o1, aes(x=mean.t,   y=mean.h,  colour="Overall EPB Aerobic"), size=1) +  
    geom_ribbon(data = o2, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Overall EPB 2x",      fill="Overall EPB 2x"), size=1, linetype = "twodash", alpha = 0.25, colour = NA) +
    geom_line(data = o2, aes(x=mean.t,   y=mean.h, colour="Overall EPB 2x"), size=1, linetype = "twodash") +
    geom_ribbon(data = o25, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Overall EPB 2.5x",    fill="Overall EPB 2.5x"), size=1, linetype = "dashed", alpha = 0.25, colour = NA) +
    geom_line(data = o25, aes(x=mean.t,   y=mean.h, colour="Overall EPB 2.5x"), size=1, linetype = "dashed") +
    geom_ribbon(data = o3, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Overall EPB 3x",      fill="Overall EPB 3x"), size=1, linetype = "dotted", alpha = 0.25, colour = NA) +
    geom_line(data = o3, aes(x=mean.t,   y=mean.h, colour="Overall EPB 3x"), size=1, linetype = "dotted") +
    geom_hline(yintercept = 1.56, linetype = "longdash", alpha = 0.25) +
    geom_hline(yintercept = lh) +
    coord_cartesian(ylim=c(0,6),xlim=c(0,1.5)) +
    labs(title = (paste0(t1," Overall EPB Launch")), x="Time", y="Glenoid Height", colour = "Model", fill = "Model") +
    theme(plot.title = element_text(hjust = 0.25))

  ggsave((paste0("Outputs/figures/launch lines/",t1," max error launch ribbon overall only.pdf")), plot = last_plot(), device = NULL, path = NULL, scale = 1, dpi = 600)
  
  ggplot() +
    geom_ribbon(data = b1, aes(x=mean.t,   ymax=max.h,   ymin=min.h,  colour="Bird EPB Aerobic",    fill="Bird EPB Aerobic"), size=1, alpha = 0.25, colour = NA) +
    geom_line(data = b1, aes(x=mean.t,   y=mean.h,  colour="Bird EPB Aerobic"), size=1) +
    geom_ribbon(data = b2, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Bird EPB 2x",         fill="Bird EPB 2x"), size=1, linetype = "twodash", alpha = 0.25, colour = NA) +
    geom_line(data = b2, aes(x=mean.t,   y=mean.h, colour="Bird EPB 2x"), size=1, linetype = "twodash") +
    geom_ribbon(data = b25, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Bird EPB 2.5x",       fill="Bird EPB 2.5x"), size=1, linetype = "dashed", alpha = 0.25, colour = NA) +
    geom_line(data = b25, aes(x=mean.t,   y=mean.h, colour="Bird EPB 2.5x"), size=1, linetype = "dashed") +
    geom_ribbon(data = b3, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Bird EPB 3x",         fill="Bird EPB 3x"), size=1, linetype = "dotted", alpha = 0.25, colour = NA) +
    geom_line(data = b3, aes(x=mean.t,   y=mean.h, colour="Bird EPB 3x"), size=1, linetype = "dotted") +
    geom_hline(yintercept = 1.56, linetype = "longdash", alpha = 0.25) +
    geom_hline(yintercept = lh) +
    coord_cartesian(ylim=c(0,6),xlim=c(0,1.5)) +
    labs(title = (paste0(t1," Bird EPB Launch")), x="Time", y="Glenoid Height", colour = "Model", fill = "Model") +
    theme(plot.title = element_text(hjust = 0.25))
  
  ggsave((paste0("Outputs/figures/launch lines/",t1," max error launch ribbon bird only.pdf")), plot = last_plot(), device = NULL, path = NULL, scale = 1, dpi = 600)
  
  ggplot() +
    geom_ribbon(data = c1, aes(x=mean.t,   ymax=max.h,   ymin=min.h,  colour="Croc EPB Aerobic", fill="Croc EPB Aerobic"), size=1, alpha = 0.25, colour = NA) +
    geom_line(data = c1, aes(x=mean.t,   y=mean.h,  colour="Croc EPB Aerobic"), size=1) +
    geom_ribbon(data = c2, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Croc EPB 2x",       fill="Croc EPB 2x"), size=1, linetype = "twodash", alpha = 0.25, colour = NA) +
    geom_line(data = c2, aes(x=mean.t,   y=mean.h, colour="Croc EPB 2x"), size=1, linetype = "twodash") +
    geom_ribbon(data = c25, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Croc EPB 2.5x",    fill="Croc EPB 2.5x"), size=1, linetype = "dashed", alpha = 0.25, colour = NA) +
    geom_line(data = c25, aes(x=mean.t,   y=mean.h, colour="Croc EPB 2.5x"), size=1, linetype = "dashed") +
    geom_ribbon(data = c3, aes(x=mean.t,   ymax=max.h,   ymin=min.h, colour="Croc EPB 3x",       fill="Croc EPB 3x"), size=1, linetype = "dotted", alpha = 0.25, colour = NA) +
    geom_line(data = c3, aes(x=mean.t,   y=mean.h, colour="Croc EPB 3x"), size=1, linetype = "dotted") +
    geom_hline(yintercept = 1.56, linetype = "longdash", alpha = 0.25) +
    geom_hline(yintercept = lh) +
    coord_cartesian(ylim=c(0,6),xlim=c(0,1.5)) +
    labs(title = (paste0(t1," Crocodile EPB Launch")), x="Time", y="Glenoid Height", colour = "Model", fill = "Model") +
    theme(plot.title = element_text(hjust = 0.25))
  
  ggsave((paste0("Outputs/figures/launch lines/",t1," max error launch ribbon croc only.pdf")), plot = last_plot(), device = NULL, path = NULL, scale = 1, dpi = 600)
  
  }


#run plotting function
oeribbon (lal = "q", t1 = "Mean Quadrupedal", lh = 0.78)
oeribbon (lal = "b", t1 = "Mean Bipedal Burst", lh = 0.98)
oeribbon (lal = "c", t1 = "Mean Bipedal Countermotion", lh = 0.82)

