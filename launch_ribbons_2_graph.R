# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/Impulse");

# read in csvs for graphs


launch.lines <- function (sc, mas, l1, l2, la, lh, t1) {
  
  quad   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"_",la,"_mm_graph.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadb   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"b_",la,"_mm_graph.csv")), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quad2x   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"_2x_",la,"_mm_graph.csv")), header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadb2x   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"b_2x_",la,"_mm_graph.csv")), header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quad25x   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"_2.5x_",la,"_mm_graph.csv")), header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadb25x   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"b_2.5x_",la,"_mm_graph.csv")), header = TRUE, sep = ",", quote =
                           "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quad3x   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"_3x_",la,"_mm_graph.csv")), header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadb3x   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"b_3x_",la,"_mm_graph.csv")), header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadc   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"_",la,"_mmc_graph.csv")), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadbc   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"b_",la,"_mmc_graph.csv")), header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quad2xc   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"_2x_",la,"_mmc_graph.csv")), header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadb2xc   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"b_2x_",la,"_mmc_graph.csv")), header = TRUE, sep = ",", quote =
                           "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quad25xc   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"_2.5x_",la,"_mmc_graph.csv")), header = TRUE, sep = ",", quote =
                           "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadb25xc   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"b_2.5x_",la,"_mmc_graph.csv")), header = TRUE, sep = ",", quote =
                            "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quad3xc   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"_3x_",la,"_mmc_graph.csv")), header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadb3xc   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"b_3x_",la,"_mmc_graph.csv")), header = TRUE, sep = ",", quote =
                           "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadcr   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"c_",la,"_mm_graph.csv")), header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadcrc   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"c_",la,"_mmc_graph.csv")), header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadcr2x   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"c_2x_",la,"_mm_graph.csv")), header = TRUE, sep = ",", quote =
                           "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadcr2xc   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"c_2x_",la,"_mmc_graph.csv")), header = TRUE, sep = ",", quote =
                            "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadcr25x   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"c_2.5x_",la,"_mm_graph.csv")), header = TRUE, sep = ",", quote =
                            "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadcr25xc   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"c_2.5x_",la,"_mmc_graph.csv")), header = TRUE, sep = ",", quote =
                             "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadcr3x   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"c_3x_",la,"_mm_graph.csv")), header = TRUE, sep = ",", quote =
                           "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadcr3xc   <- read.csv((paste0("Outputs/variable_speed/",l1,"2/",sc,"_",l2,"_",mas,"c_3x_",la,"_mmc_graph.csv")), header = TRUE, sep = ",", quote =
                            "\"",dec = ".", fill = TRUE, comment.char = "")
  
  ##Plotting##
  library(ggplot2)
  
  data <- data.frame (quad[c(1:200),2:3],quadb[c(1:200),2:3],quad2x[c(1:200),2:3],quadb2x[c(1:200),2:3],
                      quad25x[c(1:200),2:3],quadb25x[c(1:200),2:3],quad3x[c(1:200),2:3],quadb3x[c(1:200),2:3], 
                      quadc[c(1:200),2:3],quadbc[c(1:200),2:3],quad2xc[c(1:200),2:3],quadb2xc[c(1:200),2:3],
                      quad25xc[c(1:200),2:3],quadb25xc[c(1:200),2:3],quad3xc[c(1:200),2:3],quadb3xc[c(1:200),2:3],
                      quadcr[c(1:200),2:3],quadcrc[c(1:200),2:3],quadcr2x[c(1:200),2:3],quadcr2xc[c(1:200),2:3],
                      quadcr25x[c(1:200),2:3],quadcr25xc[c(1:200),2:3],quadcr3x[c(1:200),2:3],quadcr3xc[c(1:200),2:3])
  
  ggplot() +
    geom_ribbon(data = data, aes(x=force.time,   ymax=force.culmdist,   ymin=force.culmdist.8,  colour="Overall EPB Aerobic", fill="Overall EPB Aerobic"), size=1, alpha = 0.25) +
    geom_ribbon(data = data, aes(x=force.time.1, ymax=force.culmdist.1, ymin=force.culmdist.9,  colour="Bird EPB Aerobic",    fill="Bird EPB Aerobic"), size=1, alpha = 0.25) +
    geom_ribbon(data = data, aes(x=force.time.2, ymax=force.culmdist.2, ymin=force.culmdist.10, colour="Overall EPB 2x",      fill="Overall EPB 2x"), size=1, linetype = "twodash", alpha = 0.25) +
    geom_ribbon(data = data, aes(x=force.time.3, ymax=force.culmdist.3, ymin=force.culmdist.11, colour="Bird EPB 2x",         fill="Bird EPB 2x"), size=1, linetype = "twodash", alpha = 0.25) +
    geom_ribbon(data = data, aes(x=force.time.4, ymax=force.culmdist.4, ymin=force.culmdist.12, colour="Overall EPB 2.5x",    fill="Overall EPB 2.5x"), size=1, linetype = "dashed", alpha = 0.25) +
    geom_ribbon(data = data, aes(x=force.time.5, ymax=force.culmdist.5, ymin=force.culmdist.13, colour="Bird EPB 2.5x",       fill="Bird EPB 2.5x"), size=1, linetype = "dashed", alpha = 0.25) +
    geom_ribbon(data = data, aes(x=force.time.6, ymax=force.culmdist.6, ymin=force.culmdist.14, colour="Overall EPB 3x",      fill="Overall EPB 3x"), size=1, linetype = "dotted", alpha = 0.25) +
    geom_ribbon(data = data, aes(x=force.time.7, ymax=force.culmdist.7, ymin=force.culmdist.15, colour="Bird EPB 3x",         fill="Bird EPB 3x"), size=1, linetype = "dotted", alpha = 0.25) +
    geom_ribbon(data = data, aes(x=force.time.16, ymax=force.culmdist.16, ymin=force.culmdist.17,  colour="Croc EPB Aerobic", fill="Croc EPB Aerobic"), size=1, alpha = 0.25) +
    geom_ribbon(data = data, aes(x=force.time.18, ymax=force.culmdist.18, ymin=force.culmdist.19, colour="Croc EPB 2x",       fill="Croc EPB 2x"), size=1, linetype = "twodash", alpha = 0.25) +
    geom_ribbon(data = data, aes(x=force.time.20, ymax=force.culmdist.20, ymin=force.culmdist.21, colour="Croc EPB 2.5x",     fill="Croc EPB 2.5x"), size=1, linetype = "dashed", alpha = 0.25) +
    geom_ribbon(data = data, aes(x=force.time.22, ymax=force.culmdist.22, ymin=force.culmdist.23, colour="Croc EPB 3x",       fill="Croc EPB 3x"), size=1, linetype = "dotted", alpha = 0.25) +
    geom_hline(yintercept = 1.56, linetype = "longdash", alpha = 0.25) +
    geom_hline(yintercept = lh) +
    coord_cartesian(ylim=c(0,6),xlim=c(0,1)) +
    labs(title = (paste0(t1," Launch")), x="Time", y="Glenoid Height", colour = "Model", fill = "Model") +
    theme(plot.title = element_text(hjust = 0.25))
  
  ggsave((paste0("Outputs/figures/launch lines/",l1,sc,mas," launch ribbon.pdf")), plot = last_plot(), device = NULL, path = NULL, scale = 1, dpi = 600)
}


#run plotting function
launch.lines (sc = "0_73", mas = "m1", l1 = "Quad", l2 = "Quad", la = "45", lh = 0.78, t1 = "Quadrupedal Allometric 29.5kg")
launch.lines (sc = "0_73", mas = "m2", l1 = "Quad", l2 = "Quad", la = "45", lh = 0.78, t1 = "Quadrupedal Allometric 30.9kg")
launch.lines (sc = "0_73", mas = "m3", l1 = "Quad", l2 = "Quad", la = "45", lh = 0.78, t1 = "Quadrupedal Allometric 23.6kg")
launch.lines (sc = "0_67", mas = "m1", l1 = "Quad", l2 = "Quad", la = "45", lh = 0.78, t1 = "Quadrupedal Isometric 29.5kg")
launch.lines (sc = "0_67", mas = "m2", l1 = "Quad", l2 = "Quad", la = "45", lh = 0.78, t1 = "Quadrupedal Isometric 30.9kg")
launch.lines (sc = "0_67", mas = "m3", l1 = "Quad", l2 = "Quad", la = "45", lh = 0.78, t1 = "Quadrupedal Isometric 23.6kg")

launch.lines (sc = "0_73", mas = "m1", l1 = "Counter", l2 = "Biped", la = "45", lh = 0.82, t1 = "Bipedal Countermotion Allometric 29.5kg")
launch.lines (sc = "0_73", mas = "m2", l1 = "Counter", l2 = "Biped", la = "45", lh = 0.82, t1 = "Bipedal Countermotion Allometric 30.9kg")
launch.lines (sc = "0_73", mas = "m3", l1 = "Counter", l2 = "Biped", la = "45", lh = 0.82, t1 = "Bipedal Countermotion Allometric 23.6kg")
launch.lines (sc = "0_67", mas = "m1", l1 = "Counter", l2 = "Biped", la = "45", lh = 0.82, t1 = "Bipedal Countermotion Isometric 29.5kg")
launch.lines (sc = "0_67", mas = "m2", l1 = "Counter", l2 = "Biped", la = "45", lh = 0.82, t1 = "Bipedal Countermotion Isometric 30.9kg")
launch.lines (sc = "0_67", mas = "m3", l1 = "Counter", l2 = "Biped", la = "45", lh = 0.82, t1 = "Bipedal Countermotion Isometric 23.6kg")

launch.lines (sc = "0_73", mas = "m1", l1 = "Burst", l2 = "Burst", la = "80", lh = 0.98, t1 = "Bipedal Burst Allometric 29.5kg")
launch.lines (sc = "0_73", mas = "m2", l1 = "Burst", l2 = "Burst", la = "80", lh = 0.98, t1 = "Bipedal Burst Allometric 30.9kg")
launch.lines (sc = "0_73", mas = "m3", l1 = "Burst", l2 = "Burst", la = "80", lh = 0.98, t1 = "Bipedal Burst Allometric 23.6kg")
launch.lines (sc = "0_67", mas = "m1", l1 = "Burst", l2 = "Burst", la = "80", lh = 0.98, t1 = "Bipedal Burst Isometric 29.5kg")
launch.lines (sc = "0_67", mas = "m2", l1 = "Burst", l2 = "Burst", la = "80", lh = 0.98, t1 = "Bipedal Burst Isometric 30.9kg")
launch.lines (sc = "0_67", mas = "m3", l1 = "Burst", l2 = "Burst", la = "80", lh = 0.98, t1 = "Bipedal Burst Isometric 23.6kg")