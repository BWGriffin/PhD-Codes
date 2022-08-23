# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/Impulse");

# read in csvs for graphs


launch.lines <- function (sc, mas) {
  
  quad   <- read.csv((paste0("Outputs/variable_speed/Quad2/",sc,"_Quad_",mas,"_45_mm_graph.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadb   <- read.csv((paste0("Outputs/variable_speed/Quad2/",sc,"_Quad_",mas,"b_45_mm_graph.csv")), header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quad2x   <- read.csv((paste0("Outputs/variable_speed/Quad2/",sc,"_Quad_",mas,"_2x_45_mm_graph.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadb2x   <- read.csv((paste0("Outputs/variable_speed/Quad2/",sc,"_Quad_",mas,"b_2x_45_mm_graph.csv")), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quad25x   <- read.csv((paste0("Outputs/variable_speed/Quad2/",sc,"_Quad_",mas,"_2.5x_45_mm_graph.csv")), header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadb25x   <- read.csv((paste0("Outputs/variable_speed/Quad2/",sc,"_Quad_",mas,"b_2.5x_45_mm_graph.csv")), header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quad3x   <- read.csv((paste0("Outputs/variable_speed/Quad2/",sc,"_Quad_",mas,"_3x_45_mm_graph.csv")), header = TRUE, sep = ",", quote =
                         "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadb3x   <- read.csv((paste0("Outputs/variable_speed/Quad2/",sc,"_Quad_",mas,"b_3x_45_mm_graph.csv")), header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  
  quadcr   <- read.csv((paste0("Outputs/variable_speed/Quad2/",sc,"_Quad_",mas,"c_45_mm_graph.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")
  quadcr2x   <- read.csv((paste0("Outputs/variable_speed/Quad2/",sc,"_Quad_",mas,"c_2x_45_mm_graph.csv")), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  quadcr25x   <- read.csv((paste0("Outputs/variable_speed/Quad2/",sc,"_Quad_",mas,"c_2.5x_45_mm_graph.csv")), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  quadcr3x   <- read.csv((paste0("Outputs/variable_speed/Quad2/",sc,"_Quad_",mas,"c_3x_45_mm_graph.csv")), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  
  ##Plotting##
  library(ggplot2)
  
  data <- data.frame (quad[c(1:200),2:3],quadb[c(1:200),2:3],quad2x[c(1:200),2:3],quadb2x[c(1:200),2:3],
                      quad25x[c(1:200),2:3],quadb25x[c(1:200),2:3],quad3x[c(1:200),2:3],quadb3x[c(1:200),2:3],
                      quadcr[c(1:200),2:3],quadcr2x[c(1:200),2:3],quadcr25x[c(1:200),2:3],quadcr3x[c(1:200),2:3])
  
  ggplot() +

    geom_line(data = data, aes(x=force.time, y=force.culmdist,colour="Overall EPB Aerobic"), size=1) +
    geom_line(data = data, aes(x=force.time.1, y=force.culmdist.1,colour="Bird EPB Aerobic"), size=1) +
    geom_line(data = data, aes(x=force.time.2, y=force.culmdist.2,colour="Overall EPB 2x"), size=1, linetype = "twodash") +
    geom_line(data = data, aes(x=force.time.3, y=force.culmdist.3,colour="Bird EPB 2x"), size=1, linetype = "twodash") +
    geom_line(data = data, aes(x=force.time.4, y=force.culmdist.4,colour="Overall EPB 2.5x"), size=1, linetype = "dashed") +
    geom_line(data = data, aes(x=force.time.5, y=force.culmdist.5,colour="Bird EPB 2.5x"), size=1, linetype = "dashed") +
    geom_line(data = data, aes(x=force.time.6, y=force.culmdist.6,colour="Overall EPB 3x"), size=1, linetype = "dotted") +
    geom_line(data = data, aes(x=force.time.7, y=force.culmdist.7,colour="Bird EPB 3x"), size=1, linetype = "dotted") +
    geom_line(data = data, aes(x=force.time.8, y=force.culmdist.8,colour="Croc EPB Aerobic"), size=1) +
    geom_line(data = data, aes(x=force.time.9, y=force.culmdist.9,colour="Croc EPB 2x"), size=1, linetype = "twodash") +
    geom_line(data = data, aes(x=force.time.10, y=force.culmdist.10,colour="Croc EPB 2.5x"), size=1,linetype = "dashed") +
    geom_line(data = data, aes(x=force.time.11, y=force.culmdist.11,colour="Croc EPB 3x"), size=1,linetype = "dotted") +
    geom_hline(yintercept = 0.78) +
    geom_hline(yintercept = 1.56, linetype = "longdash", alpha = 0.5) +
    ylim(0,4) +
    xlim(0,1) +
    labs(title = (paste0("Quadrupedal Launch ",sc," ",mas)), x="Time", y="Glenoid Height", colour = "Model") +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave((paste0("Outputs/figures/launch lines/quad_",sc,mas," launch lines.pdf")), plot = last_plot(), device = NULL, path = NULL, scale = 1, dpi = 600)
}


#run plotting function
launch.lines (sc = "0_73", mas = "m1")
launch.lines (sc = "0_73", mas = "m2")
launch.lines (sc = "0_73", mas = "m3")
launch.lines (sc = "0_67", mas = "m1")
launch.lines (sc = "0_67", mas = "m2")
launch.lines (sc = "0_67", mas = "m3")