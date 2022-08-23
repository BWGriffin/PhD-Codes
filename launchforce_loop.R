# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/Impulse");

#Before running make sure to comment out line 36 for bipedal launches
launchforce.loop <- function(sc,launch,mass,weight){

  #bone lengths (m)
  humerus <- 0.29
  ulna    <- 0.39
  carpals <- 0.039
  wmc     <- 0.285
  femur   <- 0.277
  tibia   <- 0.338
  pes     <- 0.057
  
  #load csv
  moment <- read.csv((paste0("Moments/",sc,"_",launch,"_",mass,"_","moments.csv")), header = TRUE, sep = ",", quote =
                              "\"",dec = ".", fill = TRUE, comment.char = "")
  angles <- read.csv((paste0(launch,"_angles.csv")), header = TRUE, sep = ",", quote =
                       "\"",dec = ".", fill = TRUE, comment.char = "")
  
  #moments to forces
  forces          <- moment
  forces$sh       <- forces$sh / humerus
  forces$el       <- forces$el / ulna
  forces$wr       <- forces$wr / carpals
  forces$wm       <- forces$wm / wmc
  forces$hi       <- forces$hi / femur
  forces$kn       <- forces$kn / tibia
  forces$an_bird  <- forces$an_bird / pes
  forces$an_croc            <- forces$an_croc / pes
  forces$forelimb_sum       <- forces$sh + forces$el + forces$wr + forces$wm
  forces$hindlimb_sum_bird  <- forces$hi + forces$kn + forces$an_bird
  forces$hindlimb_sum_croc  <- forces$hi + forces$kn + forces$an_croc
  #zero out post crouch phase for quad
  forces[forces$X %in% c(16:20), c("hindlimb_sum_bird","hindlimb_sum_croc")] = 0
  
  #delta x
  angles$deltaX_fore <- angles$WMC_r_X - angles$WP1_r_X
  angles$deltaX_hind <- angles$tibia_r_X - angles$distal_tibia_r_X
  angles             <- abs(angles) #made absolute values as it is the difference
  
  #angles
  angles$theta_fore    <- asin((angles$deltaX_fore/wmc))
  angles$theta_hind    <- asin((angles$deltaX_hind/tibia))
  angles$captheta_fore <- (pi/2)-angles$theta_fore
  angles$captheta_hind <- (pi/2)-angles$theta_hind

  #GRF
  angles$grf_fore       <- forces$forelimb_sum * cos(angles$captheta_fore)
  angles$grf_bird       <- forces$hindlimb_sum_bird * cos(angles$captheta_hind)
  angles$grf_croc       <- forces$hindlimb_sum_croc * cos(angles$captheta_hind)
  angles$total_grf_bird <- (angles$grf_fore * 2) + (angles$grf_bird *2)
  angles$total_grf_croc <- (angles$grf_fore * 2) + (angles$grf_croc *2)
  
  #less pterosaur weight
  angles$accel_force_bird <- angles$total_grf_bird - weight
  angles$accel_force_croc <- angles$total_grf_croc - weight


  
  #create forces and working outputs
  write.csv(angles,(paste0("Working/",sc,"_",launch,"_",mass,"_","working.csv")))
  write.csv(forces,(paste0("Working/",sc,"_",launch,"_",mass,"_","working_forces.csv")))
  
  output <- data.frame(angles$time,angles$total_grf_bird,angles$total_grf_croc,angles$accel_force_bird,angles$accel_force_croc)
  write.csv(output,(paste0("Forces/",sc,"_",launch,"_",mass,"_","forces.csv")))
    
}
#Quad loops

launchforce.loop (sc = "0_73", launch = "Quad", mass = "m1", weight = 289.395)
launchforce.loop (sc = "0_73", launch = "Quad", mass = "m1b", weight = 289.395)
launchforce.loop (sc = "0_73", launch = "Quad", mass = "m1c", weight = 289.395)
launchforce.loop (sc = "0_73", launch = "Quad", mass = "m2", weight = 303.129)
launchforce.loop (sc = "0_73", launch = "Quad", mass = "m2b", weight = 303.129)
launchforce.loop (sc = "0_73", launch = "Quad", mass = "m2c", weight = 303.129)
launchforce.loop (sc = "0_73", launch = "Quad", mass = "m3", weight = 231.516)
launchforce.loop (sc = "0_73", launch = "Quad", mass = "m3b", weight = 231.516)
launchforce.loop (sc = "0_73", launch = "Quad", mass = "m3c", weight = 231.516)

launchforce.loop (sc = "0_67", launch = "Quad", mass = "m1", weight = 289.395)
launchforce.loop (sc = "0_67", launch = "Quad", mass = "m1b", weight = 289.395)
launchforce.loop (sc = "0_67", launch = "Quad", mass = "m1c", weight = 289.395)
launchforce.loop (sc = "0_67", launch = "Quad", mass = "m2", weight = 303.129)
launchforce.loop (sc = "0_67", launch = "Quad", mass = "m2b", weight = 303.129)
launchforce.loop (sc = "0_67", launch = "Quad", mass = "m2c", weight = 303.129)
launchforce.loop (sc = "0_67", launch = "Quad", mass = "m3", weight = 231.516)
launchforce.loop (sc = "0_67", launch = "Quad", mass = "m3b", weight = 231.516)
launchforce.loop (sc = "0_67", launch = "Quad", mass = "m3c", weight = 231.516)

#bipedal loops - comment line 36 of loop
# counter
# launchforce.loop (sc = "0_73", launch = "Biped", mass = "m1", weight = 289.395)
# launchforce.loop (sc = "0_73", launch = "Biped", mass = "m1b", weight = 289.395)
# launchforce.loop (sc = "0_73", launch = "Biped", mass = "m1c", weight = 289.395)
# launchforce.loop (sc = "0_73", launch = "Biped", mass = "m2", weight = 303.129)
# launchforce.loop (sc = "0_73", launch = "Biped", mass = "m2b", weight = 303.129)
# launchforce.loop (sc = "0_73", launch = "Biped", mass = "m2c", weight = 303.129)
# launchforce.loop (sc = "0_73", launch = "Biped", mass = "m3", weight = 231.516)
# launchforce.loop (sc = "0_73", launch = "Biped", mass = "m3b", weight = 231.516)
# launchforce.loop (sc = "0_73", launch = "Biped", mass = "m3c", weight = 231.516)
# 
# launchforce.loop (sc = "0_67", launch = "Biped", mass = "m1", weight = 289.395)
# launchforce.loop (sc = "0_67", launch = "Biped", mass = "m1b", weight = 289.395)
# launchforce.loop (sc = "0_67", launch = "Biped", mass = "m1c", weight = 289.395)
# launchforce.loop (sc = "0_67", launch = "Biped", mass = "m2", weight = 303.129)
# launchforce.loop (sc = "0_67", launch = "Biped", mass = "m2b", weight = 303.129)
# launchforce.loop (sc = "0_67", launch = "Biped", mass = "m2c", weight = 303.129)
# launchforce.loop (sc = "0_67", launch = "Biped", mass = "m3", weight = 231.516)
# launchforce.loop (sc = "0_67", launch = "Biped", mass = "m3b", weight = 231.516)
# launchforce.loop (sc = "0_67", launch = "Biped", mass = "m3c", weight = 231.516)
# 
# # burst
# launchforce.loop (sc = "0_73", launch = "Burst", mass = "m1", weight = 289.395)
# launchforce.loop (sc = "0_73", launch = "Burst", mass = "m1b", weight = 289.395)
# launchforce.loop (sc = "0_73", launch = "Burst", mass = "m1c", weight = 289.395)
# launchforce.loop (sc = "0_73", launch = "Burst", mass = "m2", weight = 303.129)
# launchforce.loop (sc = "0_73", launch = "Burst", mass = "m2b", weight = 303.129)
# launchforce.loop (sc = "0_73", launch = "Burst", mass = "m2c", weight = 303.129)
# launchforce.loop (sc = "0_73", launch = "Burst", mass = "m3", weight = 231.516)
# launchforce.loop (sc = "0_73", launch = "Burst", mass = "m3b", weight = 231.516)
# launchforce.loop (sc = "0_73", launch = "Burst", mass = "m3c", weight = 231.516)
# 
# launchforce.loop (sc = "0_67", launch = "Burst", mass = "m1", weight = 289.395)
# launchforce.loop (sc = "0_67", launch = "Burst", mass = "m1b", weight = 289.395)
# launchforce.loop (sc = "0_67", launch = "Burst", mass = "m1c", weight = 289.395)
# launchforce.loop (sc = "0_67", launch = "Burst", mass = "m2", weight = 303.129)
# launchforce.loop (sc = "0_67", launch = "Burst", mass = "m2b", weight = 303.129)
# launchforce.loop (sc = "0_67", launch = "Burst", mass = "m2c", weight = 303.129)
# launchforce.loop (sc = "0_67", launch = "Burst", mass = "m3", weight = 231.516)
# launchforce.loop (sc = "0_67", launch = "Burst", mass = "m3b", weight = 231.516)
# launchforce.loop (sc = "0_67", launch = "Burst", mass = "m3c", weight = 231.516)
