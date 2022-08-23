# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/Impulse");

##launch height calculator check comment out line 16 and 20
launchcalc <- function (anaero, mass, l_profile, forces, glen_end, glen_start, l_angle){
  
  profile <- read.csv(paste0(l_profile,"_Profile.csv"), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")  
  
  peak_data <- read.csv(paste0("Forces/", forces,"_",anaero,"_forces.csv"), header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  
  #t_base
  t_ <<- 0.01  
  #quad and burst 
  # <<- _data[10,5]
  
  #countermotion 
  # <- _data[16,5]
  
  #create force dataframe
  weight <- mass * 9.81
  force <- data.frame(profile$Normalised)
  names(force)[names(force)=='profile.Normalised'] <- 'force'
  force$force[1:20] <- peak_data$angles.accel_force_bird
  force$force[21:200] <- 0
  
  
  
  #function to do calculations in the forces dataframe
  forcecalcs <<- function (t_value){
    #time base
    timebase <- read.csv("20timebase.csv", header = TRUE, sep = ",", quote =
                           "\"",dec = ".", fill = TRUE, comment.char = "")
    
    
    
    
    timebase$time <- timebase$time * t_value  
    
    force$time <- timebase$time   
    
    
    #Quad and counter low point zeroing. Burst needs no low point zeroing
    force$accel = (force$force / mass)
    
    #post launch acceleration value change to -9.81
    force$accel[21:200] = -9.81
    
    #speed
    force$speed <-cumsum((force$accel * t_value) / 20)
    
    #get row behind function
    rowshift <- function (x, shiftLen=1L) {
      r <- (1L +shiftLen):(length(x) +shiftLen)
      r[r<1] <- NA
      return(x[r])
    }
    
    #add speed to previous row speed
    force$culmspeed <- force$speed + rowshift(force$speed, -1)
    
    #replace NA
    force$culmspeed[1] = (0.5 * force$speed[1] * t_value / 20)
    
    #distance
    force$distance <- (0.5 * force$culmspeed * t_value / 20)
    force$distance2 <- force$distance
    
    #culm dist
    #set low point height quad and counter
  #  force$distance2[11] <- glen_start
  #  force$distance2[1:10] <- 0
    #set low point height burst
      force$distance2[1] <- glen_start
    
    force$culmdist <- cumsum(force$distance2)
    launch_height <<- force$culmdist[20]
    height_reached <<- max(force$culmdist)
    
    #comment out in burst too
   # force$culmdist[1:10] <- glen_start
    
    #work
    force$work <- ((force$force + (mass * 9.81)) * force$distance)
    
    #Power
    force$power <- (force$work / (t_value / 20))
    force$power[21:200] = 0
    
    # Muscle mass fraction
    power_ <- max(force$power)
    total_power <- power_ / (sin(l_angle * (pi / 180)))
    muscle_mass_fraction <<- (total_power / 400) / mass
    
    force <<- force
    
  }
  forcecalcs(t_value = t_)
  
  if ((launch_height > (glen_end - 0.005)) & (launch_height < (glen_end + 0.005))){   
    
    print("1- glenoid height at launch")
    print(launch_height)
    print("height reached")
    print(height_reached)
    print("time")
    print(t_)
    print("muscle Mass Fraction")
    print(muscle_mass_fraction)
    
    # output <- data.frame (t_,launch_height,height_reached,muscle_mass_fraction)
    # write.csv(output,(paste0("Outputs/variable_speed/",l_profile,"2/",forces,"_",anaero,"_",l_angle,"_mm.csv")))
    # write.csv(force,(paste0("Outputs/variable_speed/",l_profile,"2/",forces,"_",anaero,"_",l_angle,"_mm_working.csv")))
    graphoutput <- data.frame (force$time,force$culmdist)
    write.csv(graphoutput,(paste0("Outputs/variable_speed/",l_profile,"3/",l_angle,"_",forces,"_",anaero,"_mm_graph.csv")))
    
    
    
  } else if (launch_height > (glen_end + 0.005)) {
    print("2- glenoid height at launch")
    print(launch_height)
    
  } else {
    repeat{
      print(launch_height)
      t_ <- t_ + 0.01
      forcecalcs(t_value = t_)
      if((launch_height > (glen_end - 0.005)) | (t_ >= 2)){
        
        
        print("3- glenoid height at launch")
        print(launch_height)
        print("height reached")
        print(height_reached)
        print("time")
        print(t_)
        print("muscle Mass Fraction")
        print(muscle_mass_fraction)
        
        # output <- data.frame (t_,launch_height,height_reached,muscle_mass_fraction)
        # write.csv(output,(paste0("Outputs/variable_speed/",l_profile,"2/",forces,"_",anaero,"_",l_angle,"_mm.csv")))
        # write.csv(force,(paste0("Outputs/variable_speed/",l_profile,"2/",forces,"_",anaero,"_",l_angle,"_mm_working.csv")))
        graphoutput <- data.frame (force$time,force$culmdist)
        write.csv(graphoutput,(paste0("Outputs/variable_speed/",l_profile,"3/",l_angle,"_",forces,"_",anaero,"_mm_graph.csv")))
        break
      } 
    }
  }
  
}

#loops comment out line 74 unless burst in which case comment out 72

# #Quad
# #2x
# #m1 = 29.5  check comment out line 74 unless burst
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# #m2 = 30.9
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# #m3 =23.6
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# #2.5x
# #m1 = 29.5  check comment out line 74 unless burst
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# #m2 = 30.9
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# #m3 =23.6
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# 
# #3x
# #m1 = 29.5  check comment out line 74 unless burst
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# #m2 = 30.9
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# #m3 =23.6
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)

# #4x
# #m1 = 29.5  check comment out line 74 unless burst
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# #m2 = 30.9
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# #m3 =23.6
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_end = 0.78, glen_start = 0.57, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 60)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 45)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_end = 0.78, glen_start = 0.57, l_angle = 30)



#Burst
#2x
#m1 = 29.5  check comment out line 16 and 20
launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

#m2 = 30.9
launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

#m3 =23.6
launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

#2.5x
#m1
launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

#m2 = 30.9
launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

#m3 =23.6
launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

#3x
#m1 = 29.5  check comment out line 16 and 20
launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

#m2 = 30.9
launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

#m3 =23.6
launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "3x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

#4x
#m1 = 29.5  check comment out line 16 and 20
launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_73_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =29.5, l_profile = "Burst", forces = "0_67_Burst_m1c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

#m2 = 30.9
launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_73_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =30.9, l_profile = "Burst", forces = "0_67_Burst_m2c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

#m3 =23.6
launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_73_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3b", glen_end = 0.97, glen_start = 0.44, l_angle = 85)

launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 75)
launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 80)
launchcalc(anaero = "4x", mass =23.6, l_profile = "Burst", forces = "0_67_Burst_m3c", glen_end = 0.97, glen_start = 0.44, l_angle = 85)



# #countermotion
# 
# #m1 = 29.5  check comment out line 16 and 20
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# #m2 = 30.9
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# #m3 =23.6
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# #2.5x
# #m1 = 29.5  check comment out line 16 and 20
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# #m2 = 30.9
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# #m3 =23.6
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "2.5x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# #3x
# #m1 = 29.5  check comment out line 16 and 20
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# #m2 = 30.9
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# #m3 =23.6
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "3x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)

# #4x
# #m1 = 29.5  check comment out line 16 and 20
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_73_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =29.5, l_profile = "Counter", forces = "0_67_Biped_m1c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# #m2 = 30.9
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_73_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =30.9, l_profile = "Counter", forces = "0_67_Biped_m2c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# #m3 =23.6
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_73_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3b", glen_end = 0.82, glen_start = 0.74, l_angle = 30)
# 
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 60)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 45)
# launchcalc(anaero = "4x", mass =23.6, l_profile = "Counter", forces = "0_67_Biped_m3c", glen_end = 0.82, glen_start = 0.74, l_angle = 30)