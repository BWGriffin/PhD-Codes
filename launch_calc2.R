# make sure directory is right you idiot
setwd("C:/Users/id18752/OneDrive - University of Bristol/Documents/MATLAB/Impulse");

##launch height calculator comment out line 26 for burst
launchcalc <- function (speed, mass, t_, l_profile, forces, glen_start, l_angle){

  profile <- read.csv(paste0(l_profile,"_Profile.csv"), header = TRUE, sep = ",", quote =
                             "\"",dec = ".", fill = TRUE, comment.char = "")  
    
  peak_data <- read.csv(paste0("Forces/", forces,"_forces.csv"), header = TRUE, sep = ",", quote =
                        "\"",dec = ".", fill = TRUE, comment.char = "")
  timebase <- read.csv("20timebase.csv", header = TRUE, sep = ",", quote =
                          "\"",dec = ".", fill = TRUE, comment.char = "")
  timebase$time <- timebase$time * t_
  #quad and burst peak
  peak <<- peak_data[10,5]  #bird ankle
  #peak <<- peak_data[10,6]  #croc ankle
  
  #create force dataframe
  force <- data.frame(timebase)
  names(force)[names(force)=='profile.Normalised...peak'] <- 'force'
  

  #countermotion peak
  #peak <- peak_data[16,5]
  
  #function to do calculations in the forces dataframe
  forcecalcs <<- function (peak_value){
    
  force$accelforce[1:20] <- peak_data$angles.accel_force_bird
  #post launch acceleration value change to -9.81
  force$accelforce[21:200] <- 0
  
  #accel
  force$accel <- force$accelforce / mass
  #post launch acceleration value change to -9.81
  force$accel[21:200] <- -9.81

  #speed
  force$speed <-cumsum((force$accel * t_) / 20)

  #get row behind function
  rowshift <- function (x, shiftLen=1L) {
    r <- (1L +shiftLen):(length(x) +shiftLen)
    r[r<1] <- NA
    return(x[r])
  }

  #add speed to previous row speed
  force$c.speed <- force$speed + rowshift(force$speed, -1)

  #replace NA
  force$c.speed[1] = force$speed[1]

  #distance
  force$distance <- (0.5 * force$c.speed * t_ / 20)
  force$distance2 <- force$distance

  #culm dist
  #set low point height
  force$distance2[11] <- glen_start
  force$c.dist <- cumsum(force$distance2)
  launch_height <<- force$c.dist[20]
  height_reached <<- max(force$c.dist)


  #work
   force$work <- ((force$force + (mass * 9.81)) * force$distance)

    #Power
   force$power <- (force$work / (t_ / 42))
   force$power[43:200] = 0

   # Muscle mass fraction
   power_peak <- max(force$power)
   total_power <- power_peak / (sin(l_angle * (pi / 180)))
   muscle_mass_fraction <<- (total_power / 400) / mass
   
   force <<- force

  }
  forcecalcs(peak_value = peak)

  if ((launch_height > (0.78 - 0.005)) & (launch_height < (0.78 + 0.005))){   
    print ("Peak force")
    print (peak)
    print("glenoid height at launch 1")
    print(launch_height)
    print("height reached")
    print(height_reached)
    print("muscle Mass Fraction")
    print(muscle_mass_fraction)
    
    output <- data.frame (peak,launch_height,height_reached,muscle_mass_fraction)
    write.csv(output,(paste0("Outputs/",speed,"/",l_profile,"/",forces,"_",l_angle,"_",t_,".csv")))
    write.csv(force,(paste0("Outputs/",speed,"/",l_profile,"/",forces,"_",l_angle,"_",t_,"_working.csv")))
    graphoutput <- data.frame (force$time,force$culmdist)
    write.csv(graphoutput,(paste0("Outputs/",speed,"/",l_profile,"/",forces,"_",l_angle,"_",t_,"_graph.csv")))
    
    
    
 } else if (launch_height <= (0.78 + 0.005)) {
    print("glenoid height at launch 2")
    print(launch_height)
    output <- data.frame (peak,launch_height,height_reached,muscle_mass_fraction)
    write.csv(output,(paste0("Outputs/",speed,"/fail/",l_profile,"/",forces,"_",l_angle,"_",t_,".csv")))
    write.csv(force,(paste0("Outputs/",speed,"/fail/",l_profile,"/",forces,"_",l_angle,"_",t_,"_working.csv")))
    graphoutput <- data.frame (force$time,force$culmdist)
    write.csv(graphoutput,(paste0("Outputs/",speed,"/fail/",l_profile,"/",forces,"_",l_angle,"_",t_,"_graph.csv")))
    
 } else {
   repeat{
    print(launch_height)
    peak <- peak - 10
    forcecalcs(peak_value = peak)
    if((launch_height < (0.78 + 0.005))){
      
      print ("Peak force")
      print (peak)
      print("glenoid height at launch 3")
      print(launch_height)
      print("height reached")
      print(height_reached)
      print("muscle Mass Fraction")
      print(muscle_mass_fraction)
      
      output <- data.frame (peak,launch_height,height_reached,muscle_mass_fraction)
      write.csv(output,(paste0("Outputs/",speed,"/",l_profile,"/",forces,"_",l_angle,"_",t_,".csv")))
      write.csv(force,(paste0("Outputs/",speed,"/",l_profile,"/",forces,"_",l_angle,"_",t_,"_working.csv")))
      graphoutput <- data.frame (force$time,force$culmdist)
      write.csv(graphoutput,(paste0("Outputs/",speed,"/",l_profile,"/",forces,"_",l_angle,"_",t_,"_graph.csv")))
      break
    }
   }
 }

}
#bat__speed = 0..129
#burst__speed = 0..21
#counter__speed = 0..291

#m1 = 29.5
launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m1", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m1", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m1", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m1", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m1", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m1", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 29.5, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_start = 0.57, l_angle = 30)

#m2 = 30.9
launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m2", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m2", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m2", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m2", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m2", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m2", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 30.9, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_start = 0.57, l_angle = 30)

#m3 =23.6
launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m3", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m3", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m3", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m3", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m3", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m3", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "bat_speed",  mass = 23.6, t_ = 0.129, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_start = 0.57, l_angle = 30)



#m1 = 29.5
launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m1", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m1", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m1", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m1", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m1", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m1", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 29.5, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_start = 0.57, l_angle = 30)

#m2 = 30.9
launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m2", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m2", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m2", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m2", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m2", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m2", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 30.9, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_start = 0.57, l_angle = 30)

#m3 =23.6
launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m3", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m3", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m3", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m3", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m3", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m3", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "burst_speed",  mass = 23.6, t_ = 0.21, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_start = 0.57, l_angle = 30)


#m1 = 29.5
launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m1", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m1", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m1", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m1b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m1c", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m1", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m1", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m1", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m1b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 29.5, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m1c", glen_start = 0.57, l_angle = 30)

#m2 = 30.9
launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m2", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m2", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m2", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m2b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m2c", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m2", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m2", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m2", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m2b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 30.9, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m2c", glen_start = 0.57, l_angle = 30)

#m3 =23.6
launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m3", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m3", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m3", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m3b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_73_Quad_m3c", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m3", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m3", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m3", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m3b", glen_start = 0.57, l_angle = 30)

launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_start = 0.57, l_angle = 60)
launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_start = 0.57, l_angle = 45)
launchcalc(speed = "counter_speed",  mass = 23.6, t_ = 0.291, l_profile = "Quad", forces = "0_67_Quad_m3c", glen_start = 0.57, l_angle = 30)