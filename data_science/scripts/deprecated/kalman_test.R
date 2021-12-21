# install packages
#install.packages("RMThreshold")
#install.packages("jsonlite")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("patchwork")
#install.packages("reshape2")
#install.packages("RSpincalc")
#install.packages("LaplacesDemon")

# laden der Libraries

library(dplyr)
library(RMThreshold)
library(ggplot2)
library(RSpincalc)
library(reshape2)
library(LaplacesDemon)

dt = 14 / 1000

# Definition von funktionen

interpolateDataChunk <- function(index, chunk){
  if(index == 1){
    index_of_first_non_zero = subset(data.frame(i = 1:length(chunk), x = chunk), x != 0)[1,1]
    chunk[0:(index_of_first_non_zero-1)] = chunk[index_of_first_non_zero]
    return(chunk[1:index_of_first_non_zero])
  }
  else{
    if(length(chunk) == 2)
      return(chunk[1])
    else if(length(chunk) > 2)
      numberOfIncrements = length(chunk[2:(length(chunk))])
      interpolationIncrement = (chunk[length(chunk)] - chunk[1]) / numberOfIncrements
      return(c(chunk[1], chunk[1] + 1:(numberOfIncrements-1) * interpolationIncrement))
  }
}

accumulateChunks <- function(vals){
  if(vals[1] == 0) {
  } else {
    index_of_first_non_zero = subset(data.frame(i = 1:length(vals), x = vals), x != 0)[1,1]
    index_of_second_non_zero = subset(data.frame(i = 1:(length(vals)-1), x = vals[(index_of_first_non_zero+1):length(vals)]), x != 0)[1,1]
    if(is.na(index_of_second_non_zero)){
      vals[length(vals)] = vals[index_of_first_non_zero]
      return(list(vals))
    } else {
      return(list(vals[index_of_first_non_zero:(index_of_second_non_zero+1)]))
    }
  }
}

# Function to interpolate values inbetween
interpolateZeroesInbetween <- function(measurements){
  chunks = list()
  measurements_temp=measurements
  for(v in 1:length(measurements)){
    if(length(measurements_temp) > 1){
      chunks[v] = accumulateChunks(measurements_temp)
      measurements_temp = measurements_temp[2:length(measurements_temp)]
    }
  }
  names(chunks) <- seq_along(chunks)
  chunks[sapply(chunks, is.null)] <- NULL
  return(unlist(mapply(interpolateDataChunk, 1:length(chunks), chunks)))
}

# Berechne Likelyhood
likelyhood_function <- function(x, mu, S){
  numberOfRows = nrow(S)
  ss = x - mu
  k = nrow(S)
  z = rowSums({ss %*% solve(S)} * ss)
  L = as.vector(-0.5 * (k * log(2 * pi) + log(det(S))) - (0.5 * z))
  
  return(L)
}

# Helper function to transform quaternions to euler angles
quaternionToEulerAngles <- function(q){
  angles = list(roll = 0, pitch = 0, yaw = 0)
  # roll (x-axis rotation)
  sinr_cosp = 2 * (q$w * q$x + q$y * q$z);
  cosr_cosp = 1 - 2 (q$x**2 - q$y**2);
  angles$roll = atan2(sinr_cosp , cosr_cosp);
  
  # pitch (y-axis rotation)
  sinp = 2*(q$y * q$w - q$x * q$z);
  if (abs(sinp) >= 1){
    angles$pitch = sign(M_PI / 2, sinp); # use 90 degrees if out of range
  }
  else{
    angles$pitch = asin(sinp) ;
  }
  
  # https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles
  # yaw (z-axis rotation)
  siny_cosp = 2 * (q$w * q$z + q$x * q$y);
  cosy_cosp = 1 - 2 * (q$y**2 + q$z**2);
  angles$yaw = atan2(siny_cosp ,  cosy_cosp);
  return(angles)
}

## Models
# Setup Velocity Model with xyz-position, xyz-velocity on two sensors (12 dimensionsional) -> no fusion
getVelocityModel <- function(){
  velocityModel = list()
  velocityModel$F = matrix(data = c(
    1,0,0,dt,0,0,0,0,0,0,0,0,
    0,0,0,1,0,0,0,0,0,0,0,0,
    0,1,0,0,dt,0,0,0,0,0,0,0,
    0,0,0,0,1,0,0,0,0,0,0,0,
    0,0,1,0,0,dt,0,0,0,0,0,0,
    0,0,0,0,0,1,0,0,0,0,0,0,
    0,0,0,0,0,0,1,0,0,dt,0,0,
    0,0,0,0,0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,0,1,0,0,dt,0,
    0,0,0,0,0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,0,1,0,0,dt,
    0,0,0,0,0,0,0,0,0,0,0,1,
  )
  ,nrow = 12, ncol = 12)
  
  velocityModel$a = matrix(data = c(0,0,0,0,0,0,0,0,0,0,0,0), nrow = 12, ncol = 1) ## state vector
  
  velocityModel$H = matrix(data = c(
    1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0
  ), nrow = 6, ncol = 12) ## observation/ measurment vector 
  
  velocityModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 12, ncol = 12), mean = 0, stddev = 1) ## white noise as process noise
  velocityModel$P = diag(c(-500,0,500,10,10,10,2,2,2,0.5,0.5,0.5))  
  
  x_sigma = 0.05; xy_sigma = 0; xz_sigma = 0; xyaw_sigma = 0; xpitch_sigma = 0; xroll_sigma = 0
  yx_sigma = 0; y_sigma = 0.02; yz_sigma = 0; yyaw_sigma = 0; ypitch_sigma = 0; yroll_sigma = 0
  zx_sigma = 0; zy_sigma = 0; z_sigma = 0.1; zyaw_sigma = 0; zpitch_sigma = 0; zroll_sigma = 0
  yawx_sigma = 0; yawy_sigma = 0; yawz_sigma = 0; yaw_sigma=0.05; yawpitch_sigma=0; yawroll_sigma = 0
  pitchx_sigma = 0; pitchy_sigma = 0; pitchz_sigma = 0; pitchyaw_sigma = 0; pitch_sigma=0.05; pitchroll_sigma=0
  rollx_sigma = 0; rolly_sigma = 0; rollz_sigma = 0; rollyaw_sigma = 0; rollpitch_sigma = 0; roll_sigma = 0.05
  
  velocityModel$R = matrix(data = c(x_sigma**2, xy_sigma, xz_sigma, xyaw_sigma, xpitch_sigma, xroll_sigma,
                                        yx_sigma, y_sigma**2, yz_sigma, yyaw_sigma, ypitch_sigma, yroll_sigma,
                                        zx_sigma, zy_sigma, z_sigma**2, zyaw_sigma, zpitch_sigma, zroll_sigma,
                                        yawx_sigma, yawy_sigma, yawz_sigma, yaw_sigma**2, yawpitch_sigma, yawroll_sigma,
                                        pitchx_sigma, pitchy_sigma, pitchz_sigma, pitchyaw_sigma, pitch_sigma**2, pitchroll_sigma,
                                        rollx_sigma, rolly_sigma, rollz_sigma, rollyaw_sigma, rollpitch_sigma, roll_sigma**2
  ), nrow = 6, ncol = 6) 
  
  return(velocityModel)
}

## Get 6 Degrees model
get6AxisOfFreedomVelocityModel <- function(){
  velocityModel = list()
  velocityModel$F = matrix(data = c(
    1,0,0,dt,0,0,
    0,0,0,1,0,0,
    0,1,0,0,dt,0,
    0,0,0,0,1,0,
    0,0,1,0,0,dt,
    0,0,0,0,0,1
  )
  ,nrow = 6, ncol = 6)
  
  velocityModel$a = matrix(data = c(0,0,0,0,0,0), nrow = 6, ncol = 1) ## state vector
  
  velocityModel$H = matrix(data = c(
    0,0,0,0,0,0,
    0,0,0,0,0,0,
    0,0,0,0,0,0,
    0,0,0,1,0,0,
    0,0,0,0,1,0,
    0,0,0,0,0,1
  ), nrow = 6, ncol = 6) ## observation/ measurment vector 
  
  velocityModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 12, ncol = 12), mean = 0, stddev = 1) ## white noise as process noise
  velocityModel$P = diag(c(-500,0,500,10,10,10,2,2,2,0.5,0.5,0.5))  
  
  x_sigma = 0.05; xy_sigma = 0; xz_sigma = 0 
  yx_sigma = 0; y_sigma = 0.02; yz_sigma = 0 
  zx_sigma = 0; zy_sigma = 0; z_sigma = 0.1
  
  
  velocityModel$R = matrix(data = c(x_sigma**2, xy_sigma, xz_sigma, 
                                    yx_sigma, y_sigma**2, yz_sigma, 
                                    zx_sigma, zy_sigma, z_sigma**2, 
  ), nrow = 3, ncol = 3) 
  
  return(velocityModel)
} 

# Setup Acceleration model with xyz-position, xyz-velocity, xyz-acceleration on one sensor (9 dimensionsional)
# Model for one Sensor and tested with IMU
getOnlyPositionAccelerationModel <-function(){
  accelerationModel = list()
  
  accelerationModel$F = matrix(data = c(
    1,0,0,dt,0,0,dt*dt/2,0,0,
    0,0,0,1,0,0,dt,0,0,
    0,0,0,0,0,0,1,0,0,
    0,1,0,0,dt,0,0,dt*dt/2,0,
    0,0,0,0,1,0,0,dt,0,
    0,0,0,0,0,0,0,1,0,
    0,0,1,0,0,dt,0,0,dt*dt/2,
    0,0,0,0,0,1,0,0,dt,
    0,0,0,0,0,0,0,0,1
  )
  ,nrow = 9, ncol = 9, byrow=TRUE)
  
  accelerationModel$a = matrix(data = c(1,0,-10, 1,0,-20, 1,0,-10), nrow = 9, ncol = 1) ## state vector
  accelerationModel$H = matrix(data = c(
    1,0,1, 0,0,0, 0,0,0, 
    0,0,0, 1,0,1, 0,0,0, 
    0,0,0, 0,0,0, 1,0,1
  ), nrow = 3, ncol = 9, byrow=TRUE) ## observation/ measurment vector 
  
  # Get white noise 
  accelerationModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 9, ncol = 9), mean = 0, stddev = 2.56) ## white noise as process noise
  accelerationModel$P = diag(c(0.001,0,-10,0.001,0,-10,0.001,0,10))
  
  x_sigma = 0.05;
  y_sigma = 0.02;
  z_sigma = 0.01;
  xy_sigma = 0.0;
  xz_sigma = 0.0;
  yx_sigma = 0.0;
  yz_sigma = 0.0;
  zx_sigma = 0.0;
  zy_sigma = 0.0;
  
  accelerationModel$R = matrix(data = c(x_sigma**2, xy_sigma, xz_sigma,
                                        yx_sigma, y_sigma**2, yz_sigma,
                                        zx_sigma, zy_sigma, z_sigma**2
  ), nrow = 3, ncol = 3)
  
  accelerationModel$colNames = c("x","y","z","v_x","v_y","v_z","a_x","a_y","a_z")
  
  return(accelerationModel)
}

# Setup Acceleration model with xyz-position, xyz-velocity, xyz-acceleration on one sensor (9 dimensionsional),
# Intended model for fusion of IMU and Kinect
getOnlyPositionAccelerationFusionWithDiffVeloModel <- function(){
  accelerationModel = list()
  
  accelerationModel$F = matrix(data = c(
    1,0,0,dt,0,0,dt*dt/2,0,0,
    0,0,0,1,0,0,dt,0,0,
    0,0,0,0,0,0,1,0,0,
    0,1,0,0,dt,0,0,dt*dt/2,0,
    0,0,0,0,1,0,0,dt,0,
    0,0,0,0,0,0,0,1,0,
    0,0,1,0,0,dt,0,0,dt*dt/2,
    0,0,0,0,0,1,0,0,dt,
    0,0,0,0,0,0,0,0,1
  )
  ,nrow = 9, ncol = 9, byrow=TRUE)
  
  accelerationModel$a = matrix(data = c(-0.05,0,0, -0.9,0,-100, 0.8,0,0), nrow = 9, ncol = 1) ## state vector
  accelerationModel$H = matrix(data = c(
    1,0,0, 0,0,0, 0,0,0,
    0,0,0, 1,0,0, 0,0,0,
    0,0,0, 0,0,0, 1,0,0,
    0,0,1, 0,0,0, 0,0,0,
    0,0,0, 0,0,1, 0,0,0,
    0,0,0, 0,0,0, 0,0,1,
    0,1,0, 0,0,0, 0,0,0,
    0,0,0, 0,1,0, 0,0,0,
    0,0,0, 0,0,0, 0,1,0
  ), nrow = 9, ncol = 9, byrow=TRUE) ## observation/ measurement vector 
  
  # Get white noise 
  accelerationModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 9, ncol = 9), mean = 0, stddev = 2.56) ## white noise as process noise
  accelerationModel$P = diag(c(0.02,0.1,100, 0.02,0.1,100, 0.05,0.1,10000))
  
  x_sigma = 0.01;
  y_sigma = 0.01;
  z_sigma = 0.01;
  a_x_sigma = 0.005;
  a_y_sigma = 0.005;
  a_z_sigma = 0.005;
  kinect_xv_sigma = 0.05;
  kinect_yv_sigma = 0.05;
  kinect_zv_sigma = 0.05;
  
  accelerationModel$R = diag(c(x_sigma**2, y_sigma**2, z_sigma**2,
                               a_x_sigma**2, a_y_sigma**2, a_z_sigma**2,
                               kinect_xv_sigma**2, kinect_yv_sigma**2, kinect_zv_sigma**2))
  
  accelerationModel$colNames = c("x","y","z","v_x","v_y","v_z","a_x","a_y","a_z")
  
  return(accelerationModel)
}

# Setup Acceleration model with xyz-position, xyz-velocity, xyz-acceleration on one sensor (9 dimensionsional),
# Intended model for fusion of IMU and Kinect, but does not use cumsum and differentiated Velocity and Acceleration
getOnlyPositionAccelerationFusionModel <- function(){
  accelerationModel = list()
  
  accelerationModel$F = matrix(data = c(
    1,0,0,dt,0,0,dt*dt/2,0,0,
    0,0,0,1,0,0,dt,0,0,
    0,0,0,0,0,0,1,0,0,
    0,1,0,0,dt,0,0,dt*dt/2,0,
    0,0,0,0,1,0,0,dt,0,
    0,0,0,0,0,0,0,1,0,
    0,0,1,0,0,dt,0,0,dt*dt/2,
    0,0,0,0,0,1,0,0,dt,
    0,0,0,0,0,0,0,0,1
  )
  ,nrow = 9, ncol = 9, byrow=TRUE)
  
  accelerationModel$a = matrix(data = c(-0.05,0,0, -0.9,0,-700, 0.8,0,11700), nrow = 9, ncol = 1) ## state vector
  accelerationModel$H = matrix(data = c(
    1,0,0, 0,0,0, 0,0,0,
    0,0,0, 1,0,0, 0,0,0,
    0,0,0, 0,0,0, 1,0,0,
    0,0,1, 0,0,0, 0,0,0,
    0,0,0, 0,0,1, 0,0,0,
    0,0,0, 0,0,0, 0,0,1
  ), nrow = 6, ncol = 9, byrow=TRUE) ## observation/ measurement vector 
  
  # Get white noise 
  accelerationModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 9, ncol = 9), mean = 0, stddev = 2.56) ## white noise as process noise
  accelerationModel$P = diag(c(0.02,0,100, 0.02,0,100, 0.05,0,10000))
  
  x_sigma = 0.001;
  y_sigma = 0.001;
  z_sigma = 0.001;
  a_x_sigma = 0.005;
  a_y_sigma = 0.005;
  a_z_sigma = 0.005;
  
  accelerationModel$R = diag(c(x_sigma**2, y_sigma**2, z_sigma**2,
                               a_x_sigma**2, a_y_sigma**2, a_z_sigma**2))
  
  accelerationModel$colNames = c("x","y","z","v_x","v_y","v_z","a_x","a_y","a_z")
  
  return(accelerationModel)
}

# Acceleration model imu + kinect, no fusion, was intended for Orientation, used Accelerometer and Gyroscope  (18 dimensionsional)(History, not part of presentation)
getAccelerationModel <- function(){
  accelerationModel = list()
  
  accelerationModel$F = t(matrix(data = c(
    1,0,0,dt,0,0,dt*dt/2,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,1,0,0,dt,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,dt,0,0,dt*dt/2,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,1,0,0,dt,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
    0,0,1,0,0,dt,0,0,dt*dt/2,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,1,0,0,dt,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,dt*dt/2,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,dt*dt/2,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,dt*dt/2,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
  )
  ,nrow = 18, ncol = 18))
  
  accelerationModel$a = matrix(data = c(-10,0,0, 500,0,0, 500,0,0, 3,0,0, 3,0,0, 3,0,0), nrow = 18, ncol = 1) ## state vector
  accelerationModel$H = matrix(data = c(
    1,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0,
    0,1,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0,
    0,0,1, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0,
    0,0,0, 1,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0,
    0,0,0, 0,1,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0,
    0,0,0, 0,0,1, 0,0,0, 0,0,0, 0,0,0, 0,0,0
  ), ncol = 18, nrow = 6) ## observation/ measurment vector 
  
  # Get white noise 
  accelerationModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 18, ncol = 18), mean = 0, stddev = 2.56) ## white noise as process noise
  accelerationModel$P = diag(c(-10,0,0,20,0,00,10,0,0,2,0,0,2,0,0,1,0,0))
  
  x_sigma = 0.05; xy_sigma = 0; xz_sigma = 0; xyaw_sigma = 0; xpitch_sigma = 0; xroll_sigma = 0
  yx_sigma = 0; y_sigma = 0.02; yz_sigma = 0; yyaw_sigma = 0; ypitch_sigma = 0; yroll_sigma = 0
  zx_sigma = 0; zy_sigma = 0; z_sigma = 0.1; zyaw_sigma = 0; zpitch_sigma = 0; zroll_sigma = 0
  yawx_sigma = 0; yawy_sigma = 0; yawz_sigma = 0; yaw_sigma=0.05; yawpitch_sigma=0; yawroll_sigma = 0
  pitchx_sigma = 0; pitchy_sigma = 0; pitchz_sigma = 0; pitchyaw_sigma = 0; pitch_sigma=0.05; pitchroll_sigma=0
  rollx_sigma = 0; rolly_sigma = 0; rollz_sigma = 0; rollyaw_sigma = 0; rollpitch_sigma = 0; roll_sigma = 0.05
  
  accelerationModel$R = matrix(data = c(x_sigma**2, xy_sigma, xz_sigma, xyaw_sigma, xpitch_sigma, xroll_sigma,
                      yx_sigma, y_sigma**2, yz_sigma, yyaw_sigma, ypitch_sigma, yroll_sigma,
                      zx_sigma, zy_sigma, z_sigma**2, zyaw_sigma, zpitch_sigma, zroll_sigma,
                      yawx_sigma, yawy_sigma, yawz_sigma, yaw_sigma**2, yawpitch_sigma, yawroll_sigma,
                      pitchx_sigma, pitchy_sigma, pitchz_sigma, pitchyaw_sigma, pitch_sigma**2, pitchroll_sigma,
                      rollx_sigma, rolly_sigma, rollz_sigma, rollyaw_sigma, rollpitch_sigma, roll_sigma**2
  ), nrow = 6, ncol = 6) 
  
  accelerationModel$colNames = c("x","y","z","v_x","v_y","v_z","a_x","a_y","a_z", "yaw","pitch","roll","v_yaw","v_pitch","v_roll","a_yaw","a_pitch","a_roll")
  
  return(accelerationModel)
}

# Acceleration model imu + kinect, no fusion, was intended for Orientation, used Accelerometer and Gyroscope  (18 dimensionsional)(History, not part of presentation)
getAccelerationModel_2 <- function(){
  accelerationModel = list()
  
  accelerationModel$F = matrix(data = c(
    1,0,0,dt,0,0,dt*dt/2,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,1,0,0,dt,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,dt,0,0,dt*dt/2,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,1,0,0,dt,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
    0,0,1,0,0,dt,0,0,dt*dt/2,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,1,0,0,dt,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,dt*dt/2,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,dt*dt/2,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,dt*dt/2,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
  )
  ,nrow = 18, ncol = 18, byrow=TRUE)
  
  accelerationModel$a = matrix(data = c(0,0,1, 0,0,1, 0,0,1, 0,0,0, 0,0,0, 0,0,0)
                               , nrow = 18, ncol = 1) ## state vector
  
  accelerationModel$H = matrix(data = c(
    0,0,1, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0,
    0,0,0, 0,0,1, 0,0,0, 0,0,0, 0,0,0, 0,0,0,
    0,0,0, 0,0,0, 0,0,1, 0,0,0, 0,0,0, 0,0,0,
    0,0,0, 0,0,0, 0,0,0, 1,0,0, 0,0,0, 0,0,0,
    0,0,0, 0,0,0, 0,0,0, 0,0,0, 1,0,0, 0,0,0,
    0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 1,0,0
  ), ncol = 18, nrow = 6, byrow=TRUE) ## observation/ measurment vector 
  
  # Get white noise 
  accelerationModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 18, ncol = 18), mean = 0, stddev = 2.56) ## white noise as process noise
  accelerationModel$P = diag(c(0,0,1,0,0,1,0,0,1,2,0,0,2,0,0,1,0,0))
  
  x_sigma = 0.5;y_sigma = 0.02; z_sigma = 0.1; yaw_sigma=0.05;pitch_sigma=0.05; roll_sigma = 0.05;
  
  accelerationModel$R = diag(c(x_sigma**2, y_sigma**2, z_sigma**2, yaw_sigma**2, pitch_sigma**2, roll_sigma**2)) 
  
  accelerationModel$colNames = c("x","y","z","v_x","v_y","v_z","a_x","a_y","a_z", "yaw","pitch","roll","v_yaw","v_pitch","v_roll","a_yaw","a_pitch","a_roll")
  
  return(accelerationModel)
}

# Acceleration model imu + kinect, no fusion, was intended for Orientation, used Accelerometer and Gyroscope  (18 dimensionsional)(History, not part of presentation)
getAccelerationModel_with_kinect <- function(){
  accelerationModel = list()
  
  accelerationModel$F = t(matrix(data = c(
    1,0,0,dt,0,0,dt*dt/2,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,1,0,0,dt,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,dt,0,0,dt*dt/2,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,1,0,0,dt,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
    0,0,1,0,0,dt,0,0,dt*dt/2,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,1,0,0,dt,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,dt*dt/2,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,dt*dt/2,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,dt*dt/2,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
  )
  ,nrow = 18, ncol = 18, byrow=TRUE))
  
  accelerationModel$a = matrix(data = c(0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0)
                               , nrow = 18, ncol = 1) ## state vector
  
  accelerationModel$H = matrix(data = c(
    1,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0,
    0,0,0, 1,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0,
    0,0,0, 0,0,0, 1,0,0, 0,0,0, 0,0,0, 0,0,0,
    0,0,1, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0,
    0,0,0, 0,0,1, 0,0,0, 0,0,0, 0,0,0, 0,0,0,
    0,0,0, 0,0,0, 0,0,1, 0,0,0, 0,0,0, 0,0,0,
    0,0,0, 0,0,0, 0,0,0, 1,0,0, 0,0,0, 0,0,0,
    0,0,0, 0,0,0, 0,0,0, 0,0,0, 1,0,0, 0,0,0,
    0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 1,0,0
  ), ncol = 18, nrow = 9, byrow=TRUE) ## observation/ measurment vector 
  
  # Get white noise 
  accelerationModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 18, ncol = 18), mean = 0, stddev = 2.56) ## white noise as process noise
  accelerationModel$P = diag(c(-10,0,1,-10,0,1,10,0,1,2,0,0,2,0,0,1,0,0))
  
  x_sigma = 0.001; y_sigma = 0.001; z_sigma = 0.001; ax_sigma=0.05; ay_sigma=0.05; az_sigma=0.5; yaw_sigma=0.05; pitch_sigma=0.05;  roll_sigma = 0.05;
  
  accelerationModel$R = diag(c(x_sigma**2, y_sigma**2, z_sigma**2,ax_sigma**2, ay_sigma**2, az_sigma**2, yaw_sigma**2, pitch_sigma**2, roll_sigma**2)) 
  
  accelerationModel$colNames = c("x","y","z","v_x","v_y","v_z","a_x","a_y","a_z", "yaw","pitch","roll","v_yaw","v_pitch","v_roll","a_yaw","a_pitch","a_roll")
  
  return(accelerationModel)
}

# Jerk model imu + kinect, no fusion, was intended for Orientation, used Accelerometer and Gyroscope  (24 dimensionsional)(History, not part of presentation)
getJerkModel <- function(){
  jerkModel = list()
  
  d1 = dt
  d2 = dt**2/2
  d3 = dt**3/6
  
  jerkModel$F = t(matrix(data = c(
    1,0,0,  d1,0,0, d2,0,0, d3,0,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  1,0,0,  d1,0,0, d2,0,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  1,0,0,  d1,0,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,0,0,  1,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,1,0,  0,d1,0, 0,d2,0, 0,d3,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,1,0,  0,d1,0, 0,d2,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,1,0,  0,d1,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,1,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,1,  0,0,d1, 0,0,d2, 0,0,d3, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,1,  0,0,dt, 0,0,d2, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,0,1,  0,0,d1, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,1,  0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  1,0,0,  d1,0,0, d2,0,0, d3,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  1,0,0,  d1,0,0, d2,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  1,0,0,  d1,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  1,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,1,0,  0,d1,0, 0,d2,0, 0,d3,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,1,0,  0,d1,0, 0,d2,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,1,0,  0,d1,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,1,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,1,  0,0,d1, 0,0,d2, 0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,1,  0,0,d1, 0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,1,  0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,1
  )
  ,nrow = 24, ncol = 24))
  
  jerkModel$a = matrix(
    data = c(-500,0,0, 500,0,0, 500,0,0, 0,0,0, 1,0,0, 0,0,0, 0,0,0, 0,0,0), 
    nrow = 24,
    ncol = 1
    ) ## state vector
  jerkModel$H = matrix(data = c(
    1,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0, 0,0,0,
    0,1,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0, 0,0,0,
    0,0,1,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0, 0,0,0,
    0,0,0,  1,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0, 0,0,0,
    0,0,0,  0,1,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0, 0,0,0,
    0,0,0,  0,0,1,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0, 0,0,0
  ), nrow = 6, ncol = 24) ## observation/ measurment vector 
  
  # Get white noise 
  jerkModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 24, ncol = 24), mean = 0, stddev = 2.56) ## white noise as process noise
  jerkModel$P = diag(c(10,0,500,10,10,10,2,2,2,1,1,1, 2,2,2,0.5,0.5,0.5,0.01,0.01,0.01,0.01,0.01,0.01))
   
  x_sigma = 0.05; xy_sigma = 0; xz_sigma = 0; xyaw_sigma = 0; xpitch_sigma = 0; xroll_sigma = 0
  yx_sigma = 0; y_sigma = 0.05; yz_sigma = 0; yyaw_sigma = 0; ypitch_sigma = 0; yroll_sigma = 0
  zx_sigma = 0; zy_sigma = 0; z_sigma = 0.1; zyaw_sigma = 0; zpitch_sigma = 0; zroll_sigma = 0
  yawx_sigma = 0; yawy_sigma = 0; yawz_sigma = 0; yaw_sigma=0.05; yawpitch_sigma=0; yawroll_sigma = 0
  pitchx_sigma = 0; pitchy_sigma = 0; pitchz_sigma = 0; pitchyaw_sigma = 0; pitch_sigma=0.05; pitchroll_sigma=0
  rollx_sigma = 0; rolly_sigma = 0; rollz_sigma = 0; rollyaw_sigma = 0; rollpitch_sigma = 0; roll_sigma = 0.05
  
  jerkModel$R = matrix(data = c(x_sigma**2, xy_sigma, xz_sigma, xyaw_sigma, xpitch_sigma, xroll_sigma,
                                        yx_sigma, y_sigma**2, yz_sigma, yyaw_sigma, ypitch_sigma, yroll_sigma,
                                        zx_sigma, zy_sigma, z_sigma**2, zyaw_sigma, zpitch_sigma, zroll_sigma,
                                        yawx_sigma, yawy_sigma, yawz_sigma, yaw_sigma**2, yawpitch_sigma, yawroll_sigma,
                                        pitchx_sigma, pitchy_sigma, pitchz_sigma, pitchyaw_sigma, pitch_sigma**2, pitchroll_sigma,
                                        rollx_sigma, rolly_sigma, rollz_sigma, rollyaw_sigma, rollpitch_sigma, roll_sigma**2
  ), nrow = 6, ncol = 6) 
  
  jerkModel$colNames = c("x","y","z","v_x","v_y","v_z","a_x","a_y","a_z", "j_x", "j_y", "j_z", "yaw","pitch","roll","v_yaw","v_pitch","v_roll","a_yaw","a_pitch","a_roll", "j_yaw", "j_pitch", "j_roll")
  
  return(jerkModel)
}

# Jerk model imu + kinect, no fusion, was intended for Orientation, used Accelerometer and Gyroscope  (24 dimensionsional)(History, not part of presentation)
getJerkModel_2 <- function(){
  jerkModel = list()
  
  d1 = dt
  d2 = dt**2/2
  d3 = dt**3/6
  
  jerkModel$F = matrix(data = c(
    1,0,0,  d1,0,0, d2,0,0, d3,0,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  1,0,0,  d1,0,0, d2,0,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  1,0,0,  d1,0,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,0,0,  1,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,1,0,  0,d1,0, 0,d2,0, 0,d3,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,1,0,  0,d1,0, 0,d2,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,1,0,  0,d1,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,1,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,1,  0,0,d1, 0,0,d2, 0,0,d3, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,1,  0,0,dt, 0,0,d2, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,0,1,  0,0,d1, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,1,  0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  1,0,0,  d1,0,0, d2,0,0, d3,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  1,0,0,  d1,0,0, d2,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  1,0,0,  d1,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  1,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,1,0,  0,d1,0, 0,d2,0, 0,d3,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,1,0,  0,d1,0, 0,d2,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,1,0,  0,d1,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,1,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,1,  0,0,d1, 0,0,d2, 0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,1,  0,0,d1, 0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,1,  0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,1
  )
  ,nrow = 24, ncol = 24, byrow=TRUE)
  
  jerkModel$a = matrix(
    data = c(0,0,500,0, 0,0,500,0, 0,0,500,0, 1,0,0,0, 1,0,0,0, 1,0,0,0), 
    nrow = 24,
    ncol = 1
  ) ## state vector
  jerkModel$H = matrix(data = c(
    0,0,1,0,  0,0,0,0,  0,0,0,0,  0,0,0,0,  0,0,0,0,  0,0,0,0,
    0,0,0,0,  0,0,1,0,  0,0,0,0,  0,0,0,0,  0,0,0,0,  0,0,0,0,
    0,0,0,0,  0,0,0,0,  0,0,1,0,  0,0,0,0,  0,0,0,0,  0,0,0,0,
    0,0,0,0,  0,0,0,0,  0,0,0,0,  1,0,0,0,  0,0,0,0,  0,0,0,0,
    0,0,0,0,  0,0,0,0,  0,0,0,0,  0,0,0,0,  1,0,0,0,  0,0,0,0,
    0,0,0,0,  0,0,0,0,  0,0,0,0,  0,0,0,0,  0,0,0,0,  1,0,0,0
  ), nrow = 6, ncol = 24,  byrow=TRUE) ## observation/ measurment vector 
  
  # Get white noise 
  jerkModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 24, ncol = 24), mean = 0, stddev = 2.56) ## white noise as process noise
  jerkModel$P = diag(c(10,0,500,10,10,10,2,2,2,1,1,1, 2,2,2,0.5,0.5,0.5,0.01,0.01,0.01,0.01,0.01,0.01))
  
  x_sigma = 0.05; xy_sigma = 0; xz_sigma = 0; xyaw_sigma = 0; xpitch_sigma = 0; xroll_sigma = 0
  yx_sigma = 0; y_sigma = 0.05; yz_sigma = 0; yyaw_sigma = 0; ypitch_sigma = 0; yroll_sigma = 0
  zx_sigma = 0; zy_sigma = 0; z_sigma = 0.1; zyaw_sigma = 0; zpitch_sigma = 0; zroll_sigma = 0
  yawx_sigma = 0; yawy_sigma = 0; yawz_sigma = 0; yaw_sigma=0.05; yawpitch_sigma=0; yawroll_sigma = 0
  pitchx_sigma = 0; pitchy_sigma = 0; pitchz_sigma = 0; pitchyaw_sigma = 0; pitch_sigma=0.05; pitchroll_sigma=0
  rollx_sigma = 0; rolly_sigma = 0; rollz_sigma = 0; rollyaw_sigma = 0; rollpitch_sigma = 0; roll_sigma = 0.05
  
  jerkModel$R = matrix(data = c(x_sigma**2, xy_sigma, xz_sigma, xyaw_sigma, xpitch_sigma, xroll_sigma,
                                yx_sigma, y_sigma**2, yz_sigma, yyaw_sigma, ypitch_sigma, yroll_sigma,
                                zx_sigma, zy_sigma, z_sigma**2, zyaw_sigma, zpitch_sigma, zroll_sigma,
                                yawx_sigma, yawy_sigma, yawz_sigma, yaw_sigma**2, yawpitch_sigma, yawroll_sigma,
                                pitchx_sigma, pitchy_sigma, pitchz_sigma, pitchyaw_sigma, pitch_sigma**2, pitchroll_sigma,
                                rollx_sigma, rolly_sigma, rollz_sigma, rollyaw_sigma, rollpitch_sigma, roll_sigma**2
  ), nrow = 6, ncol = 6) 
  
  jerkModel$colNames = c("x","y","z","v_x","v_y","v_z","a_x","a_y","a_z", "j_x", "j_y", "j_z", "yaw","pitch","roll","v_yaw","v_pitch","v_roll","a_yaw","a_pitch","a_roll", "j_yaw", "j_pitch", "j_roll")
  
  return(jerkModel)
}

# Will Return Filter Object, from the variables
kalman_init <- function(x,P,Q,H,F,R){
  kalmanFilter <- list()
  
  kalmanFilter$x = x
  kalmanFilter$P = P
  kalmanFilter$Q = Q
  kalmanFilter$H = H
  kalmanFilter$R = R
  kalmanFilter$K = P %*% t(H) %*% solve(H %*% P %*% t(H) + R)
  kalmanFilter$F = F
  
  class(kalmanFilter) = "kalmanFilter"
  
  return(kalmanFilter)
}

# Predict next state base on the filter, that is input
kalman_predict <- function(kalmanFilter){
  
  x = kalmanFilter$F %*% kalmanFilter$x
  P = kalmanFilter$F %*% kalmanFilter$P %*% t(kalmanFilter$F) + kalmanFilter$Q
  
  return(list(x = x, P = P))
}

# Updates the filter with the measurement z
kalman_update <- function(z, kalmanFilter){
  S = kalmanFilter$H %*% kalmanFilter$P %*% t(kalmanFilter$H) + kalmanFilter$R
  K = kalmanFilter$P %*% t(kalmanFilter$H) %*% solve(S)
  
  y = z - kalmanFilter$H %*% kalmanFilter$x
  x = kalmanFilter$x + K %*% y
  P = (diag(dim(kalmanFilter$F)[1]) - K %*% kalmanFilter$H) %*% kalmanFilter$P
  
  #L = dmvn(x = y[,1], mu = (kalmanFilter$H %*% kalmanFilter$x)[,1], Sigma = S, log=TRUE)
  
  L = likelyhood_function(y[,1], (kalmanFilter$H %*% kalmanFilter$x)[,1], S)
  
  kalmanFilter$K = K
  kalmanFilter$P = P
  kalmanFilter$x = x
  kalmanFilter$L = L
  
  return(kalmanFilter)
}

# Update Transition Matrix of Acceleration Model with new delta value, (historisches Überbleibsel)
updateAccelerationF <- function(dt){
  F = matrix(data = c(
    1,0,0,dt,0,0,dt*dt/2,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,1,0,0,dt,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,dt,0,0,dt*dt/2,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,1,0,0,dt,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
    0,0,1,0,0,dt,0,0,dt*dt/2,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,1,0,0,dt,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,dt*dt/2,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,dt*dt/2,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,dt*dt/2,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
  )
  ,nrow = 18, ncol = 18, byrow=TRUE)
  return(F)
}

# Update Transition Matrix of Position Model with new delta value, (historisches Überbleibsel)
updatePositionAccelerationF <- function(dt){
  F = matrix(data = c(
    1,0,0,dt,0,0,dt*dt/2,0,0,
    0,0,0,1,0,0,dt,0,0,
    0,0,0,0,0,0,1,0,0,
    0,1,0,0,dt,0,0,dt*dt/2,0,
    0,0,0,0,1,0,0,dt,0,
    0,0,0,0,0,0,0,1,0,
    0,0,1,0,0,dt,0,0,dt*dt/2,
    0,0,0,0,0,1,0,0,dt,
    0,0,0,0,0,0,0,0,1
  )
  ,nrow = 9, ncol = 9, byrow=TRUE)
  return(F)
}

# Update Transition Matrix of Jerk Model with new delta value, (historisches Überbleibsel)
updateJerkF <- function(dt){
  d1 = dt
  d2 = dt**2/2
  d3 = dt**3/6
  
  F = matrix(data = c(
    1,0,0,  d1,0,0, d2,0,0, d3,0,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  1,0,0,  d1,0,0, d2,0,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  1,0,0,  d1,0,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,0,0,  1,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,1,0,  0,d1,0, 0,d2,0, 0,d3,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,1,0,  0,d1,0, 0,d2,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,1,0,  0,d1,0, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,1,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,1,  0,0,d1, 0,0,d2, 0,0,d3, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,1,  0,0,dt, 0,0,d2, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,0,1,  0,0,d1, 0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,1,  0,0,0,  0,0,0,  0,0,0,  0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  1,0,0,  d1,0,0, d2,0,0, d3,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  1,0,0,  d1,0,0, d2,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  1,0,0,  d1,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  1,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,1,0,  0,d1,0, 0,d2,0, 0,d3,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,1,0,  0,d1,0, 0,d2,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,1,0,  0,d1,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,1,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,1,  0,0,d1, 0,0,d2, 0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,1,  0,0,d1, 0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,1,  0,0,0,
    0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,0,  0,0,1
  )
  ,nrow = 24, ncol = 24, byrow=TRUE)
  return(F)
}

getSlope <- function(x1, x2, dt){
  return((x2 - x1) / dt)
}

# Daten import


imu_and_kinect <- read.csv2("D:/Projekte/Digital-Identity/data_science/imu_and_kinect.csv")

# Interpolieren der Kinect Daten

interpolated_kinect_foot_right_x = interpolateZeroesInbetween(imu_and_kinect$kinect_foot_right.x)
interpolated_kinect_foot_right_x = c(interpolated_kinect_foot_right_x[1], interpolated_kinect_foot_right_x, last(interpolated_kinect_foot_right_x))
interpolated_kinect_foot_right_y = interpolateZeroesInbetween(imu_and_kinect$kinect_foot_right.y)
interpolated_kinect_foot_right_y = c(interpolated_kinect_foot_right_y[1], interpolated_kinect_foot_right_y, last(interpolated_kinect_foot_right_y))
interpolated_kinect_foot_right_z = interpolateZeroesInbetween(imu_and_kinect$kinect_foot_right.z)
interpolated_kinect_foot_right_z = c(interpolated_kinect_foot_right_z[1], interpolated_kinect_foot_right_z, last(interpolated_kinect_foot_right_z))

interpolated_kinect_hand_right_x = interpolateZeroesInbetween(imu_and_kinect$kinect_hand_right.x)
interpolated_kinect_hand_right_x = c(interpolated_kinect_hand_right_x[1], interpolated_kinect_hand_right_x, last(interpolated_kinect_hand_right_x))
interpolated_kinect_hand_right_y = interpolateZeroesInbetween(imu_and_kinect$kinect_hand_right.y)
interpolated_kinect_hand_right_y = c(interpolated_kinect_hand_right_y[1], interpolated_kinect_hand_right_y, last(interpolated_kinect_hand_right_y))
interpolated_kinect_hand_right_z = interpolateZeroesInbetween(imu_and_kinect$kinect_hand_right.z)
interpolated_kinect_hand_right_z = c(interpolated_kinect_hand_right_z[1], interpolated_kinect_hand_right_z, last(interpolated_kinect_hand_right_z))

# Zeit in Millisekunden übersetzen

imu_and_kinect$t = (imu_and_kinect$t.m * 60 * 1000 + imu_and_kinect$t.s * 1000 + imu_and_kinect$t.ms)

# Erstelle Data Frame für kinect daten
# Darauf achten das datensätze gleiche Länge aufweisen

interpolatedKinectValues = data.frame(
  t = imu_and_kinect$t[1:length(imu_and_kinect$t)-1],
  x = interpolated_kinect_hand_right_x,
  y = interpolated_kinect_hand_right_y,
  z = interpolated_kinect_hand_right_z
)

# Differenziere Geschwindigkeit für Kinect Daten

diff_velocities_x = (interpolated_kinect_foot_right_x[2:(length(interpolated_kinect_foot_right_x))] - interpolated_kinect_foot_right_x[1:(length(interpolated_kinect_foot_right_x)-1)]) / (imu_and_kinect$t[2:(length(interpolated_kinect_foot_right_x))] - imu_and_kinect$t[1:(length(interpolated_kinect_foot_right_x)-1)])
diff_velocities_x = c(diff_velocities_x[1], diff_velocities_x)
diff_velocities_y = (interpolated_kinect_foot_right_y[2:(length(interpolated_kinect_foot_right_y))] - interpolated_kinect_foot_right_y[1:(length(interpolated_kinect_foot_right_y)-1)]) / (imu_and_kinect$t[2:(length(interpolated_kinect_foot_right_y))] - imu_and_kinect$t[1:(length(interpolated_kinect_foot_right_y)-1)])
diff_velocities_y = c(diff_velocities_y[1], diff_velocities_y)
diff_velocities_z = (interpolated_kinect_foot_right_z[2:(length(interpolated_kinect_foot_right_z))] - interpolated_kinect_foot_right_x[1:(length(interpolated_kinect_foot_right_z)-1)]) / (imu_and_kinect$t[2:(length(interpolated_kinect_foot_right_z))] - imu_and_kinect$t[1:(length(interpolated_kinect_foot_right_z)-1)])
diff_velocities_z = c(diff_velocities_z[1], diff_velocities_z)

interpolatedKinectDiffVelocities = data.frame(
  t = imu_and_kinect$t[1:length(imu_and_kinect$t)-1],
  x = diff_velocities_x,
  y = diff_velocities_y,
  z = diff_velocities_z
)

# Differenziere Beschleunigung von Kinect Daten 

diff_accelerations_x = (diff_velocities_x[2:(length(diff_velocities_x))] - interpolated_kinect_foot_right_x[1:(length(interpolated_kinect_foot_right_x)-1)]) / (imu_and_kinect$t[2:(length(interpolated_kinect_foot_right_x))] - imu_and_kinect$t[1:(length(diff_velocities_x)-1)])
diff_accelerations_x = c(diff_accelerations_x[1], diff_accelerations_x)
diff_accelerations_y = (diff_velocities_y[2:(length(diff_velocities_y))] - interpolated_kinect_foot_right_y[1:(length(interpolated_kinect_foot_right_y)-1)]) / (imu_and_kinect$t[2:(length(interpolated_kinect_foot_right_y))] - imu_and_kinect$t[1:(length(diff_velocities_y)-1)])
diff_accelerations_y = c(diff_accelerations_y[1], diff_accelerations_y)
diff_accelerations_z = (diff_velocities_z[2:(length(diff_velocities_z))] - interpolated_kinect_foot_right_x[1:(length(interpolated_kinect_foot_right_z)-1)]) / (imu_and_kinect$t[2:(length(interpolated_kinect_foot_right_z))] - imu_and_kinect$t[1:(length(diff_velocities_z)-1)])
diff_accelerations_z = c(diff_accelerations_z[1], diff_accelerations_z)

interpolatedKinectDiffAccelerations = data.frame(
  t = imu_and_kinect$t[1:length(imu_and_kinect$t)-1],
  x = diff_accelerations_x,
  y = diff_accelerations_y,
  z = diff_accelerations_z
)

# Get Velocity from Imu Acceleration with cumulative sum

imu_velocity_x = cumsum(imu_and_kinect$imu_acceleration.x[1:length(imu_and_kinect$t)-1]) * diff(imu_and_kinect$t)
imu_velocity_y = cumsum(imu_and_kinect$imu_acceleration.y[1:length(imu_and_kinect$t)-1]) * diff(imu_and_kinect$t)
imu_velocity_z = cumsum(imu_and_kinect$imu_acceleration.z[1:length(imu_and_kinect$t)-1]) * diff(imu_and_kinect$t)

# Get Position from Cumulative Velocity Sum from Imu Acceleration

imu_position_x = cumsum(imu_velocity_x) * diff(imu_and_kinect$t)
imu_position_y = cumsum(imu_velocity_y) * diff(imu_and_kinect$t)
imu_position_z = cumsum(imu_velocity_z) * diff(imu_and_kinect$t)


data_lower_lim = 0
data_upper_lim = 800

#processModel = getAccelerationModel_with_kinect()
#processModel = getOnlyPositionAccelerationModel()
processModel = getOnlyPositionAccelerationFusionWithDiffVeloModel()

kalmanFilter = kalman_init(
  processModel$a,
  processModel$P,
  processModel$Q,
  processModel$H,
  processModel$F,
  processModel$R
)

kalmanFilter_2 = kalman_init(
  processModel$a,
  processModel$P,
  processModel$Q,
  processModel$H,
  processModel$F,
  processModel$R
)

i = 2
result = list()
#ypr = quaternionToEulerAngles(list(x = df$gyro.x, y = df$gyro.y, z = df$gyro.z, w = df$gyro.w))
#ypr = quaternionToEulerAngles(list(x = imu_right_hand_and_kinect$imu_orientation.w,
#                                   y = imu_right_hand_and_kinect$imu_orientation.x,
#                                   z = imu_right_hand_and_kinect$imu_orientation.y,
#                                   w = imu_right_hand_and_kinect$imu_orientation.z
#                              ))
ldata = c(0)
ldata_2 = c(0)

meanQuotientXAcceleration = mean(interpolatedKinectDiffAccelerations$x / imu_and_kinect$imu_acceleration.x[1:length(imu_and_kinect$t)-1])
meanQuotientYAcceleration = mean(interpolatedKinectDiffAccelerations$y / imu_and_kinect$imu_acceleration.y[1:length(imu_and_kinect$t)-1])
meanQuotientZAcceleration = mean(interpolatedKinectDiffAccelerations$z / imu_and_kinect$imu_acceleration.z[1:length(imu_and_kinect$t)-1])

meanAccelerationQuotient = data.frame(
  x = meanQuotientXAcceleration,
  y = meanQuotientYAcceleration,
  z = meanQuotientZAcceleration
)

useBlendingByLikelyHood = TRUE


if(useBlendingByLikelyHood){
  blendIncrement = 0.01;
  blendA = 1.0;
  blendB = 0.0;
  blendBetweenFilters = FALSE;
  useFirstKalman = TRUE;
  
  while(i + 1 < length(imu_and_kinect$t[data_lower_lim:data_upper_lim])) {
    #z = rbind( df$acc.x[data_lower_lim:data_upper_lim][i],
    #           df$acc.y[data_lower_lim:data_upper_lim][i],
    #           df$acc.z[data_lower_lim:data_upper_lim][i])
    #           ypr$yaw[data_lower_lim:data_upper_lim][i],
    #           ypr$pitch[data_lower_lim:data_upper_lim][i],
    #           ypr$roll[data_lower_lim:data_upper_lim][i])
    
    z = rbind(interpolatedKinectValues$x[data_lower_lim:data_upper_lim][i],
              interpolatedKinectValues$y[data_lower_lim:data_upper_lim][i],
              interpolatedKinectValues$z[data_lower_lim:data_upper_lim][i],
              imu_and_kinect$imu_acceleration.x[data_lower_lim:data_upper_lim][i] * interpolatedKinectDiffAccelerations$x[data_lower_lim:data_upper_lim][i],
              imu_and_kinect$imu_acceleration.y[data_lower_lim:data_upper_lim][i] * interpolatedKinectDiffAccelerations$y[data_lower_lim:data_upper_lim][i],
              imu_and_kinect$imu_acceleration.z[data_lower_lim:data_upper_lim][i] * interpolatedKinectDiffAccelerations$z[data_lower_lim:data_upper_lim][i],
              interpolatedKinectDiffVelocities$x[data_lower_lim:data_upper_lim][i],
              interpolatedKinectDiffVelocities$y[data_lower_lim:data_upper_lim][i],
              interpolatedKinectDiffVelocities$z[data_lower_lim:data_upper_lim][i]
    )
    #         ypr$yaw[data_lower_lim:data_upper_lim][i],
    #         ypr$pitch[data_lower_lim:data_upper_lim][i],
    #         ypr$roll[data_lower_lim:data_upper_lim][i])
    
    data = kalman_predict(kalmanFilter)
    data_2 = kalman_predict(kalmanFilter_2)
    
    result = rbind(result, data$x[,1] * blendA + data_2$x[,1] * blendB)
    
    kalmanFilter = kalman_update(z, kalmanFilter)
    kalmanFilter_2 = kalman_update(z, kalmanFilter_2)
    
    ldata = append(ldata , kalmanFilter$L)
    ldata_2 = append(ldata_2, kalmanFilter$L_2)
    
    if(blendBetweenFilters){
      if(useFirstKalman){
        blendA = blendA + blendIncrement
        blendB = blendB - blendIncrement
        if(blendA == 1){
          blendBetweenFilters = FALSE
        }
      } else {
        blendA = blendA - blendIncrement
        blendB = blendB + blendIncrement
        if(blendB == 1){
          blendBetweenFilters = FALSE
        }
      }
    }
    
    if(kalmanFilter$L < -500000000 && useFirstKalman){
      kalmanFilter_2 = kalman_init(
        kalmanFilter$x,
        processModel$P,
        processModel$Q, 
        processModel$H,
        processModel$F,
        processModel$R
      )
      useFirstKalman = FALSE
      blendBetweenFilters = TRUE
    } else if(kalmanFilter_2$L < -500000000 && !useFirstKalman){
      kalmanFilter = kalman_init(
        kalmanFilter_2$x,
        processModel$P,
        processModel$Q, 
        processModel$H,
        processModel$F,
        processModel$R
      )
      useFirstKalman = TRUE
      blendBetweenFilters = TRUE
    }
    i = i + 1
  } 
}
if(!useBlendingByLikelyHood) {
  while(i + 1 < length(imu_and_kinect$t[data_lower_lim:data_upper_lim])) {
    
    z = rbind(interpolatedKinectValues$x[data_lower_lim:data_upper_lim][i],
              interpolatedKinectValues$y[data_lower_lim:data_upper_lim][i],
              interpolatedKinectValues$z[data_lower_lim:data_upper_lim][i],
              imu_and_kinect$imu_acceleration.x[data_lower_lim:data_upper_lim][i] * interpolatedKinectDiffAccelerations$x[data_lower_lim:data_upper_lim][i],
              imu_and_kinect$imu_acceleration.y[data_lower_lim:data_upper_lim][i] * interpolatedKinectDiffAccelerations$y[data_lower_lim:data_upper_lim][i],
              imu_and_kinect$imu_acceleration.z[data_lower_lim:data_upper_lim][i] * interpolatedKinectDiffAccelerations$z[data_lower_lim:data_upper_lim][i],
              interpolatedKinectDiffVelocities$x[data_lower_lim:data_upper_lim][i],
              interpolatedKinectDiffVelocities$y[data_lower_lim:data_upper_lim][i],
              interpolatedKinectDiffVelocities$z[data_lower_lim:data_upper_lim][i]
    )
    
    #z = rbind(
    #  imu_and_kinect$imu_acceleration.x[data_lower_lim:data_upper_lim][i],
    #  imu_and_kinect$imu_acceleration.y[data_lower_lim:data_upper_lim][i],
    #  imu_and_kinect$imu_acceleration.z[data_lower_lim:data_upper_lim][i]
    #)
    
    data = kalman_predict(kalmanFilter)
    
    result = rbind(result, data$x[,1])
    
    kalmanFilter = kalman_update(z, kalmanFilter)
    
    ldata = append(ldata, kalmanFilter$L)
    
    i = i + 1
  } 
}

likelyhoodDf = data.frame(t = imu_and_kinect$t[1:(length(ldata))], l=ldata)

colnames(result) = processModel$colNames
result = data.frame(result)
result = rbind(result, 0)
result = rbind(result, 0)
result = rbind(result, 0)

plot_up_limit = 700
plot_low_limit = 0

#plotDf = data.frame(t = imu_right_hand_and_kinect$t,
#                    pos.x = imu_right_hand_and_kinect$kinect_hand_right.x,
#                    pos.y = imu_right_hand_and_kinect$kinect_hand_right.x,
#                    pos.z = imu_right_hand_and_kinect$kinect_hand_right.x,
#                    acc.x = imu_right_hand_and_kinect$imu_acceleration.x,
#                    acc.y = imu_right_hand_and_kinect$imu_acceleration.y,
#                    acc.z = imu_right_hand_and_kinect$imu_acceleration.z)

isPlottingAccelerationResults = data.frame(
  x=TRUE,
  y=TRUE,
  z=TRUE
)
isPlottingPositionResults = data.frame(
  x=TRUE,
  y=TRUE,
  z=TRUE
  )
isPlottingGyroResults = data.frame(
  yaw=FALSE,
  pitch=FALSE,
  roll=FALSE
)
isPlottingLikelyhood = TRUE

plotDf = data.frame(t = imu_and_kinect$t[1:length(imu_and_kinect$t)-1],
                    x = interpolatedKinectValues$x,
                    y = interpolatedKinectValues$y,
                    z = interpolatedKinectValues$z,
                    acc.x = imu_and_kinect$imu_acceleration.x[1:length(imu_and_kinect$t)-1],
                    acc.y = imu_and_kinect$imu_acceleration.y[1:length(imu_and_kinect$t)-1],
                    acc.z = imu_and_kinect$imu_acceleration.z[1:length(imu_and_kinect$t)-1]
)

if(isPlottingAccelerationResults$x) {
  # x acc plot
  accxdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      a_x = plotDf$acc.x[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * interpolatedKinectDiffAccelerations$x[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  accxdf$K_x = as.integer(result$a_x[plot_low_limit:plot_up_limit])
  accxdf_long = melt(accxdf, id = "t")
  ggplot(accxdf_long, aes(x=t, y=value, color=variable)) + geom_line()
}

if(isPlottingAccelerationResults$y) {
  # y acc plot
  accydf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      a_y = plotDf$acc.y[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * interpolatedKinectDiffAccelerations$y[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  accydf$K_y = as.integer(result$a_y[plot_low_limit:plot_up_limit])
  accydf_long = melt(accydf, id = "t")
  ggplot(accydf_long, aes(x=t, y=value, color=variable)) + geom_line()
}

if(isPlottingAccelerationResults$z) {
  # z acc plot
  acczdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      a_z = plotDf$acc.z[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * interpolatedKinectDiffAccelerations$z[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  acczdf$K_z = as.integer(result$a_z[plot_low_limit:plot_up_limit])
  acczdf_long = melt(acczdf, id = "t")
  ggplot(acczdf_long, aes(x=t, y=value, color=variable)) + geom_line()  
}

plotList = list()

if(isPlottingPositionResults$x) {
  # x plot
  xdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      x = plotDf$x[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  xdf$K_x = as.numeric(result$x[plot_low_limit:plot_up_limit])
  xdf_long = melt(xdf, id = "t")
  ggplot(xdf_long, aes(x=t, y=value, color=variable)) + geom_line()
}

if(isPlottingPositionResults$y) {
  # y plot
  ydf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                   y = plotDf$y[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  ydf$K_y = as.numeric(result$y[plot_low_limit:plot_up_limit])
  ydf_long = melt(ydf, id = "t")
  ggplot(ydf_long, aes(x=t, y=value, color=variable)) + geom_line()
}

if(isPlottingPositionResults$z) {
  # z plot
  zdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                   z = plotDf$z[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  zdf$K_z = as.numeric(result$z[plot_low_limit:plot_up_limit])
  zdf_long = melt(zdf, id = "t")
  ggplot(zdf_long, aes(x=t, y=value, color=variable)) + geom_line()
}

if(isPlottingGyroResults$yaw) {
  # yaw plot
  gyroYawdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                         yaw = ypr$yaw[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  gyroYawdf$K_yaw = as.numeric(result$yaw[plot_low_limit:plot_up_limit])
  gyroYawdf_long = melt(gyroYawdf, id = "t")
  ggplot(gyroYawdf_long, aes(x=t, y=value, color=variable)) + geom_line()
}

if(isPlottingGyroResults$pitch) {
  # pitch plot
  gyroPitchdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                           pitch = ypr$pitch[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  gyroPitchdf$K_pitch = as.numeric(result$pitch[plot_low_limit:plot_up_limit])
  gyroPitchdf_long = melt(gyroPitchdf, id = "t")
  ggplot(gyroPitchdf_long, aes(x=t, y=value, color=variable)) + geom_line()
}

if(isPlottingGyroResults$roll) {
  # roll plot
  gyroRolldf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                          roll = ypr$roll[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  gyroRolldf$K_roll = as.numeric(result$roll[plot_low_limit:plot_up_limit])
  gyroRolldf_long = melt(gyroRolldf, id = "t")
  ggplot(gyroRolldf_long, aes(x=t, y=value, color=variable)) + geom_line()
}

ggplot(likelyhoodDf, aes(x=t, y=l, color='likelyhood')) + geom_line()  

#residuals_data = list()
#residuals_data$x = abs(as.double(result$x)) - abs(as.double(df$acc.x[data_lower_lim:data_upper_lim]))

#residuals = list()
#residuals$x = sum(abs(as.double(result$x)) - abs(as.double(df$acc.x[data_lower_lim:data_upper_lim]))) / length(result$x)
#residuals$y = sum(abs(as.double(result$y)) - abs(as.double(df$acc.y[data_lower_lim:data_upper_lim]))) / length(result$y)
#residuals$z = sum(abs(as.double(result$z)) - abs(as.double(df$acc.z[data_lower_lim:data_upper_lim]))) / length(result$z)
#residuals$yaw = sum(abs(as.double(result$yaw)) - abs(as.double(ypr$yaw[data_lower_lim:data_upper_lim]))) / length(result$yaw)
#residuals$pitch = sum(abs(as.double(result$pitch)) - abs(as.double(ypr$pitch[data_lower_lim:data_upper_lim]))) / length(result$pitch)
#residuals$roll = sum(abs(as.double(result$roll)) - abs(as.double(ypr$roll[data_lower_lim:data_upper_lim]))) / length(result$roll)

#residuals$x
