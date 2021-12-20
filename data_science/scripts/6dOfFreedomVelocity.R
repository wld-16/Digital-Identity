# install packages
install.packages("RMThreshold")
install.packages("jsonlite")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("patchwork")
install.packages("reshape2")
install.packages("RSpincalc")
install.packages("LaplacesDemon")
install.packages("dlm")
install.packages("onion")

# laden der Libraries

library(dplyr)
library(RMThreshold)
library(ggplot2)
library(RSpincalc)
library(reshape2)
library(LaplacesDemon)
library(onion)



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
## Get 6 Degrees model
get6AxisOfFreedomVelocityModel <- function(initialState, initialBelief, isPositionSensor = FALSE, isAccelerationSensor = FALSE){
  #Currently used variable names -> variable names from Slides
  # F -> A: State Transition Matrix
  # a -> x: State Vector
  # H -> C: Observation Matrix
  # Q -> Q: Process Noise Matrix
  # R -> R: Sensor Noise Matrix
  # P -> P: Sensor Belief
  
  velocityModel = list()
  velocityModel$F = matrix(data = c(
    1,0,0,dt,0,0,
    0,1,0,0,dt,0,
    0,0,1,0,0,dt,
    0,0,0,1,0,0,
    0,0,0,0,1,0,
    0,0,0,0,0,1
  )
  ,nrow = 6, ncol = 6, byrow=TRUE)
  
  velocityModel$a = matrix(data = initialState, nrow = 6, ncol = 1) ## state vector
  
  if(isAccelerationSensor){
    velocityModel$H = matrix(data = c(
      0,0,0,1,0,0,
      0,0,0,0,1,0,
      0,0,0,0,0,1
    ), nrow = 3, ncol = 6) ## observation/ measurment vector 
  }
  if(isPositionSensor) {
    velocityModel$H = matrix(data = c(
      1,0,0,0,0,0,
      0,1,0,0,0,0,
      0,0,1,0,0,0
    ), nrow = 3, ncol = 6, byrow=TRUE) ## observation/ measurment vector  
  }
  
  #velocityModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 6, ncol = 6), mean = 0, stddev = 2) ## white noise as process noise
  
  Q = matrix(data = c(
    0.01**2,0,0,
    0,01**2,0,
    0,0,0.01**2
  ), nrow = 3, ncol = 3, byrow=TRUE)
  
  velocityModel$Gd = matrix(data =c(
    dt**2/2, 0, 0, dt**2/2, 0, 0,
    0, dt**2/2, 0, 0, dt**2/2, 0,
    0, 0, dt**2/2, 0, 0, dt**2/2,
    dt, 0, 0, dt, 0, 0,
    0, dt, 0, 0, dt, 0,
    0, 0, dt, 0, 0, dt
  ), nrow=6, ncol=3, byrow = TRUE)
  
  velocityModel$Q = velocityModel$Gd %*% Q %*% t(velocityModel$Gd)
  
  velocityModel$P = initialBelief  
  
  x_sigma = 500; xy_sigma = 0; xz_sigma = 0 
  yx_sigma = 0; y_sigma = 500; yz_sigma = 0 
  zx_sigma = 0; zy_sigma = 0; z_sigma = 100
  
  
  velocityModel$R = matrix(data = c(x_sigma**2, xy_sigma, xz_sigma, 
                                    yx_sigma, y_sigma**2, yz_sigma, 
                                    zx_sigma, zy_sigma, z_sigma**2
  ), nrow = 3, ncol = 3) 
  
  velocityModel$colNames = c("x","y","z","v_x","v_y","v_z")
  
  return(velocityModel)
}

## Get 6 Degrees model
get6AxisOfFreedomVelocityModelUnity <- function(){
  velocityModel = list()
  velocityModel$F = matrix(data = c(
    1,0,0,dt,0,0,
    0,0,0,1,0,0,
    0,1,0,0,dt,0,
    0,0,0,0,1,0,
    0,0,1,0,0,dt,
    0,0,0,0,0,1
  )
  ,nrow = 6, ncol = 6, byrow=TRUE)
  
  velocityModel$a = matrix(data = c(0.055,-0.0964,0.28,3.9,-3.46,18.78), nrow = 6, ncol = 1) ## state vector
  
  velocityModel$H = matrix(data = c(
    0,0,0,10,0,0,
    0,0,0,0,10,0,
    0,0,0,0,0,1000
  ), nrow = 3, ncol = 6, byrow=TRUE) ## observation/ measurement vector 
  
  velocityModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 6, ncol = 6), mean = 0, stddev = 2) ## white noise as process noise
  velocityModel$P = diag(c(0,0,0,1,0.7,0.7))
  
  x_sigma = 0.02; xy_sigma = 0; xz_sigma = 0 
  yx_sigma = 0; y_sigma = 0.02; yz_sigma = 0 
  zx_sigma = 0; zy_sigma = 0; z_sigma = 0.02
  
  
  velocityModel$R = matrix(data = c(x_sigma**2, xy_sigma, xz_sigma, 
                                    yx_sigma, y_sigma**2, yz_sigma, 
                                    zx_sigma, zy_sigma, z_sigma**2
  ), nrow = 3, ncol = 3) 
  
  velocityModel$colNames = c("x","v_x","y","v_y","z","v_z")
  
  return(velocityModel)
} 

## Get 6 Degrees model
get6degFusion <- function(initialState, initialBelief){
  velocityModel = list()
  velocityModel$F = matrix(data = c(
    1,0,0,dt,0,0,
    0,1,0,0,dt,0,
    0,0,1,0,0,dt,
    0,0,0,1,0,0,
    0,0,0,0,1,0,
    0,0,0,0,0,1
  )
  ,nrow = 6, ncol = 6, byrow=TRUE)
  
  velocityModel$a = matrix(data = initialState, nrow = 6, ncol = 1) ## state vector
  
  velocityModel$H = matrix(data = c(
    1,0,0,0,0,0,
    0,1,0,0,0,0,  
    0,0,1,0,0,0,
    0,0,0,10,0,0,
    0,0,0,0,10,0,
    0,0,0,0,0,1000
  ), nrow = 6, ncol = 6, byrow=TRUE) ## observation/ measurement vector 
  
  #velocityModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 6, ncol = 6), mean = 0, stddev = 2) ## white noise as process noise
  velocityModel$P = initialBelief  
  
  x_sigma = 0.02; y_sigma = 0.02; z_sigma = 0.02
  vx_sigma = 0.02; vy_sigma = 0.02; vz_sigma = 0.02
  
  Q = matrix(data = c(
    1,0,0,0,0,0,
    0,1,0,0,0,0,
    0,0,1,0,0,0,
    0,0,0,10**2,0,0,
    0,0,0,0,10**2,0,
    0,0,0,0,0,0.5**2
  ), nrow = 6, ncol = 6, byrow=TRUE)
  
  velocityModel$Gd = matrix(data =c(
    dt**2/2, 0, 0, 0, 0, 0,
    0, dt**2/2, 0, 0, 0, 0,
    0, 0, dt**2/2, 0, 0, 0,
    dt,0,0, dt, 0, 0,
    0,0,0, 0, dt, 0,
    0,0,0, 0, 0, dt
  ), nrow=6, ncol=6, byrow = TRUE)
  
  velocityModel$Q = velocityModel$Gd %*% Q %*% t(velocityModel$Gd)
  
  
  velocityModel$R = diag(c(x_sigma**2, y_sigma**2, z_sigma**2, vx_sigma**2, vy_sigma**2, vz_sigma**2))
  
  velocityModel$colNames = c("x","v_x","y","v_y","z","v_z")
  
  return(velocityModel)
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

dlm_init <- function(x,P,Q,H,F,R){
  kalmanFilter <- list()
  
  kalmanFilter$x = x
  kalmanFilter$P = P
  kalmanFilter$Q = Q
  kalmanFilter$H = H
  kalmanFilter$R = R
  kalmanFilter$K = P %*% t(H) %*% solve(H %*% P %*% t(H) + R)
  kalmanFilter$F = F
  
  #V, W (covariance matrices of the measurement and state equations, respectively)
  # FF and GG (measurement equation matrix and transition matrix respectively)
  # m0, C0 (prior mean and covariance matrix of the state vector)
  
  return(dlm(FF = H, V = R, GG = F, W = Q, m0 = x, C0 = P))
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
  x = kalmanFilter$x + K %*% (y - kalmanFilter$H %*% kalmanFilter$x)
  P = (diag(dim(kalmanFilter$F)[1]) - K %*% kalmanFilter$H) %*% kalmanFilter$P
  
  #L = dmvn(x = y[,1], mu = (kalmanFilter$H %*% kalmanFilter$x)[,1], Sigma = S, log=TRUE)
  
  L = likelyhood_function(y[,1], (kalmanFilter$H %*% kalmanFilter$x)[,1], S)
  
  kalmanFilter$K = K
  kalmanFilter$P = P
  kalmanFilter$x = x
  kalmanFilter$L = L
  
  return(kalmanFilter)
}

getSlope <- function(x1, x2, dt){
  return((x2 - x1) / dt)
}

# Daten import


imu_and_kinect <- read.csv2("D:/Projekte/Digital-Identity/data_science/imu_and_kinect_with_orientation.csv")

# Interpolieren der Kinect Daten

interpolated_kinect_foot_right_x = interpolateZeroesInbetween(imu_and_kinect$kinect_foot_right.x)
interpolated_kinect_foot_right_y = interpolateZeroesInbetween(imu_and_kinect$kinect_foot_right.y)
interpolated_kinect_foot_right_z = interpolateZeroesInbetween(imu_and_kinect$kinect_foot_right.z)

interpolated_kinect_hand_right_x = interpolateZeroesInbetween(imu_and_kinect$kinect_hand_right.x)
interpolated_kinect_hand_right_y = interpolateZeroesInbetween(imu_and_kinect$kinect_hand_right.y)
interpolated_kinect_hand_right_z = interpolateZeroesInbetween(imu_and_kinect$kinect_hand_right.z)

imu_and_kinect = imu_and_kinect[0:length(interpolated_kinect_hand_right_z),]

# Zeit in Millisekunden ?bersetzen

imu_and_kinect$t = (imu_and_kinect$t.m * 60 * 1000 + imu_and_kinect$t.s * 1000 + imu_and_kinect$t.ms)

# Erstelle Data Frame f?r kinect daten
# Darauf achten das datens?tze gleiche L?nge aufweisen

interpolatedKinectValues = data.frame(
  t = imu_and_kinect$t[1:length(imu_and_kinect$t)-1],
  x = interpolated_kinect_hand_right_x,
  y = interpolated_kinect_hand_right_y,
  z = interpolated_kinect_hand_right_z
)

# Differenziere Geschwindigkeit f?r Kinect Daten

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
data_upper_lim = 2200


processModelImu = get6AxisOfFreedomVelocityModel(c(0,0,180000, 0, 0, 18000), diag(c(1,1,1,0,0,0)), isAccelerationSensor = TRUE)
processModelKinect = get6AxisOfFreedomVelocityModel(c(0.00,0.0,0,0,0,0), diag(c(0.5,0.4,0.2,0,0,0)), isPositionSensor = TRUE)
processModelImuUnity = get6AxisOfFreedomVelocityModelUnity()
processModelFusion = get6degFusion(c(0,0,0,1,1,5), diag(c(1,1,1,1,0.7,0.7)))

kalmanFilterImu = kalman_init(
  processModelImu$a,
  processModelImu$P,
  processModelImu$Q,
  processModelImu$H,
  processModelImu$F,
  processModelImu$R
)

dlmImu = dlm_init(processModelImu$a,
         processModelImu$P,
         processModelImu$Q,
         processModelImu$H,
         processModelImu$F,
         processModelImu$R
)

dlmKinect = dlm_init(processModelKinect$a,
                     processModelKinect$P,
                     processModelKinect$Q,
                     processModelKinect$H,
                     processModelKinect$F,
                     processModelKinect$R
                     )

dlmFusion = dlm_init(
  processModelFusion$a,
  processModelFusion$P,
  processModelFusion$Q,
  processModelFusion$H,
  processModelFusion$F,
  processModelFusion$R
)

kalmanFilterKinect = kalman_init(
  processModelKinect$a,
  processModelKinect$P,
  processModelKinect$Q,
  processModelKinect$H,
  processModelKinect$F,
  processModelKinect$R
)

kalmanFilterImuUnity = kalman_init(
  processModelImuUnity$a,
  processModelImuUnity$P,
  processModelImuUnity$Q,
  processModelImuUnity$H,
  processModelImuUnity$F,
  processModelImuUnity$R
)

kalmanFilterFusion = kalman_init(
  processModelFusion$a,
  processModelFusion$P,
  processModelFusion$Q,
  processModelFusion$H,
  processModelFusion$F,
  processModelFusion$R
)

i = 2
resultImu = list()
resultKinect = list()
resultImuUnity = list()
resultFusion = list()
resultDlmImu = list()
resultDlmKinect = list()
resultDlmFusion = list()

ldata = c(0)

meanQuotientXAcceleration = mean(interpolatedKinectDiffAccelerations$x / imu_and_kinect$imu_acceleration.x[1:length(imu_and_kinect$t)-1])
meanQuotientYAcceleration = mean(interpolatedKinectDiffAccelerations$y / imu_and_kinect$imu_acceleration.y[1:length(imu_and_kinect$t)-1])
meanQuotientZAcceleration = mean(interpolatedKinectDiffAccelerations$z / imu_and_kinect$imu_acceleration.z[1:length(imu_and_kinect$t)-1])

meanAccelerationQuotient = data.frame(
  x = meanQuotientXAcceleration,
  y = meanQuotientYAcceleration,
  z = meanQuotientZAcceleration
)

while(i + 1 < length(imu_and_kinect$t[data_lower_lim:data_upper_lim])) {
  z_imu = rbind(
    imu_and_kinect$imu_acceleration.x[data_lower_lim:data_upper_lim][i],
    imu_and_kinect$imu_acceleration.y[data_lower_lim:data_upper_lim][i],
    imu_and_kinect$imu_acceleration.z[data_lower_lim:data_upper_lim][i]
  )
  
  z_kinect = rbind(
    interpolatedKinectValues$x[data_lower_lim:data_upper_lim][i],
    interpolatedKinectValues$y[data_lower_lim:data_upper_lim][i],
    interpolatedKinectValues$z[data_lower_lim:data_upper_lim][i]
  )
  
  z_fusion = rbind(
    interpolatedKinectValues$x[data_lower_lim:data_upper_lim][i],
    interpolatedKinectValues$y[data_lower_lim:data_upper_lim][i],
    interpolatedKinectValues$z[data_lower_lim:data_upper_lim][i],
    imu_and_kinect$imu_acceleration.x[data_lower_lim:data_upper_lim][i],
    imu_and_kinect$imu_acceleration.y[data_lower_lim:data_upper_lim][i],
    imu_and_kinect$imu_acceleration.z[data_lower_lim:data_upper_lim][i]
  )
  
  
  dataImu = kalman_predict(kalmanFilterImu)
  dataKinect = kalman_predict(kalmanFilterKinect)
  dataImuUnity = kalman_predict(kalmanFilterImuUnity)
  dataFusion = kalman_predict(kalmanFilterFusion)
  
  resultImu = rbind(resultImu, dataImu$x[,1])
  resultKinect = rbind(resultKinect, dataKinect$x[,1])
  resultImuUnity = rbind(resultImuUnity, dataImuUnity$x[,1])
  resultFusion = rbind(resultFusion, dataFusion$x[,1])
  
  kalmanFilterImu = kalman_update(z_imu, kalmanFilterImu)
  kalmanFilterKinect = kalman_update(z_kinect, kalmanFilterKinect)
  kalmanFilterImuUnity = kalman_update(z_imu, kalmanFilterImuUnity)
  kalmanFilterFusion = kalman_update(z_fusion, kalmanFilterFusion)
  
  i = i + 1
}

dlmDataKinect = dlmFilter(
  rbind(
    interpolatedKinectValues$x[data_lower_lim:data_upper_lim],
    interpolatedKinectValues$y[data_lower_lim:data_upper_lim],
    interpolatedKinectValues$z[data_lower_lim:data_upper_lim]
  ), dlmKinect, debug = FALSE, simplify = FALSE
)

dlmDataImu = dlmFilter(
  rbind(
    imu_and_kinect$imu_acceleration.x[data_lower_lim:data_upper_lim],
    imu_and_kinect$imu_acceleration.y[data_lower_lim:data_upper_lim],
    imu_and_kinect$imu_acceleration.z[data_lower_lim:data_upper_lim]
    ), dlmImu, debug = FALSE, simplify = FALSE
  )

dlmDataFusion = dlmFilter(
  rbind(
    interpolatedKinectValues$x[data_lower_lim:data_upper_lim],
    interpolatedKinectValues$y[data_lower_lim:data_upper_lim],
    interpolatedKinectValues$z[data_lower_lim:data_upper_lim],
    imu_and_kinect$imu_acceleration.x[data_lower_lim:data_upper_lim],
    imu_and_kinect$imu_acceleration.y[data_lower_lim:data_upper_lim],
    imu_and_kinect$imu_acceleration.z[data_lower_lim:data_upper_lim]
  ), dlmFusion, debug = FALSE, simplify = FALSE
)

resultDlmImu = data.frame(x=dlmDataImu[["f"]][1,], y=dlmDataImu[["f"]][2,], z=dlmDataImu[["f"]][3,])
resultDlmKinect = data.frame(x=dlmDataKinect[["f"]][1,], y=dlmDataKinect[["f"]][2,], z=dlmDataKinect[["f"]][3,])
resultDlmFusion = data.frame(
  x=dlmDataFusion[["f"]][1,],
  y=dlmDataFusion[["f"]][2,],
  z=dlmDataFusion[["f"]][3,],
  vx=dlmDataFusion[["f"]][4,],
  vy=dlmDataFusion[["f"]][5,],
  vz=dlmDataFusion[["f"]][6,]
  )

colnames(resultDlmImu) = c("v_x","v_y","v_z")
resultDlmImu = data.frame(resultDlmImu)
resultDlmImu = rbind(resultDlmImu, 0)
resultDlmImu = rbind(resultDlmImu, 0)
resultDlmImu = rbind(resultDlmImu, 0)

colnames(resultDlmKinect) = c("x","y","z")
resultDlmKinect = data.frame(resultDlmKinect)
resultDlmKinect = rbind(resultDlmKinect, 0)
resultDlmKinect = rbind(resultDlmKinect, 0)
resultDlmKinect = rbind(resultDlmKinect, 0)

colnames(resultDlmFusion) = c("x","y","z","v_x","v_y","v_z")
resultDlmFusion = data.frame(resultDlmKinect)
resultDlmFusion = rbind(resultDlmFusion, 0)
resultDlmFusion = rbind(resultDlmFusion, 0)
resultDlmFusion = rbind(resultDlmFusion, 0)


colnames(resultImu) = processModelImu$colNames
resultImu = data.frame(resultImu)
resultImu = rbind(resultImu, 0)
resultImu = rbind(resultImu, 0)
resultImu = rbind(resultImu, 0)

colnames(resultImuUnity) = processModelImuUnity$colNames
resultImuUnity = data.frame(resultImuUnity)
resultImuUnity = rbind(resultImuUnity, 0)
resultImuUnity = rbind(resultImuUnity, 0)
resultImuUnity = rbind(resultImuUnity, 0)

colnames(resultKinect) = processModelKinect$colNames
resultKinect = data.frame(resultKinect)
resultKinect = rbind(resultKinect, 0)
resultKinect = rbind(resultKinect, 0)
resultKinect = rbind(resultKinect, 0)

colnames(resultFusion) = processModelFusion$colNames
resultFusion = data.frame(resultFusion)
resultFusion = rbind(resultFusion, 0)
resultFusion = rbind(resultFusion, 0)
resultFusion = rbind(resultFusion, 0)

plot_up_limit = 1600
plot_low_limit = 0



plotDf = data.frame(t = imu_and_kinect$t[1:length(imu_and_kinect$t)-1],
                    acc.x = imu_and_kinect$imu_acceleration.x[1:length(imu_and_kinect$t)-1],
                    acc.y = imu_and_kinect$imu_acceleration.y[1:length(imu_and_kinect$t)-1],
                    acc.z = imu_and_kinect$imu_acceleration.z[1:length(imu_and_kinect$t)-1],
                    kin.x = interpolatedKinectValues$x[1:length(imu_and_kinect$t)-1],
                    kin.y = interpolatedKinectValues$y[1:length(imu_and_kinect$t)-1],
                    kin.z = interpolatedKinectValues$z[1:length(imu_and_kinect$t)-1]
)

isPlottingResults = data.frame(
  x=FALSE,
  y=FALSE,
  z=FALSE
)
isPlottingKinectResults = data.frame(
  x=FALSE,
  y=FALSE,
  z=FALSE
)
isPlottingUnityResults = data.frame(
  x=FALSE,
  y=FALSE,
  z=FALSE
)
isPlottingFusionResults = data.frame( 
  x=TRUE,
  y=TRUE,
  z=TRUE
)
isPlottingDlmImuResults = data.frame(
  x=TRUE,
  y=TRUE,
  z=TRUE
)
isPlottingDlmKinectResults = data.frame(
  x=FALSE,
  y=FALSE,
  z=FALSE
)
isPlottingDlmFusionResults = data.frame(
  x=TRUE,
  y=TRUE,
  z=TRUE
)


if(isPlottingResults$x) {
  # x acc plot
  accxdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_x = plotDf$acc.x[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  accxdf$K_x = as.integer(resultImu$v_x[plot_low_limit:plot_up_limit])
  accxdf_long = melt(accxdf, id = "t")
  ggplot(accxdf_long, aes(x=t, y=value, color=variable)) + geom_line() 
}
if(isPlottingResults$y) {
  # y acc plot
  accydf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_y = plotDf$acc.y[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  accydf$K_y = as.integer(resultImu$v_y[plot_low_limit:plot_up_limit])
  accydf_long = melt(accydf, id = "t")
  ggplot(accydf_long, aes(x=t, y=value, color=variable)) + geom_line()
}
if(isPlottingResults$z) {
  # z acc plot
  acczdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_z = plotDf$acc.z[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  acczdf$K_z = as.integer(resultImu$v_z[plot_low_limit:plot_up_limit])
  acczdf_long = melt(acczdf, id = "t")
  ggplot(acczdf_long, aes(x=t, y=value, color=variable)) + geom_line()  
}

if(isPlottingUnityResults$x) {
  # x acc Unity plot
  accxudf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                       accelerometer_x = (plotDf$acc.x[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] + 2000) * 0.00005)
  accxudf$K_x = as.double(resultImuUnity$x[plot_low_limit:plot_up_limit])
  accxudf_long = melt(accxudf, id = "t")
  ggplot(accxudf_long, aes(x=t, y=value, color=variable)) + geom_line()
}
if(isPlottingUnityResults$y) {
  # y acc Unity plot
  accyudf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                       accelerometer_y = (plotDf$acc.y[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] -1000) * 0.0001)
  accyudf$K_y = as.double(resultImuUnity$y[plot_low_limit:plot_up_limit])
  accyudf_long = melt(accyudf, id = "t")
  ggplot(accyudf_long, aes(x=t, y=value, color=variable)) + geom_line()
}
if(isPlottingUnityResults$z) {
  # z acc Unity plot
  acczudf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_z = (plotDf$acc.z[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] + 524200) * 0.000001)
  acczudf$K_z = as.double(resultImuUnity$z[plot_low_limit:plot_up_limit])
  acczudf_long = melt(acczudf, id = "t")
  ggplot(acczudf_long, aes(x=t, y=value, color=variable)) + geom_line()  
}

if(isPlottingKinectResults$x) {
  # x acc plot
  accxdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      kinect_x = plotDf$kin.x[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  accxdf$K_x = as.double(resultKinect$x[plot_low_limit:plot_up_limit])
  accxdf_long = melt(accxdf, id = "t")
  ggplot(accxdf_long, aes(x=t, y=value, color=variable)) + geom_line() 
}
if(isPlottingKinectResults$y) {
  # y acc plot
  accydf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      kinect_y = plotDf$kin.y[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  accydf$K_y = as.double(resultKinect$y[plot_low_limit:plot_up_limit])
  accydf_long = melt(accydf, id = "t")
  ggplot(accydf_long, aes(x=t, y=value, color=variable)) + geom_line()
}
if(isPlottingKinectResults$z) {
  # z acc plot
  acczdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      kinect_z = plotDf$kin.z[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  acczdf$K_z = as.double(resultKinect$z[plot_low_limit:plot_up_limit])
  acczdf_long = melt(acczdf, id = "t")
  ggplot(acczdf_long, aes(x=t, y=value, color=variable)) + geom_line()  
}

if(isPlottingFusionResults$x) {
  # x acc plot
  accxdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_x = (plotDf$acc.x[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] *0.0001),
                      kinect_x = plotDf$kin.x[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * 2)
  accxdf$K_x = as.double(resultFusion$x[plot_low_limit:plot_up_limit])
  accxdf_long = melt(accxdf, id = "t")
  ggplot(accxdf_long, aes(x=t, y=value, color=variable)) + geom_line() + ylim(-2, 2)
}
if(isPlottingFusionResults$y) {
  # y acc plot
  accydf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_y = (plotDf$acc.y[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * 0.0001),
                      kinect_y = plotDf$kin.y[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * 2)
  accydf$K_y = as.double(resultFusion$y[plot_low_limit:plot_up_limit])
  accydf_long = melt(accydf, id = "t")
  ggplot(accydf_long, aes(x=t, y=value, color=variable)) + geom_line() + ylim(-2, 2)
}
if(isPlottingFusionResults$z) {
  # z acc plot
  acczdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_z = (plotDf$acc.z[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * 0.00005),
                      kinect_z = plotDf$kin.z[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  acczdf$K_z = as.double(resultFusion$z[plot_low_limit:plot_up_limit])
  acczdf_long = melt(acczdf, id = "t")
  ggplot(acczdf_long, aes(x=t, y=value, color=variable)) + geom_line() 
}

if(isPlottingDlmImuResults$x) {
  # x acc plot
  accxdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_x = plotDf$acc.x[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  accxdf$K_x = as.integer(resultDlmImu$v_x[plot_low_limit:plot_up_limit])
  accxdf_long = melt(accxdf, id = "t")
  ggplot(accxdf_long, aes(x=t, y=value, color=variable)) + geom_line() 
}
if(isPlottingDlmImuResults$y) {
  # y acc plot
  accydf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_y = plotDf$acc.y[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  accydf$K_y = as.integer(resultDlmImu$v_y[plot_low_limit:plot_up_limit])
  accydf_long = melt(accydf, id = "t")
  ggplot(accydf_long, aes(x=t, y=value, color=variable)) + geom_line()
}
if(isPlottingDlmImuResults$z) {
  # z acc plot
  acczdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_z = plotDf$acc.z[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  acczdf$K_z = as.integer(resultDlmImu$v_z[plot_low_limit:plot_up_limit])
  acczdf_long = melt(acczdf, id = "t")
  ggplot(acczdf_long, aes(x=t, y=value, color=variable)) + geom_line()  
}

if(isPlottingDlmKinectResults$x) {
  # x acc plot
  accxdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      kinect_x = plotDf$kin.x[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  accxdf$K_x = as.double(resultDlmKinect$x[plot_low_limit:plot_up_limit])
  accxdf_long = melt(accxdf, id = "t")
  ggplot(accxdf_long, aes(x=t, y=value, color=variable)) + geom_line() 
}
if(isPlottingDlmKinectResults$y) {
  # y acc plot
  accydf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      kinect_y = plotDf$kin.y[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  accydf$K_y = as.double(resultDlmKinect$y[plot_low_limit:plot_up_limit])
  accydf_long = melt(accydf, id = "t")
  ggplot(accydf_long, aes(x=t, y=value, color=variable)) + geom_line()
}
if(isPlottingDlmKinectResults$z) {
  # z acc plot
  acczdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      kinect_z = plotDf$kin.z[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  acczdf$K_z = as.double(resultDlmKinect$z[plot_low_limit:plot_up_limit])
  acczdf_long = melt(acczdf, id = "t")
  ggplot(acczdf_long, aes(x=t, y=value, color=variable)) + geom_line()  
}

if(isPlottingDlmFusionResults$x) {
  # x acc plot
  accxdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_x = (plotDf$acc.x[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] *0.0001),
                      kinect_x = plotDf$kin.x[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * 2)
  accxdf$K_x = as.double(resultDlmFusion$x[plot_low_limit:plot_up_limit])
  accxdf_long = melt(accxdf, id = "t")
  ggplot(accxdf_long, aes(x=t, y=value, color=variable)) + geom_line() + ylim(-2, 2)
}
if(isPlottingDlmFusionResults$y) {
  # y acc plot
  accydf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_y = (plotDf$acc.y[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * 0.0001),
                      kinect_y = plotDf$kin.y[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * 2)
  accydf$K_y = as.double(resultDlmFusion$y[plot_low_limit:plot_up_limit])
  accydf_long = melt(accydf, id = "t")
  ggplot(accydf_long, aes(x=t, y=value, color=variable)) + geom_line() + ylim(-2, 2)
}
if(isPlottingDlmFusionResults$z) {
  # z acc plot
  acczdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_z = (plotDf$acc.z[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * 0.00005),
                      kinect_z = plotDf$kin.z[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  acczdf$K_z = as.double(resultDlmFusion$z[plot_low_limit:plot_up_limit])
  acczdf_long = melt(acczdf, id = "t")
  ggplot(acczdf_long, aes(x=t, y=value, color=variable)) + geom_line() 
}
