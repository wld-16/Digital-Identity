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

dlmGuanglongAccelerationModel = dlm::dlm(
  FF = processAccelerationModel$H,
  V = diag(nrow = 6, ncol = 6),
  GG = processAccelerationModel$Ad, 
  W = processAccelerationModel$Q, 
  m0 = processAccelerationModel$x,
  C0 = processAccelerationModel$P
)



processModel = get12DimensionsAccelerationModel(
  c(params$x_initial_sx,params$x_initial_vx,params$x_initial_ax,params$x_initial_gx,
    params$x_initial_sy,params$x_initial_vy,params$x_initial_ay,params$x_initial_gy,
    params$x_initial_sz,params$x_initial_vz,params$x_initial_az,params$x_initial_gz),
  diag(c(
    params$initial_belief_11,
    params$initial_belief_22,
    params$initial_belief_33,
    params$initial_belief_44,
    params$initial_belief_55,
    params$initial_belief_66,
    params$initial_belief_77,
    params$initial_belief_88,
    params$initial_belief_99,
    params$initial_belief_00,
    params$initial_belief_AA,
    params$initial_belief_BB)))

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
## Get 9 Degrees model
get9degAccelerationModel <-function(initialState, initialBelief, isAccelerationSensor=FALSE, isPositionSensor=FALSE){
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
  
  accelerationModel$a = matrix(data = initialState, nrow = 9, ncol = 1) ## state vector
  if(isPositionSensor){
    accelerationModel$H = matrix(data = c(
      1,0,0, 0,0,0, 0,0,0, 
      0,1,0, 0,0,0, 0,0,0, 
      0,0,1, 0,0,0, 0,0,0
    ), nrow = 3, ncol = 9, byrow=TRUE) ## observation/ measurment vector 
  }
  if(isAccelerationSensor){
    accelerationModel$H = matrix(data = c(
      0,0,0, 0,0,0, 1,0,0, 
      0,0,0, 0,0,0, 0,1,0, 
      0,0,0, 0,0,0, 0,0,1
    ), nrow = 3, ncol = 9, byrow=TRUE) ## observation/ measurment vector  
  }
  
  # Get white noise 
  #accelerationModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 9, ncol = 9), mean = 0, stddev = 2.56) ## white noise as process noise
  
  Q = matrix(data = c(
    3**2,0,0,
    0,3**2,0,
    0,0,1**2
  ), nrow = 3, ncol = 3, byrow=TRUE)
  
  accelerationModel$Gd = matrix(data =c(
    dt**3/6, 0, 0,
    dt**2/2, 0, 0,
    dt, 0, 0,
    0, dt**3/6, 0,
    0, dt**2/2, 0,
    0, dt, 0,
    0, 0, dt**3/6,
    0, 0, dt**2/2,
    0, 0, dt
  ), nrow=9, ncol=3, byrow = TRUE)
  
  accelerationModel$Q = accelerationModel$Gd %*% Q %*% t(accelerationModel$Gd)
  
  accelerationModel$P = initialBelief
  
  x_sigma = 0.05; xy_sigma = 0.0; xz_sigma = 0.0;
  yx_sigma = 0.0; y_sigma = 0.05; yz_sigma = 0.0;
  zx_sigma = 0.0; zy_sigma = 0.0; z_sigma = 0.05;
  
  accelerationModel$R = matrix(data = c(x_sigma**2, xy_sigma, xz_sigma,
                                        yx_sigma, y_sigma**2, yz_sigma,
                                        zx_sigma, zy_sigma, z_sigma**2
  ), nrow = 3, ncol = 3)
  
  accelerationModel$colNames = c("x","v_x","a_x","y","v_y","a_y","z","v_z","a_z")
  
  return(accelerationModel)
}

get9degAccelerationModelUnity <-function(){
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
  
  accelerationModel$a = matrix(data = c(0,0,0, 0,0,0, 3436,-2000,1870), nrow = 9, ncol = 1) ## state vector
  accelerationModel$H = matrix(data = c(
    0,0,0, 0,0,0, 0.05,0,0, 
    0,0,0, 0,0,0, 0,0.05,0, 
    0,0,0, 0,0,0, 0,0,10
  ), nrow = 3, ncol = 9, byrow=TRUE) ## observation/ measurment vector 
  
  # Get white noise 
  accelerationModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 9, ncol = 9), mean = 0, stddev = 2.56) ## white noise as process noise
  accelerationModel$P = diag(c(0,0,0,0,0,0,0.2,0.2,1))
  
  x_sigma = 0.01; xy_sigma = 0.0; xz_sigma = 0.0;
  y_sigma = 0.05; yx_sigma = 0.0; yz_sigma = 0.0;
  z_sigma = 0.01; zx_sigma = 0.0; zy_sigma = 0.0;
  
  accelerationModel$R = matrix(data = c(x_sigma**2, xy_sigma, xz_sigma,
                                        yx_sigma, y_sigma**2, yz_sigma,
                                        zx_sigma, zy_sigma, z_sigma**2
  ), nrow = 3, ncol = 3)
  
  accelerationModel$colNames = c("x","v_x","a_x","y","v_y","a_y","z","v_z","a_z")
  
  return(accelerationModel)
}

get9degFusion <-function() {
  fusionModel = list()
  
  fusionModel$F = matrix(data = c(
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
  
  fusionModel$a = matrix(data = c(0.00038, 0.0546,0.95, -0.0003,-0.043,	-3.45, 0.00018,0.026,	900), nrow = 9, ncol = 1) ## state vector
  fusionModel$H = matrix(data = c(
    0.2,0,0, 0,0,0, 0,0,0, 
    0,0.2,0, 0,0,0, 0,0,0, 
    0,0,1, 0,0,0, 0,0,0,
    0,0,0, 0,0,0, 0.05,0,0, 
    0,0,0, 0,0,0, 0,0.05,0, 
    0,0,0, 0,0,0, 0,0,10
  ), nrow = 6, ncol = 9, byrow=TRUE) ## observation/ measurment vector 
  
  # Get white noise 
  fusionModel$Q = add.Gaussian.noise(matrix(data = 1,nrow = 9, ncol = 9), mean = 0, stddev = 1) ## white noise as process noise
  fusionModel$P = diag(c(1,1,0.9,0,0,0,0.5,0.5,0.2))
  
  x_sigma = 0.001; y_sigma = 0.55; z_sigma = 0.55; 
  vx_sigma=0.05; vy_sigma=0.05;  vz_sigma = 0.05;
  ax_sigma=0.05; ay_sigma=0.5; az_sigma=0.05;
  
  fusionModel$R = diag(c(x_sigma**2, y_sigma**2, z_sigma**2, ax_sigma**2, ay_sigma**2, az_sigma**2)) 
  
  fusionModel$colNames = c("x","v_x","a_x","y","v_y","a_y","z","v_z","a_z")
  
  return(fusionModel)
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

processModelImu = get9degAccelerationModel(c(0,0,0, 0,0,0, 450,-290,18700), diag(c(0,0,0,0,0,0,0.5,0.5,0.5)), isAccelerationSensor = TRUE)
processModelKinect = get9degAccelerationModel(c(0,0,0, 0,0,0, 0,0,0), diag(c(0.5,0.5,0.5,0,0,0,0,0,0)), isPositionSensor = TRUE)
processModelImuUnity = get9degAccelerationModelUnity()
processModelFusion = get9degFusion()


kalmanFilterImu = kalman_init(
  processModelImu$a,
  processModelImu$P,
  processModelImu$Q,
  processModelImu$H,
  processModelImu$F,
  processModelImu$R
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

plot_up_limit = 2100
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
  x=TRUE,
  y=TRUE,
  z=TRUE
)
isPlottingKinectResults = data.frame(
  x=TRUE,
  y=TRUE,
  z=TRUE
)
isPlottingUnityResults = data.frame(
  x=TRUE,
  y=TRUE,
  z=TRUE
)
isPlottingFusionResults = data.frame(
  x=TRUE,
  y=TRUE,
  z=TRUE
)


if(isPlottingResults$x) {
  # x acc plot
  accxdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_x = plotDf$acc.x[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  accxdf$K_x = as.integer(resultImu$a_x[plot_low_limit:plot_up_limit])
  accxdf_long = melt(accxdf, id = "t")
  ggplot(accxdf_long, aes(x=t, y=value, color=variable)) + geom_line() 
}
if(isPlottingResults$y) {
  # y acc plot
  accydf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_y = plotDf$acc.y[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  accydf$K_y = as.integer(resultImu$a_y[plot_low_limit:plot_up_limit])
  accydf_long = melt(accydf, id = "t")
  ggplot(accydf_long, aes(x=t, y=value, color=variable)) + geom_line()
}
if(isPlottingResults$z) {
  # z acc plot
  acczdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      accelerometer_z = plotDf$acc.z[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit])
  acczdf$K_z = as.integer(resultImu$a_z[plot_low_limit:plot_up_limit])
  acczdf_long = melt(acczdf, id = "t")
  ggplot(acczdf_long, aes(x=t, y=value, color=variable)) + geom_line()  
}

if(isPlottingUnityResults$x) {
  # x acc Unity plot
  accxudf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                       accelerometer_x = (plotDf$acc.x[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * 0.0001))
  accxudf$K_x = as.double(resultImuUnity$x[plot_low_limit:plot_up_limit])
  accxudf_long = melt(accxudf, id = "t")
  ggplot(accxudf_long, aes(x=t, y=value, color=variable)) + geom_line() + ylim(-2, 2)
}
if(isPlottingUnityResults$y) {
  # y acc Unity plot
  accyudf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                       accelerometer_y = (plotDf$acc.y[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * 0.0001))
  accyudf$K_y = as.double(resultImuUnity$y[plot_low_limit:plot_up_limit])
  accyudf_long = melt(accyudf, id = "t")
  ggplot(accyudf_long, aes(x=t, y=value, color=variable)) + geom_line() + ylim(-2, 2)
}
if(isPlottingUnityResults$z) {
  # z acc Unity plot
  acczudf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                       accelerometer_z = (plotDf$acc.z[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * 0.00005))
  acczudf$K_z = as.double(resultImuUnity$z[plot_low_limit:plot_up_limit])
  acczudf_long = melt(acczudf, id = "t")
  ggplot(acczudf_long, aes(x=t, y=value, color=variable)) + geom_line() + ylim(-2, 2)
}

if(isPlottingKinectResults$x) {
  # x acc plot
  accxdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      kinect_x = plotDf$kin.x[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * 2)
  accxdf$K_x = as.double(resultKinect$x[plot_low_limit:plot_up_limit])
  accxdf_long = melt(accxdf, id = "t")
  ggplot(accxdf_long, aes(x=t, y=value, color=variable)) + geom_line() + ylim(-2, 2) 
}
if(isPlottingKinectResults$y) {
  # y acc plot
  accydf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      kinect_y = plotDf$kin.y[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * 2)
  accydf$K_y = as.double(resultKinect$y[plot_low_limit:plot_up_limit])
  accydf_long = melt(accydf, id = "t")
  ggplot(accydf_long, aes(x=t, y=value, color=variable)) + geom_line() + ylim(-2, 2)
}
if(isPlottingKinectResults$z) {
  # z acc plot
  acczdf = data.frame(t = plotDf$t[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit],
                      kinect_z = plotDf$kin.z[data_lower_lim:data_upper_lim][plot_low_limit:plot_up_limit] * 2)
  acczdf$K_z = as.double(resultKinect$z[plot_low_limit:plot_up_limit])
  acczdf_long = melt(acczdf, id = "t")
  ggplot(acczdf_long, aes(x=t, y=value, color=variable)) + geom_line() + ylim(-2, 2)
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