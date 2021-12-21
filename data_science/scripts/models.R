Ts = 0.014
process_noise_orientation_angular_velocity_sigma_x = 0.05
process_noise_orientation_angular_velocity_sigma_y = 0.05
process_noise_orientation_angular_velocity_sigma_z = 0.05

process_noise_acceleration_noise_omega_x = 0.05
process_noise_acceleration_noise_omega_y = 0.05
process_noise_acceleration_noise_omega_z = 0.05

sensor_noise_imu_euler_velocity_x =  0.5
sensor_noise_imu_euler_velocity_y =  0.5
sensor_noise_imu_euler_velocity_z =  0.5

sensor_noise_imu_sigma_x = 0.009810001
sensor_noise_imu_sigma_y = 0.009810001
sensor_noise_imu_sigma_z = 0.009810001

sensor_noise_kinect_sigma_x = 0.05
sensor_noise_kinect_sigma_y = 0.05
sensor_noise_kinect_sigma_z = 0.05


getGuanglongOrientationModel <- function(initialState, initialBelief) {
  #Currently used variable names -> variable names from Slides
  # F -> A: State Transition Matrix
  # a -> x: State Vector
  # H -> C: Observation Matrix
  # Q -> Q: Process Noise Matrix
  # R -> R: Sensor Noise Matrix
  # P -> P: Sensor Belief
  
  processModel = list()
  # Vielleicht sind die Quaternion states hier dynamisch zu sehen
  processModel$Ad = matrix(data = c(
    1,0,0,0,        -initialState[2] * Ts/2  ,-initialState[3]     ,-initialState[4] * Ts/2,
    0,1,0,0,        initialState[1] * Ts/2   ,initialState[4]      ,initialState[3] * Ts/2,
    0,0,1,0,        initialState[4] * Ts/2   ,initialState[1]*Ts/2 ,-initialState[2] * Ts/2,
    0,0,0,1,        -initialState[3] * Ts/2  ,initialState[2]*Ts/2 ,initialState * Ts/2,
    0,0,0,0,        1,0,0,
    0,0,0,0,        0,1,0,
    0,0,0,0,        0,0,1
  )
  ,nrow = 7, ncol = 7, byrow=TRUE)
  
  processModel$x = matrix(data = initialState, nrow = 7, ncol = 1) ## state vector
  
  processModel$H = matrix(data = c(
    0,0,0,0,1,0,0,
    0,0,0,0,0,1,0,
    0,0,0,0,0,0,1
  ), nrow = 3, ncol = 7, byrow=TRUE) ## observation/ measurment vector 
  
  omega = c(
    1,
    1,
    1,
    1,
    process_noise_orientation_angular_velocity_sigma_x,
    process_noise_orientation_angular_velocity_sigma_y,
    process_noise_orientation_angular_velocity_sigma_z
  )
  
  
  processModel$Q = diag(omega)
  processModel$R = matrix(data = c(
    sensor_noise_imu_euler_velocity_x, 
    sensor_noise_imu_euler_velocity_y,
    sensor_noise_imu_euler_velocity_z
  ),ncol= 1, nrow = 3)
  
  processModel$P = initialBelief
  
  processModel$colNames = c("q_0","q_1","q_2","q_3","v_x","v_y","v_z")
  
  return(processModel)
}

# TODO: Transformationsmatrix berechnen 
getGuanglongAccelerationModel <- function(initialState, initialBelief, matrixHandToLocal, gravityVector) {
  #Currently used variable names -> variable names from Slides
  # F -> A: State Transition Matrix
  # a -> x: State Vector
  # H -> C: Observation Matrix
  # Q -> Q: Process Noise Matrix
  # R -> R: Sensor Noise Matrix
  # P -> P: Sensor Belief
  processModel = list()
  Ts = 0.014
  processModel$Ad = matrix(data = c(
    1,Ts,matrixHandToLocal$mx_x * Ts**2/2,  0,0,matrixHandToLocal$my_x * Ts**2/2,  0,0,matrixHandToLocal$mz_x * Ts**2/2,
    0,1,matrixHandToLocal$mx_x * Ts,        0,0,matrixHandToLocal$my_x * Ts,       0,0,matrixHandToLocal$mz_x * Ts,
    0,0,1,                                  0,0,0,                                 0,0,0,
    0,0,matrixHandToLocal$mx_y * Ts**2/2,   1,Ts,matrixHandToLocal$my_y * Ts**2/2,  0,0,matrixHandToLocal$mz_y * Ts**2/2,
    0,0,matrixHandToLocal$mx_y * Ts,        0,1,matrixHandToLocal$my_y * Ts,       0,0,matrixHandToLocal$mz_y * Ts,
    0,0,0,                                  0,0,1,                                 0,0,0,
    0,0,matrixHandToLocal$mx_z * Ts**2/2,   0,0,matrixHandToLocal$my_z * Ts**2/2,  1,Ts,matrixHandToLocal$mz_z * Ts**2/2,
    0,0,matrixHandToLocal$mx_z * Ts,        0,0,matrixHandToLocal$my_z * Ts,       0,1,matrixHandToLocal$mz_z * Ts,
    0,0,0,                                  0,0,0,                                 0,0,1
  )
  ,nrow = 9, ncol = 9, byrow=TRUE)
  
  processModel$x = matrix(data = initialState, nrow = 9, ncol = 1) ## state vector
  processModel$B = matrix(data = c(0,0,0,0,0,0, -vectorLength(gravityVector) * Ts**2/2, -vectorLength(gravityVector) * Ts, 0), nrow = 9, ncol = 1)
  
  processModel$H = matrix(data = c(
    1,0,0,0,0,0,0,0,0,
    0,0,1,0,0,0,0,0,0,
    0,0,0,1,0,0,0,0,0,
    0,0,0,0,0,1,0,0,0,
    0,0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,0,0,1
  ), nrow = 6, ncol = 9, byrow=TRUE) ## observation/ measurment vector 
  
  processModel$Gd = matrix(data = c(
    Ts**2/2,0,0,
    Ts,     0,0,
    1,      0,0,
    0,Ts**2/2,0,
    0,Ts,     0,
    0,1,      0,
    0,0,Ts**2/2,
    0,0,Ts     ,
    0,0,1      
  ), nrow=9, ncol=3, byrow = TRUE)
  
  omega = diag(x=c(
    process_noise_acceleration_noise_omega_x,
    process_noise_acceleration_noise_omega_y,
    process_noise_acceleration_noise_omega_z)
  )
  
  processModel$Q = processModel$Gd %*% omega %*% t(processModel$Gd)
  
  processModel$P = initialBelief
  processModel$R = matrix(nrow = 6, ncol = 1, data = c(
    sensor_noise_kinect_sigma_x,
    sensor_noise_kinect_sigma_y,
    sensor_noise_kinect_sigma_z,
    sensor_noise_imu_sigma_x,
    sensor_noise_imu_sigma_y,
    sensor_noise_imu_sigma_z
  ))
  
  processModel$colNames = c("x","v_x","a_x","y","v_y","a_y","z","v_z","a_z")
  
  return(processModel)
}