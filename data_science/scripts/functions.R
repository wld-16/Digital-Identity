Ts = 0.014

simulated_orientation_transition_at_index <- function(i){
  return (
    matrix(
      data = c(
        1,0,0,0,        -simulated_normalized_quaternions[i,2] * Ts/2  ,-simulated_normalized_quaternions[i,3]*Ts/2     ,-simulated_normalized_quaternions[i,4] * Ts/2,
        0,1,0,0,        simulated_normalized_quaternions[i,1] * Ts/2   ,simulated_normalized_quaternions[i,4]*Ts/2      ,simulated_normalized_quaternions[i,3] * Ts/2,
        0,0,1,0,        simulated_normalized_quaternions[i,4] * Ts/2   ,simulated_normalized_quaternions[i,1]*Ts/2      ,-simulated_normalized_quaternions[i,2] * Ts/2,
        0,0,0,1,        -simulated_normalized_quaternions[i,3] * Ts/2  ,simulated_normalized_quaternions[i,2]*Ts/2      ,simulated_normalized_quaternions[i,1] * Ts/2,
        0,0,0,0,        1,0,0,
        0,0,0,0,        0,1,0,
        0,0,0,0,        0,0,1
      ), nrow = 7, ncol = 7, byrow = TRUE
    )
  )
}

orientation_transition_at_index <- function(i){
  return(processOrientationModel$Ad + matrix(
    data = c(
      0,0,0,0,-imkDf$kinect_hand_right_orientation.y[i] * Ts/2,
      -imkDf$kinect_hand_right_orientation.z[i],-imkDf$kinect_hand_right_orientation.w[i] * Ts/2,
      0,0,0,0,imkDf$kinect_hand_right_orientation.x[i] * Ts/2,
      imkDf$kinect_hand_right_orientation.w[i],
      imkDf$kinect_hand_right_orientation.z[i] * Ts/2,
      0,0,0,0,imkDf$kinect_hand_right_orientation.w[i] * Ts/2,
      imkDf$kinect_hand_right_orientation.x[i]*Ts/2 ,
      -imkDf$kinect_hand_right_orientation.y[i] * Ts/2,
      0,0,0,0,-imkDf$kinect_hand_right_orientation.z[i] * Ts/2,
      imkDf$kinect_hand_right_orientation.y[i]*Ts/2 ,
      imkDf$kinect_hand_right_orientation.x[i] * Ts/2,
      0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,
      0,0,0,0,0,0,0
    ), nrow=7, ncol=7, byrow = TRUE))
}

quaternion_rotation_matrix <- function(q0, q1, q2, q3){
  
  # First row of the rotation matrix
  r00 = 2 * (q0 * q0 + q1 * q1) - 1
  r01 = 2 * (q1 * q2 - q0 * q3)
  r02 = 2 * (q1 * q3 + q0 * q2)
  
  # Second row of the rotation matrix
  r10 = 2 * (q1 * q2 + q0 * q3)
  r11 = 2 * (q0 * q0 + q2 * q2) - 1
  r12 = 2 * (q2 * q3 - q0 * q1)
  
  # Third row of the rotation matrix
  r20 = 2 * (q1 * q3 - q0 * q2)
  r21 = 2 * (q2 * q3 + q0 * q1)
  r22 = 2 * (q0 * q0 + q3 * q3) - 1
  
  # 3x3 rotation matrix
  rot_matrix = matrix(data = c(r00, r01, r02, r10, r11, r12, r20, r21, r22), nrow = 3, ncol = 3)
  
  return(rot_matrix)
}

rotated_X_component <- function(i){
  rotation_matrix = quaternion_rotation_matrix(
    imkDf$kinect_hand_right_orientation.w[i],
    imkDf$kinect_hand_right_orientation.x[i],
    imkDf$kinect_hand_right_orientation.y[i],
    imkDf$kinect_hand_right_orientation.z[i]
  )
  
  return (rotation_matrix[1,1] * imkDf$imu_acceleration.x[i] + rotation_matrix[1,2] * imkDf$imu_acceleration.y[i] + rotation_matrix[1,3] * imkDf$imu_acceleration.z[i])
}

rotated_y_component <- function(i){
  rotation_matrix = quaternion_rotation_matrix(
    imkDf$kinect_hand_right_orientation.w[i],
    imkDf$kinect_hand_right_orientation.x[i],
    imkDf$kinect_hand_right_orientation.y[i],
    imkDf$kinect_hand_right_orientation.z[i]
  )
  
  return (rotation_matrix[2,1] * imkDf$imu_acceleration.x[i] + rotation_matrix[2,2] * imkDf$imu_acceleration.y[i] + rotation_matrix[2,3] * imkDf$imu_acceleration.z[i])
}

rotated_z_component <- function(i){
  rotation_matrix = quaternion_rotation_matrix(
    imkDf$kinect_hand_right_orientation.w[i],
    imkDf$kinect_hand_right_orientation.x[i],
    imkDf$kinect_hand_right_orientation.y[i],
    imkDf$kinect_hand_right_orientation.z[i]
  )
  
  return (rotation_matrix[3,1] * imkDf$imu_acceleration.x[i] + rotation_matrix[3,2] * imkDf$imu_acceleration.y[i] + rotation_matrix[3,3] * imkDf$imu_acceleration.z[i])
}

acceleration_simulation_transition_at_index <- function(i){
  
  rotation_matrix = quaternion_rotation_matrix(
    simulation_rotation_quaternion$w[i],
    simulation_rotation_quaternion$x[i],
    simulation_rotation_quaternion$y[i],
    simulation_rotation_quaternion$z[i])
  
  return(matrix(data = c(
    1,Ts, rotation_matrix[1,1] * Ts**2/2,  0,0,rotation_matrix[2,1] * Ts**2/2,  0,0,rotation_matrix[3,1] * Ts**2/2,
    0,1,rotation_matrix[1,1] * Ts,         0,0,rotation_matrix[2,1] * Ts,       0,0,rotation_matrix[3,1] * Ts,
    0,0,1,                                 0,0,0,                               0,0,0,
    0,0,rotation_matrix[1,2] * Ts**2/2,    1,Ts,rotation_matrix[2,2] * Ts**2/2, 0,0,rotation_matrix[3,2] *Ts**2/2,
    0,0,rotation_matrix[1,2] * Ts,         0,1,rotation_matrix[2,2] * Ts,       0,0,rotation_matrix[3,2] *Ts,
    0,0,0,                                 0,0,1,                               0,0,0,
    0,0,rotation_matrix[1,3] * Ts**2/2,    0,0,rotation_matrix[2,3] * Ts**2/2, 1,Ts,rotation_matrix[3,3] *Ts**2/2,
    0,0,rotation_matrix[1,3] *Ts,          0,0,rotation_matrix[2,3] * Ts,       0,1,rotation_matrix[3,3] *Ts,
    0,0,0,                                 0,0,0,                               0,0,1
  )
  ,nrow = 9, ncol = 9, byrow=TRUE))
}

acceleration_transition_at_index <- function(i){
  
  rotation_matrix = quaternion_rotation_matrix(
    imkDf$kinect_hand_right_orientation.w[i],
    imkDf$kinect_hand_right_orientation.x[i],
    imkDf$kinect_hand_right_orientation.y[i],
    imkDf$kinect_hand_right_orientation.z[i])
  
  return(matrix(data = c(
    1,Ts, rotation_matrix[1,1] * Ts**2/2,  0,0,rotation_matrix[2,1] * Ts**2/2,  0,0,rotation_matrix[3,1] * Ts**2/2,
    0,1,rotation_matrix[1,1] * Ts,         0,0,rotation_matrix[2,1] * Ts,       0,0,rotation_matrix[3,1] * Ts,
    0,0,1,                                 0,0,0,                               0,0,0,
    0,0,rotation_matrix[1,2] * Ts**2/2,    1,Ts,rotation_matrix[2,2] * Ts**2/2, 0,0,rotation_matrix[3,2] *Ts**2/2,
    0,0,rotation_matrix[1,2] * Ts,         0,1,rotation_matrix[2,2] * Ts,       0,0,rotation_matrix[3,2] *Ts,
    0,0,0,                                 0,0,1,                               0,0,0,
    0,0,rotation_matrix[1,3] * Ts**2/2,    0,0,rotation_matrix[2,3] * Ts**2/2, 1,Ts,rotation_matrix[3,3] *Ts**2/2,
    0,0,rotation_matrix[1,3] *Ts,          0,0,rotation_matrix[2,3] * Ts,       0,1,rotation_matrix[3,3] *Ts,
    0,0,0,                                 0,0,0,                               0,0,1
  )
  ,nrow = 9, ncol = 9, byrow=TRUE))
}

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

crossProduct <- function(a1,a2,a3,b1,b2,b3){
  return(
    data.frame(
      x=a2*b3-a3*b2, 
      y=a3*b1-a1*b3,
      z=a1*b2-a2*b1
    )
  )
}

scalarProduct <- function(a1,a2,a3,b1,b2,b3){
  return(a1*b1+a2*b2+a3*b3)
}

vectorLength <- function(vector3){
  return(sqrt(vector3$x**2 + vector3$y**2 + vector3$z**2))
}

justReturn <- function(index,frame){
  return(frame)
}

eulerToQuaternion <- function(theta, ux, uy, uz) {
  theta = theta / 180 * pi
  return(data.frame(
    w = cos(theta/2),
    x = ux * sin(theta/2),
    y = uy * sin(theta/2),
    z = uz * sin(theta/2)
  ))
}

rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

quaternion_multiplication <- function(q1, q2){
  x =  q1$x * q2$w + q1$y * q2$z - q1$z * q2$y + q1$w * q2$x;
  y = -q1$x * q2$z + q1$y * q2$w + q1$z * q2$x + q1$w * q2$y;
  z =  q1$x * q2$y - q1$y * q2$x + q1$z * q2$w + q1$w * q2$z;
  w = -q1$x * q2$x - q1$y * q2$y - q1$z * q2$z + q1$w * q2$w;
  
  M = sqrt(w**2 + x**2 + y**2 + z**2)
  
  return(data.frame(
    w = w / M,
    x = x / M,
    y = y / M,
    z = z / M
  ))
  
}
