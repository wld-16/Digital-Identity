getGuanglongOrientationModel <- function(Ts, initialState, initialBelief, processNoise, sensorNoise) {
  #Currently used variable names -> variable names from Slides
  # F -> A: State Transition Matrix
  # a -> x: State Vector
  # H -> C: Observation Matrix
  # Q -> Q: Process Noise Matrix
  # R -> R: Sensor Noise Matrix
  # P -> P: Sensor Belief
  
  processModel = list()
  processModel$Ts = Ts
  # Vielleicht sind die Quaternion states hier dynamisch zu sehen
  # Arrays in R starten mit 1
  processModel$Ad = matrix(data = c(
    1,0,0,0,        -initialState[2] * Ts/2  ,-initialState[3]*Ts/2     ,-initialState[4] * Ts/2,
    0,1,0,0,        initialState[1] * Ts/2   ,-initialState[4]*Ts/2      ,initialState[3] * Ts/2,
    0,0,1,0,        initialState[4] * Ts/2   ,initialState[1]*Ts/2      ,-initialState[2] * Ts/2,
    0,0,0,1,        -initialState[3] * Ts/2  ,initialState[2]*Ts/2      ,initialState[1] * Ts/2,
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
  
  omega = diag(c(
    1,
    1,
    1,
    1,
    processNoise$vx,
    processNoise$vy,
    processNoise$vz
  ))
  
  
  processModel$Gd = matrix( data = c(
    -initialState[2] * Ts/2,-initialState[3]*Ts/2,-initialState[4] * Ts/2,
    initialState[1] * Ts/2,-initialState[4]*Ts/2,initialState[3] * Ts/2,
    initialState[4] * Ts/2,initialState[1]*Ts/2 ,-initialState[2] * Ts/2,
    -initialState[3] * Ts/2,initialState[2]*Ts/2 ,initialState[1] * Ts/2,
    1,0,0,
    0,1,0,
    0,0,1
  ), nrow=7, ncol=3, byrow = TRUE)
  
  omega = diag(x=c(
    processNoise$vx,
    processNoise$vy,
    processNoise$vz)
  )
  
  processModel$Q = processModel$Gd %*% omega %*% t(processModel$Gd)
  
  processModel$P = initialBelief
  
  processModel$R = matrix(data = c(
    sensorNoise$vx, 
    sensorNoise$vy,
    sensorNoise$vz
  ),ncol= 1, nrow = 3)
  
  processModel$P = initialBelief
  
  processModel$colNames = c("q_0","q_1","q_2","q_3","v_x","v_y","v_z")
  
  return(processModel)
}

# TODO: Transformationsmatrix berechnen 
getGuanglongAccelerationModel <- function(Ts, initialState, initialBelief, matrixHandToLocal, gravityVector, processNoise, sensorNoise) {
  #Currently used variable names -> variable names from Slides
  # F -> A: State Transition Matrix
  # a -> x: State Vector
  # H -> C: Observation Matrix
  # Q -> Q: Process Noise Matrix
  # R -> R: Sensor Noise Matrix
  # P -> P: Sensor Belief
  processModel = list()
  processModel$Ts = Ts
  processModel$Ad = matrix(data = c(
    1,Ts,matrixHandToLocal$mx_x * Ts**2/2,  0,0,matrixHandToLocal$my_x * Ts**2/2,  0,0,matrixHandToLocal$mz_x * Ts**2/2,
    0,1, matrixHandToLocal$mx_x * Ts,       0,0,matrixHandToLocal$my_x * Ts,       0,0,matrixHandToLocal$mz_x * Ts,
    0,0,1,                                  0,0,0,                                 0,0,0,
    0,0,matrixHandToLocal$mx_y * Ts**2/2,   1,Ts,matrixHandToLocal$my_y * Ts**2/2, 0,0,matrixHandToLocal$mz_y * Ts**2/2,
    0,0,matrixHandToLocal$mx_y * Ts,        0,1,matrixHandToLocal$my_y * Ts,       0,0,matrixHandToLocal$mz_y * Ts,
    0,0,0,                                  0,0,1,                                 0,0,0,
    0,0,matrixHandToLocal$mx_z * Ts**2/2,   0,0,matrixHandToLocal$my_z * Ts**2/2,  1,Ts,matrixHandToLocal$mz_z * Ts**2/2,
    0,0,matrixHandToLocal$mx_z * Ts,        0,0,matrixHandToLocal$my_z * Ts,       0,1,matrixHandToLocal$mz_z * Ts,
    0,0,0,                                  0,0,0,                                 0,0,1
  )
  ,nrow = 9, ncol = 9, byrow=TRUE)
  
  
  
  processModel$x = matrix(data = as.double(initialState), nrow = 9, ncol = 1) ## state vector
  processModel$B = matrix(data = c(0,0,0,0,0,0, -vectorLength(gravityVector) * Ts**2/2, -vectorLength(gravityVector) * Ts, 0), nrow = 9, ncol = 1)
  
  processModel$H = matrix(data = c(
    1,0,0,0,0,0,0,0,0,
    0,0,1,0,0,0,0,0,0,
    0,0,0,1,0,0,0,0,0,
    0,0,0,0,0,1,0,0,0,
    0,0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,0,0,1
  ), nrow = 6, ncol = 9, byrow=TRUE) ## observation/ measurment vector 
  
  ## Gd Matrix with rotation
  #processModel$Gd = matrix(data = c(
  #  matrixHandToLocal$mx_x * Ts**2/2,matrixHandToLocal$mx_y * Ts**2/2,matrixHandToLocal$mx_z * Ts**2/2,
  #  matrixHandToLocal$mx_x * Ts,     matrixHandToLocal$mx_y * Ts,matrixHandToLocal$mx_z * Ts,
  #  1,      0,0,
  #  matrixHandToLocal$my_x * Ts**2/2,matrixHandToLocal$my_y * Ts**2/2,matrixHandToLocal$my_z * Ts**2/2,
  #  matrixHandToLocal$my_x * Ts,matrixHandToLocal$my_y * Ts,     matrixHandToLocal$my_z * Ts,
  #  0,1,      0,
  #  matrixHandToLocal$mz_x * Ts**2/2,matrixHandToLocal$mz_y * Ts**2/2 ,matrixHandToLocal$mz_z * Ts**2/2,
  #  matrixHandToLocal$mz_x * Ts,matrixHandToLocal$mz_y * Ts ,matrixHandToLocal$mz_z * Ts     ,
  #  0,0,1      
  #), nrow=9, ncol=3, byrow = TRUE)
  
  ## Gd Matrix without rotation
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
    processNoise$x,
    processNoise$y,
    processNoise$z)
  )
  
  processModel$Q = processModel$Gd %*% omega %*% t(processModel$Gd)
  
  processModel$P = initialBelief
  processModel$R = matrix(nrow = 6, ncol = 1, data = c(
    sensorNoise$x,
    sensorNoise$y,
    sensorNoise$z,
    sensorNoise$ax,
    sensorNoise$ay,
    sensorNoise$az
  ))
  
  processModel$colNames = c("x","v_x","a_x","y","v_y","a_y","z","v_z","a_z")
  
  return(processModel)
}
