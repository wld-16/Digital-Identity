orientation_simulation_environment <- new.env()

setOrientationSimulationProcessNoise <- function(processNoise){
  assign("sim_process_noise", processNoise, env=orientation_simulation_environment)
}

getOrientationSimulationProcessNoise <- function(processNoise){
  return(get("sim_process_noise", orientation_simulation_environment))
}

setOrientationSimulationSensorNoise <- function(sensorNoise){
  assign("sim_sensor_noise", sensorNoise, env=orientation_simulation_environment)
}

getOrientationSimulationSensorNoise <- function() {
  return(get("sim_sensor_noise", orientation_simulation_environment))
}

setOrientationSimulationModell <- function(model){
  assign("sim_model", model, env=orientation_simulation_environment)
}

getOrientationSimulationModell <- function() {
  return(get("sim_model", orientation_simulation_environment))
}

generateNextOrientationFrame <- function(x_k, x_ki){
  sim_model = getOrientationSimulationModell()
  sim_orientation_process_noise = getOrientationSimulationProcessNoise()
  
  A_k = sim_model$Ad
  B = diag(x = 0,nrow=7)
  uk = matrix(nrow=7, ncol=1, data = 0)
  wk = matrix(nrow=7, ncol=1, data = c(
    0,
    0,
    0,
    0,
    rnorm(1,0,sim_orientation_process_noise$vx),
    rnorm(1,0,sim_orientation_process_noise$vy),
    rnorm(1,0,sim_orientation_process_noise$vz)
    ))
  
  x_ki = A_k %*% x_k + B %*% uk + wk
  
  q1 = x_ki[1]
  q2 = x_ki[2]
  q3 = x_ki[3]
  q4 = x_ki[4]
  
  M = sqrt(q1**2 + q2**2 + q3**2 + q4**2)
  x_ki[1] = x_ki[1]/M
  x_ki[2] = x_ki[2]/M
  x_ki[3] = x_ki[3]/M
  x_ki[4] = x_ki[4]/M
  
  return(x_ki)
}

generateNextOrientationMeasurement <- function(x_k, x_ki){
  sim_model = getOrientationSimulationModell()
  sim_sensor_noise = getOrientationSimulationSensorNoise()
  H = sim_model$H
  v_k = matrix(nrow=3, ncol=1, data=c(
    rnorm(1,0,sim_sensor_noise$vx),
    rnorm(1,0,sim_sensor_noise$vy),
    rnorm(1,0,sim_sensor_noise$vz)
  ))
  
  z_k = H %*% x_k + v_k
  return(z_k)
}

acceleration_simulation_environment <- new.env()

setAccelerationSimulationProcessNoise <- function(processNoise){
  assign("sim_process_noise", processNoise, env=acceleration_simulation_environment)
}

getAccelerationSimulationProcessNoise <- function(processNoise){
  return(get("sim_process_noise", acceleration_simulation_environment))
}

setAccelerationSimulationSensorNoise <- function(sensorNoise){
  assign("sim_sensor_noise", sensorNoise, env=acceleration_simulation_environment)
}

getAccelerationSimulationSensorNoise <- function() {
  return(get("sim_sensor_noise", acceleration_simulation_environment))
}

setAccelerationSimulationModell <- function(model){
  assign("sim_model", model, env=acceleration_simulation_environment)
}

getAccelerationSimulationModell <- function() {
  return(get("sim_model", acceleration_simulation_environment))
}

# Simulation of Acceleration

generateNextAccelerationFrameWithGravity <- function(x_k, x_ki){
  sim_model = getAccelerationSimulationModell()
  sim_process_noise = getAccelerationSimulationProcessNoise()
  
  Ts = 0.014
  A_k = sim_model$Ad
  B = diag(x = 1, nrow=9)
  uk = matrix(nrow=9, ncol=1, data = c(0,0,0,-9.81 * Ts**2/2,-9.81 * Ts,1,0,0,0))
  wk = matrix(nrow=9, ncol=1, data = c(
    0,
    0,
    rnorm(1,0,sim_process_noise$x),
    0,
    0,
    rnorm(1,0,sim_process_noise$y),
    0,
    0,
    rnorm(1,0,sim_process_noise$z)))
  
  x_ki = A_k %*% x_k + B %*% uk + wk 
  return(x_ki)
}

generateNextAccelerationFrame <- function(x_k, x_ki){
  sim_model = getAccelerationSimulationModell()
  sim_process_noise = getAccelerationSimulationProcessNoise()
  
  A_k = sim_model$Ad
  B = diag(x = 0, nrow=9)
  uk = matrix(nrow=9, ncol=1, data = 0)
  wk = matrix(nrow=9, ncol=1, data = c(
    0,
    0,
    rnorm(1,0,sim_process_noise$x),
    0,
    0,
    rnorm(1,0,sim_process_noise$y),
    0,
    0,
    rnorm(1,0,sim_process_noise$z)))
  
  x_ki = A_k %*% x_k + B %*% uk + wk 
  return(x_ki)
}

generateNextAccelerationMeasurement <- function(x_k){
  sim_model = getAccelerationSimulationModell()
  sim_sensor_noise = getAccelerationSimulationSensorNoise()
  
  H = sim_model$H
  v_k = matrix(nrow=6, ncol=1, data= c(
   rnorm(1,0,sim_sensor_noise$x),
   rnorm(1,0,sim_sensor_noise$y),
   rnorm(1,0,sim_sensor_noise$z),
   rnorm(1,0,sim_sensor_noise$ax),
   rnorm(1,0,sim_sensor_noise$ay),
   rnorm(1,0,sim_sensor_noise$az)
  ))
  
  z_k = H %*% x_k + v_k
  return(z_k)
}
