closest_values <- function(t_value, bigger_time_pool){
  values = c()
  i = 1
  for (t in bigger_time_pool) {
    values = c(bigger_time_pool[i] - t_value, bigger_time_pool[i+1] - t_value)
    
    if(values[1] < 0 && values[2] >= 0) {
      print(c(i, i+1))
      break
    }
    else {
      i = i + 1
    }
  }
}

sampleFreq <- 10
q_origin <- list(1.0, 0.0, 0.0, 0.0)
beta <- 0.1

MadgwickAHRSupdate <- function(gx, gy, gz, ax, ay, az, q){
  qDot = 0.5 * c(-q[2] * gx- q[3] * gy- q[4] * gz,
                 q[1] * gx+ q[3] * gz- q[4] * gy,
                 q[1] * gy- q[3] * gz+ q[4] * gx,
                 q[1] * gz+ q[2] * gy- q[3] * gx
  )
  
  if(!((ax == 0.0) && (ay == 0.0) && (az == 0.0))){
    recipNorm = sqrt(ax * ax + ay * ay + az * az)
    ax = ax * recipNorm
    ay = ay * recipNorm
    az = az * recipNorm
    
    two_q = 2.0 * q
    four_q = 4.0 * q
    ate_q = 8 * q
    q_squared = q * q
    
    # Gradient decent
    s0 = four_q[1] * q_squared[3] + two_q[3] * ax + four_q[1] * q_squared[2] - two_q[2] * ay
    s1 = four_q[2] * q_squared[4] - two_q[4] * ax + 4.0 * q_squared[1] * q[2] - two_q[1] * ay - four_q[2] + ate_q[2] * q[2] + ate_q[2] * q[3] + four_q[2] * az
    s2 = 4.0 * q_squared[1] * q[3] + two_q[1] * ax + four_q[3] * q_squared[4] - two_q[4] * ay - four_q[3] + ate_q[3] * q[2] + ate_q[3] * q[3] + four_q[3] * az
    s3 = 4.0 * q_squared[2] * q[4] - two_q[2] * ax + 4.0 * q_squared[3] * q[4] - two_q[3] * ay
    
    recipNorm = sqrt(s0 * s0 + s1 * s1 + s2 * s2 + s3 * s3)
    s0 = s0 * recipNorm
    s1 = s1 * recipNorm
    s2 = s2 * recipNorm
    s3 = s3 * recipNorm
    
    qDot = qDot - (beta * c(s0, s1, s2, s3))
    
  }
  
  q = q + (qDot * (1 / sampleFreq))
  recipNorm = sqrt(q*q)
  
  q = q * recipNorm
  return(q)
}

kalman_frame <- function(t1, t1_1, ax, ay, az, gx, gy, gz, phi_hat, theta_hat){
  
  
  # Sample Time
  dt = t1 - t1_1
  
  # Get Accelerometer measurements
  
  acc_angles = get_acc_angles(ax, ay, az)
  acc_angles[1] = acc_angles[1] - phi_offset
  acc_angles[2] = acc_angles[2] - theta_offset
  
  
  # Get Gyro measurements
  #gx = gx * pi / 180
  #gy = gy * pi / 180
  #gz = gz * pi / 180
  
  phi_dot = gx + sin(phi_hat) * tan(theta_hat) * gy + cos(phi_hat) * tan(theta_hat) * gz
  theta_dot = cos(phi_hat) * gy - sin(phi_hat) * gz
  
  # Kalman filter
  A = matrix(data = c(1,-dt,0,0, 0,1,0,0, 0,0,1,-dt, 0,0,0,1), nrow = 4, ncol = 4)
  B = matrix(data = c(dt,0, 0,0, 0,dt, 0,0), nrow = 4, ncol = 2)
  
  gyro_input = c(phi_dot, theta_dot)
  
  
  state_estimate = A %*% state_estimate + B %*% gyro_input
  P = (A %*% (P %*% t(A))) + Q
  
  # Predict
  measurement = acc_angles
  
  y_tilde = measurement - C %*% state_estimate
  S = R + (C %*% (P %*% t(C)))
  K = P %*% (t(C) %*% solve(S))
  
  # Update
  state_estimate = state_estimate + K %*% y_tilde
  P = (diag(4 - K %*% C)) %*% P
  
  phi_hat = state_estimate[1]
  theta_hat = state_estimate[3]
  
  return(c(state_estimate, t1))
}

gyro_integrate <- function(angles, gyr){
  gyro_phi = gyr[1] + sin(angles[[1]]) * tan(angles[[2]]) * gyr[2] + cos(angles[[1]]) * tan(angles[[2]]) * gyr[3]
  gyro_theta = cos(angles[[1]]) * gyr[2] - sin(angles[[1]]) * gyr[3]
  return(list(gyro_phi, gyro_theta))
}

test = data.frame(t(data.frame(gyro$x, gyro$y, gyro$z)))
init = list(0,0)
result = Reduce('gyro_integrate',test,init = init,accumulate = TRUE)

acc_t = t(c(acc))
names(acc_t) = c("time", "X", "Y", "Z")

gyro_t = t(c(gyro))
names(gyro_t) = c("time", "X", "Y", "Z")

orientation = vector(mode = "list", length(gyro_t$time))

i = 1
orientation[[i]] = q_origin

while(i < length(gyro_t$time) && i < length(acc_t$time)){
  i = i + 1
  result <- MadgwickAHRSupdate(gyro_t$X[i], gyro_t$Y[i], gyro_t$Z[i], acc_t$X[i], acc_t$Y[i], acc_t$Z[i], orientation[[i-1]])
  orientation[[i]] = result
}