
# Uncomment to install packages
# install.packages("testthat")
# install.packages("usethis")

# For quaternion rotations see reference here
# http://www.euclideanspace.com/maths/geometry/rotations/conversions/eulerToQuaternion/steps/index.htm

library(testthat)
library(usethis)

source("D:\\Projekte\\Digital-Identity\\data_science\\scripts\\functions.R")
source("D:\\Projekte\\Digital-Identity\\data_science\\scripts\\models.R")
source("D:\\Projekte\\Digital-Identity\\data_science\\scripts\\initial_data.R")

test_that("model remains 0 when input 0", {
  initialIdleState = c(0,0,0,0,0,0,0,0,0)
  initialIdleBelief = diag(9)
  initial_gravity = data.frame(
    x = 0,
    y = 0,
    z = 9.81
  )
  
  rotationMatrix = quaternion_rotation_matrix(1,0,0,0)
  
  initial_matrixHandToLocal = data.frame(
    mx_x = rotationMatrix[1,1], my_x = rotationMatrix[1,2], mz_x = rotationMatrix[1,3],
    mx_y = rotationMatrix[2,1], my_y = rotationMatrix[2,2], mz_y = rotationMatrix[2,3],
    mx_z = rotationMatrix[3,1], my_z = rotationMatrix[3,2], mz_z = rotationMatrix[3,3]
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
    ) 
  
  result = model$Ad %*% initialIdleState
  
  expect_equal(result, matrix(data = c(0,0,0,0,0,0,0,0,0), nrow = 9, ncol = 1))
})

test_that("model a_x=1 without rotation", {
  initialIdleState = c(0,0,1,0,0,0,0,0,0)
  initialIdleBelief = diag(9)
  initial_gravity = data.frame(
    x = 0,
    y = 0,
    z = 9.81
  )
  rotationMatrix = quaternion_rotation_matrix(1,0,0,0)
  
  initial_matrixHandToLocal = data.frame(
    mx_x = rotationMatrix[1,1], my_x = rotationMatrix[1,2], mz_x = rotationMatrix[1,3],
    mx_y = rotationMatrix[2,1], my_y = rotationMatrix[2,2], mz_y = rotationMatrix[2,3],
    mx_z = rotationMatrix[3,1], my_z = rotationMatrix[3,2], mz_z = rotationMatrix[3,3]
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  ) 
  
  result = model$Ad %*% initialIdleState
  
  expect_equal(result[,1], c(model$Ts**2/2,model$Ts,1,0,0,0,0,0,0))
})

test_that("model a_y=1 without rotation", {
  initialIdleState = c(0,0,1,0,0,0,0,0,0)
  initialIdleBelief = diag(9)
  initial_gravity = data.frame(
    x = 0,
    y = 0,
    z = 9.81
  )
  rotationMatrix = quaternion_rotation_matrix(1,0,0,0)
  
  initial_matrixHandToLocal = data.frame(
    mx_x = rotationMatrix[1,1], my_x = rotationMatrix[1,2], mz_x = rotationMatrix[1,3],
    mx_y = rotationMatrix[2,1], my_y = rotationMatrix[2,2], mz_y = rotationMatrix[2,3],
    mx_z = rotationMatrix[3,1], my_z = rotationMatrix[3,2], mz_z = rotationMatrix[3,3]
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  ) 
  
  result = model$Ad %*% initialIdleState
  
  expect_equal(result[,1], c(model$Ts**2/2,model$Ts,1,0,0,0,0,0,0))
})

test_that("model a_z=1 without rotation", {
  initialIdleState = c(0,0,0,0,0,0,0,0,1)
  initialIdleBelief = diag(9)
  initial_gravity = data.frame(
    x = 0,
    y = 0,
    z = 9.81
  )
  rotationMatrix = quaternion_rotation_matrix(1,0,0,0)
  
  initial_matrixHandToLocal = data.frame(
    mx_x = rotationMatrix[1,1], my_x = rotationMatrix[1,2], mz_x = rotationMatrix[1,3],
    mx_y = rotationMatrix[2,1], my_y = rotationMatrix[2,2], mz_y = rotationMatrix[2,3],
    mx_z = rotationMatrix[3,1], my_z = rotationMatrix[3,2], mz_z = rotationMatrix[3,3]
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  ) 
  
  result = model$Ad %*% initialIdleState
  
  expect_equal(result[,1], c(0,0,0,0,0,0,model$Ts**2/2,model$Ts,1))
})

test_that("model test a_x = 1 with x rotation 90 deg", {
  initialIdleState = c(0,0,1,0,0,0,0,0,0)
  initialIdleBelief = diag(9)
  initial_gravity = data.frame(
    x = 0,
    y = 0,
    z = 9.81
  )
  
  phi = pi/2
  rotationMatrix = quaternion_rotation_matrix(
    sin(phi/2),
    0,
    0,
    sin(phi/2)
    )
  
  initial_matrixHandToLocal = data.frame(
    mx_x = rotationMatrix[1,1], my_x = rotationMatrix[1,2], mz_x = rotationMatrix[1,3],
    mx_y = rotationMatrix[2,1], my_y = rotationMatrix[2,2], mz_y = rotationMatrix[2,3],
    mx_z = rotationMatrix[3,1], my_z = rotationMatrix[3,2], mz_z = rotationMatrix[3,3]
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  ) 
  
  result = model$Ad %*% matrix(data = initialIdleState, nrow = 9, ncol = 1)
  
  expect_equal(result[,1], c(0,0,1,-model$Ts**2/2,-model$Ts,0,0,0,0))
})

test_that("model test a_x = 1 with y rotation 180 deg", {
  initialIdleState = c(0,0,1,0,0,0,0,0,0)
  initialIdleBelief = diag(9)
  initial_gravity = data.frame(
    x = 0,
    y = 0,
    z = 9.81
  )
  
  phi = pi/2
  rotationMatrix = quaternion_rotation_matrix(
    0,
    0,
    sin(phi/2),
    -sin(phi/2)
  )
  
  initial_matrixHandToLocal = data.frame(
    mx_x = rotationMatrix[1,1], my_x = rotationMatrix[1,2], mz_x = rotationMatrix[1,3],
    mx_y = rotationMatrix[2,1], my_y = rotationMatrix[2,2], mz_y = rotationMatrix[2,3],
    mx_z = rotationMatrix[3,1], my_z = rotationMatrix[3,2], mz_z = rotationMatrix[3,3]
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  ) 
  
  result = model$Ad %*% matrix(data = initialIdleState, nrow = 9, ncol = 1)
  
  expect_equal(result[,1], c(-model$Ts**2/2,-model$Ts,1,0,0,0,0,0,0))
})

test_that("model test a_x = 1 with z rotation 90 deg", {
  initialIdleState = c(0,0,1,0,0,0,0,0,0)
  initialIdleBelief = diag(9)
  initial_gravity = data.frame(
    x = 0,
    y = 0,
    z = 9.81
  )
  
  phi = pi/2
  rotationMatrix = quaternion_rotation_matrix(
    sin(phi/2),
    0,
    sin(phi/2),
    0
  )
  
  initial_matrixHandToLocal = data.frame(
    mx_x = rotationMatrix[1,1], my_x = rotationMatrix[1,2], mz_x = rotationMatrix[1,3],
    mx_y = rotationMatrix[2,1], my_y = rotationMatrix[2,2], mz_y = rotationMatrix[2,3],
    mx_z = rotationMatrix[3,1], my_z = rotationMatrix[3,2], mz_z = rotationMatrix[3,3]
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  ) 
  
  result = model$Ad %*% matrix(data = initialIdleState, nrow = 9, ncol = 1)
  
  expect_equal(result[,1], c(0,0,1,0,0,0,model$Ts**2/2,model$Ts,0))
})

test_that("model test a_x = 1 with z rotation 90 deg", {
  initialIdleState = c(0,0,1,0,0,0,0,0,0)
  initialIdleBelief = diag(9)
  initial_gravity = data.frame(
    x = 0,
    y = 0,
    z = 9.81
  )
  
})
