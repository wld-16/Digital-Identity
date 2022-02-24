
library(testthat)
library(usethis)

source("D:\\Projekte\\Digital-Identity\\data_science\\scripts\\functions.R")
source("D:\\Projekte\\Digital-Identity\\data_science\\scripts\\models.R")
source("D:\\Projekte\\Digital-Identity\\data_science\\scripts\\initial_data.R")

test_that("model remains 0 when input 0", {
  initial_orientation_state = c(1,0,0,0,0,0,0)
  initial_orientation_belief = diag(7)
  
  model = getGuanglongOrientationModel(
    initialState = initial_orientation_state, 
    initialBelief = initial_orientation_belief,
    processNoise = orientation_process_noise,
    sensorNoise = orientation_sensor_noise)
  
  result = model$Ad %*% initial_orientation_state
  expect_equal(result[,1], c(1,0,0,0,0,0,0), nrow = 7, ncol = 1)
})

test_that("model quaternion change when x_angle_velocity = 1 and q = identity", {
  initial_orientation_state = c(1,0,0,0,1,0,0)
  initial_orientation_belief = diag(7)
  
  model = getGuanglongOrientationModel(
    initialState = initial_orientation_state, 
    initialBelief = initial_orientation_belief,
    processNoise = orientation_process_noise,
    sensorNoise = orientation_sensor_noise)
  
  result = model$Ad %*% initial_orientation_state
  expect_equal(result[,1], c(1,model$Ts/2,0,0,1,0,0), nrow = 7, ncol = 1)
})

test_that("model quaternion change when y_angle_velocity = 1 and q = identity", {
  initial_orientation_state = c(1,0,0,0,0,1,0)
  initial_orientation_belief = diag(7)
  
  model = getGuanglongOrientationModel(
    initialState = initial_orientation_state, 
    initialBelief = initial_orientation_belief,
    processNoise = orientation_process_noise,
    sensorNoise = orientation_sensor_noise)
  
  result = model$Ad %*% initial_orientation_state
  expect_equal(result[,1], c(1,0,model$Ts/2,0,0,1,0), nrow = 7, ncol = 1)
})

test_that("model quaternion change when z_angle_velocity = 1 and q = identity", {
  initial_orientation_state = c(1,0,0,0,0,0,1)
  initial_orientation_belief = diag(7)
  
  model = getGuanglongOrientationModel(
    initialState = initial_orientation_state, 
    initialBelief = initial_orientation_belief,
    processNoise = orientation_process_noise,
    sensorNoise = orientation_sensor_noise)
  
  result = model$Ad %*% initial_orientation_state
  expect_equal(result[,1], c(1,0,0,model$Ts/2,0,0,1), nrow = 7, ncol = 1)
})

test_that("model quaternion change when x_angle_velocity = 2 and q = identity", {
  initial_orientation_state = c(1,0,0,0,2,0,0)
  initial_orientation_belief = diag(7)
  
  model = getGuanglongOrientationModel(
    initialState = initial_orientation_state, 
    initialBelief = initial_orientation_belief,
    processNoise = orientation_process_noise,
    sensorNoise = orientation_sensor_noise)
  
  print(model$Ad)
  result = model$Ad %*% initial_orientation_state
  expect_equal(result[,1], c(1,2 * model$Ts/2,0,0,2,0,0), nrow = 7, ncol = 1)
})

test_that("model quaternion change when x_angle_velocity = 2 and q = (.5,.5,.5,.5)", {
  initial_orientation_state = c(0.5,0.5,0.5,0.5,2,0,0)
  initial_orientation_belief = diag(7)
  
  model = getGuanglongOrientationModel(
    initialState = initial_orientation_state, 
    initialBelief = initial_orientation_belief,
    processNoise = orientation_process_noise,
    sensorNoise = orientation_sensor_noise)
  
  result = model$Ad %*% initial_orientation_state
  expect_equal(result[,1],
               c(0.5 - 2 * 0.5 * model$Ts/2,
                 0.5 + 2 * 0.5 * model$Ts/2,
                 0.5 + 2 * 0.5 * model$Ts/2,
                 0.5 - 2 * 0.5 * model$Ts/2,
                 2,
                 0,
                 0
                 ), nrow = 7, ncol = 1)
  print(result[,1])
})

