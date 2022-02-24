library(testthat)
library(usethis)

source("D:\\Projekte\\Digital-Identity\\data_science\\scripts\\functions.R")
source("D:\\Projekte\\Digital-Identity\\data_science\\scripts\\models.R")
source("D:\\Projekte\\Digital-Identity\\data_science\\scripts\\initial_data.R")
source("D:\\Projekte\\Digital-Identity\\data_science\\scripts\\simulation.R")

test_that("single simulation step with no rotation no noise", {
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
  acceleration_process_noise = data.frame(
    x = 0.0,
    y = 0.0,
    z = 0.0
  )
  
  acceleration_sensor_noise = data.frame(
    x = 0.00,
    y = 0.00,
    z = 0.00,
    ax = 0.00,
    ay = 0.00,
    az = 0.00
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  )
  
  setAccelerationSimulationModell(model)
  setAccelerationSimulationProcessNoise(acceleration_process_noise)
  setAccelerationSimulationSensorNoise(acceleration_sensor_noise)
  
  x_k = as.double(initialIdleState)
  
  resultState = generateNextAccelerationFrame(x_k)
  resultMeasurement = generateNextAccelerationMeasurement(resultState)
  
  expect_equal(resultState[1], 0)
  expect_equal(resultState[2], 0)  
  expect_equal(resultState[3], 0) 
  expect_equal(resultState[4], 0)  
  expect_equal(resultState[5], 0)  
  expect_equal(resultState[6], 0) 
  expect_equal(resultState[7], 0)  
  expect_equal(resultState[8], 0) 
  expect_equal(resultState[9], 0)
  
  expect_equal(resultMeasurement[1], 0.00)
  expect_equal(resultMeasurement[2], 0.00)
  expect_equal(resultMeasurement[3], 0.00)
  expect_equal(resultMeasurement[4], 0.00)
  expect_equal(resultMeasurement[5], 0.00)
  expect_equal(resultMeasurement[6], 0.00)
})

test_that("single simulation step with acceleration on x axis no noise", {
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
  
  acceleration_process_noise = data.frame(
    x = 0.0,
    y = 0.0,
    z = 0.0
  )
  
  acceleration_sensor_noise = data.frame(
    x = 0.0,
    y = 0.0,
    z = 0.0,
    ax = 0.0,
    ay = 0.0,
    az = 0.0
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  )
  
  setAccelerationSimulationModell(model)
  setAccelerationSimulationProcessNoise(acceleration_process_noise)
  setAccelerationSimulationSensorNoise(acceleration_sensor_noise)
  
  x_k = as.double(initialIdleState)
  
  resultState = generateNextAccelerationFrame(x_k)
  resultMeasurement = generateNextAccelerationMeasurement(resultState)
  
  expect_equal(resultState[1], 0.014**2/2)
  expect_equal(resultState[2], 0.014)  
  expect_equal(resultState[3], 1) 
  expect_equal(resultState[4], 0)  
  expect_equal(resultState[5], 0)  
  expect_equal(resultState[6], 0) 
  expect_equal(resultState[7], 0)  
  expect_equal(resultState[8], 0) 
  expect_equal(resultState[9], 0)
  
  expect_equal(resultMeasurement[1], 0.014**2/2)
  expect_equal(resultMeasurement[2], 1)
  expect_equal(resultMeasurement[3], 0.0)
  expect_equal(resultMeasurement[4], 0.0)
  expect_equal(resultMeasurement[5], 0.0)
  expect_equal(resultMeasurement[6], 0.0)
})

test_that("single simulation step with negative acceleration on y axis no noise", {
  initialIdleState = c(0,0,0,0,0,-1,0,0,0)
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
  
  acceleration_process_noise = data.frame(
    x = 0.0,
    y = 0.0,
    z = 0.0
  )
  
  acceleration_sensor_noise = data.frame(
    x = 0.0,
    y = 0.0,
    z = 0.0,
    ax = 0.0,
    ay = 0.0,
    az = 0.0
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  )
  
  setAccelerationSimulationModell(model)
  setAccelerationSimulationProcessNoise(acceleration_process_noise)
  setAccelerationSimulationSensorNoise(acceleration_sensor_noise)
  
  x_k = as.double(initialIdleState)
  
  resultState = generateNextAccelerationFrame(x_k)
  resultMeasurement = generateNextAccelerationMeasurement(resultState)
  
  expect_equal(resultState[1], 0)
  expect_equal(resultState[2], 0)
  expect_equal(resultState[3], 0)
  expect_equal(resultState[4], -0.014**2/2)
  expect_equal(resultState[5], -0.014)  
  expect_equal(resultState[6], -1) 
  expect_equal(resultState[7], 0)  
  expect_equal(resultState[8], 0) 
  expect_equal(resultState[9], 0)
  
  expect_equal(resultMeasurement[1], 0)
  expect_equal(resultMeasurement[2], 0)
  expect_equal(resultMeasurement[3], -0.014**2/2)
  expect_equal(resultMeasurement[4], -1)
  expect_equal(resultMeasurement[5], 0)
  expect_equal(resultMeasurement[6], 0)
})

test_that("single simulation step with acceleration on z axis no noise", {
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
  
  acceleration_process_noise = data.frame(
    x = 0.0,
    y = 0.0,
    z = 0.0
  )
  
  acceleration_sensor_noise = data.frame(
    x = 0.0,
    y = 0.0,
    z = 0.0,
    ax = 0.0,
    ay = 0.0,
    az = 0.0
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  )
  
  setAccelerationSimulationModell(model)
  setAccelerationSimulationProcessNoise(acceleration_process_noise)
  setAccelerationSimulationSensorNoise(acceleration_sensor_noise)
  
  x_k = as.double(initialIdleState)
  
  resultState = generateNextAccelerationFrame(x_k)
  resultMeasurement = generateNextAccelerationMeasurement(resultState)
  
  expect_equal(resultState[1], 0)
  expect_equal(resultState[2], 0)
  expect_equal(resultState[3], 0)
  expect_equal(resultState[4], 0)
  expect_equal(resultState[5], 0)
  expect_equal(resultState[6], 0)
  expect_equal(resultState[7], 0.014**2/2) 
  expect_equal(resultState[8], 0.014)  
  expect_equal(resultState[9], 1) 
  
  expect_equal(resultMeasurement[1], 0)
  expect_equal(resultMeasurement[2], 0)
  expect_equal(resultMeasurement[3], 0)
  expect_equal(resultMeasurement[4], 0)
  expect_equal(resultMeasurement[5], 0.014**2/2)
  expect_equal(resultMeasurement[6], 1)
})

test_that("single simulation step with acceleration on x axis and a rotated system on the y axis no noise", {
  initialIdleState = c(0,0,1,0,0,0,0,0,0)
  initialIdleBelief = diag(9)
  initial_gravity = data.frame(
    x = 0,
    y = 0,
    z = 9.81
  )
  
  rotationMatrix = quaternion_rotation_matrix(1/sqrt(2),0,1/sqrt(2),0)
  
  initial_matrixHandToLocal = data.frame(
    mx_x = rotationMatrix[1,1], my_x = rotationMatrix[1,2], mz_x = rotationMatrix[1,3],
    mx_y = rotationMatrix[2,1], my_y = rotationMatrix[2,2], mz_y = rotationMatrix[2,3],
    mx_z = rotationMatrix[3,1], my_z = rotationMatrix[3,2], mz_z = rotationMatrix[3,3]
  )
  
  acceleration_process_noise = data.frame(
    x = 0.0,
    y = 0.0,
    z = 0.0
  )
  
  acceleration_sensor_noise = data.frame(
    x = 0.0,
    y = 0.0,
    z = 0.0,
    ax = 0.0,
    ay = 0.0,
    az = 0.0
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  )
  
  setAccelerationSimulationModell(model)
  setAccelerationSimulationProcessNoise(acceleration_process_noise)
  setAccelerationSimulationSensorNoise(acceleration_sensor_noise)
  
  x_k = as.double(initialIdleState)
  
  resultState = generateNextAccelerationFrame(x_k)
  resultMeasurement = generateNextAccelerationMeasurement(resultState)
  
  expect_equal(resultState[1], 0, tolerance = 0.001)
  expect_equal(resultState[2], 0, tolerance = 0.001)
  expect_equal(resultState[3], 1, tolerance = 0.001)
  expect_equal(resultState[4], 0, tolerance = 0.001)
  expect_equal(resultState[5], 0, tolerance = 0.001)
  expect_equal(resultState[6], 0, tolerance = 0.001)
  expect_equal(resultState[7], 0.014**2/2, tolerance = 0.001)
  expect_equal(resultState[8], 0.014, tolerance = 0.001)
  expect_equal(resultState[9], 0, tolerance = 0.001) 
  
  expect_equal(resultMeasurement[1], 0, tolerance = 0.0001)
  expect_equal(resultMeasurement[2], 1, tolerance = 0.0001)
  expect_equal(resultMeasurement[3], 0, tolerance = 0.0001)
  expect_equal(resultMeasurement[4], 0, tolerance = 0.0001)
  expect_equal(resultMeasurement[5], 0.014**2/2, tolerance = 0.0001)
  expect_equal(resultMeasurement[6], 0, tolerance = 0.0001)
})

test_that("single simulation step with acceleration on y axis and a rotated system on the x axis no noise", {
  initialIdleState = c(0,0,0,0,0,1,0,0,0)
  initialIdleBelief = diag(9)
  initial_gravity = data.frame(
    x = 0,
    y = 0,
    z = 9.81
  )
  
  rotationMatrix = quaternion_rotation_matrix(1/sqrt(2),1/sqrt(2),0,0)
  
  initial_matrixHandToLocal = data.frame(
    mx_x = rotationMatrix[1,1], my_x = rotationMatrix[1,2], mz_x = rotationMatrix[1,3],
    mx_y = rotationMatrix[2,1], my_y = rotationMatrix[2,2], mz_y = rotationMatrix[2,3],
    mx_z = rotationMatrix[3,1], my_z = rotationMatrix[3,2], mz_z = rotationMatrix[3,3]
  )
  
  acceleration_process_noise = data.frame(
    x = 0.0,
    y = 0.0,
    z = 0.0
  )
  
  acceleration_sensor_noise = data.frame(
    x = 0.0,
    y = 0.0,
    z = 0.0,
    ax = 0.0,
    ay = 0.0,
    az = 0.0
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  )
  
  setAccelerationSimulationModell(model)
  setAccelerationSimulationProcessNoise(acceleration_process_noise)
  setAccelerationSimulationSensorNoise(acceleration_sensor_noise)
  
  x_k = as.double(initialIdleState)
  
  resultState = generateNextAccelerationFrame(x_k)
  resultMeasurement = generateNextAccelerationMeasurement(resultState)
  
  expect_equal(resultState[1], 0, tolerance = 0.001)
  expect_equal(resultState[2], 0, tolerance = 0.001)
  expect_equal(resultState[3], 0, tolerance = 0.001)
  expect_equal(resultState[4], 0, tolerance = 0.001)
  expect_equal(resultState[5], 0, tolerance = 0.001)
  expect_equal(resultState[6], 1, tolerance = 0.001)
  expect_equal(resultState[7], -0.014**2/2, tolerance = 0.001)
  expect_equal(resultState[8], -0.014, tolerance = 0.001)
  expect_equal(resultState[9], 0, tolerance = 0.001) 
  
  expect_equal(resultMeasurement[1], 0, tolerance = 0.0001)
  expect_equal(resultMeasurement[2], 0, tolerance = 0.0001)
  expect_equal(resultMeasurement[3], 0, tolerance = 0.0001)
  expect_equal(resultMeasurement[4], 1, tolerance = 0.0001)
  expect_equal(resultMeasurement[5], 0.014**2/2, tolerance = 0.001)
  expect_equal(resultMeasurement[6], 0, tolerance = 0.0001)
})

test_that("ten simulation step with acceleration on y axis no noise", {
  initialIdleState = c(0,0,0,0,0,1,0,0,0)
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
  
  acceleration_process_noise = data.frame(
    x = 0.0,
    y = 0.0,
    z = 0.0
  )
  
  acceleration_sensor_noise = data.frame(
    x = 0.0,
    y = 0.0,
    z = 0.0,
    ax = 0.0,
    ay = 0.0,
    az = 0.0
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  )
  
  setAccelerationSimulationModell(model)
  setAccelerationSimulationProcessNoise(acceleration_process_noise)
  setAccelerationSimulationSensorNoise(acceleration_sensor_noise)
  
  x_k = as.double(initialIdleState)
  
  resultState = Reduce(generateNextAccelerationFrame, 1:10, x_k)
  resultMeasurement = generateNextAccelerationMeasurement(resultState)
  
  expect_equal(resultState[1], 0)
  expect_equal(resultState[2], 0)
  expect_equal(resultState[3], 0)
  expect_equal(resultState[4], 0.014**2/2 * 10**2)
  expect_equal(resultState[5], 0.014 * 10)  
  expect_equal(resultState[6], 1) 
  expect_equal(resultState[7], 0)  
  expect_equal(resultState[8], 0) 
  expect_equal(resultState[9], 0)
  
  expect_equal(resultMeasurement[1], 0.0)
  expect_equal(resultMeasurement[2], 0.0)
  expect_equal(resultMeasurement[3], 0.014**2/2 * 10**2)
  expect_equal(resultMeasurement[4], 1)
  expect_equal(resultMeasurement[5], 0.0)
  expect_equal(resultMeasurement[6], 0.0)
})

test_that("ten simulation step with acceleration on x axis no noise", {
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
  
  acceleration_process_noise = data.frame(
    x = 0.0,
    y = 0.0,
    z = 0.0
  )
  
  acceleration_sensor_noise = data.frame(
    x = 0.0,
    y = 0.0,
    z = 0.0,
    ax = 0.0,
    ay = 0.0,
    az = 0.0
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  )
  
  setAccelerationSimulationModell(model)
  setAccelerationSimulationProcessNoise(acceleration_process_noise)
  setAccelerationSimulationSensorNoise(acceleration_sensor_noise)
  
  x_k = as.double(initialIdleState)
  
  resultState = Reduce(generateNextAccelerationFrame, 1:10, x_k)
  resultMeasurement = generateNextAccelerationMeasurement(resultState)
  
  expect_equal(resultState[1], 0.014**2/2 * 10**2)
  expect_equal(resultState[2], 0.014 * 10)  
  expect_equal(resultState[3], 1) 
  expect_equal(resultState[4], 0)  
  expect_equal(resultState[5], 0)  
  expect_equal(resultState[6], 0) 
  expect_equal(resultState[7], 0)  
  expect_equal(resultState[8], 0) 
  expect_equal(resultState[9], 0)
  
  expect_equal(resultMeasurement[1], 0.014**2/2 * 10**2)
  expect_equal(resultMeasurement[2], 1)
  expect_equal(resultMeasurement[3], 0.0)
  expect_equal(resultMeasurement[4], 0.0)
  expect_equal(resultMeasurement[5], 0.0)
  expect_equal(resultMeasurement[6], 0.0)
})

## Test case will fail in around 0.03 % of executions -> https://www.mathsisfun.com/data/standard-normal-distribution.html

test_that("single simulation step with no rotation with noise", {
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
  acceleration_process_noise = data.frame(
    x = 0.5,
    y = 0.5,
    z = 0.5
  )
  
  acceleration_sensor_noise = data.frame(
    x = 0.01,
    y = 0.01,
    z = 0.01,
    ax = 0.05,
    ay = 0.05,
    az = 0.05
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  )
  
  setAccelerationSimulationModell(model)
  setAccelerationSimulationProcessNoise(acceleration_process_noise)
  setAccelerationSimulationSensorNoise(acceleration_sensor_noise)
  
  x_k = as.double(initialIdleState)
  
  resultState = generateNextAccelerationFrame(x_k)
  resultMeasurement = generateNextAccelerationMeasurement(resultState)
  
  
  expect_equal(resultState[1],0)
  expect_equal(resultState[2], 0)  
  expect_true(resultState[3] < 0.5 * 3 && resultState[3] > -0.5 * 3) 
  expect_equal(resultState[4], 0)  
  expect_equal(resultState[5], 0)  
  expect_true(resultState[6] < 0.5 * 3 && resultState[6] > -0.5 * 3) 
  expect_equal(resultState[7], 0)  
  expect_equal(resultState[8], 0) 
  expect_true(resultState[9] < 0.5 * 3 && resultState[9] > -0.5 * 3) 
  
  expect_true(resultMeasurement[1] < 0.01 * 3 && resultMeasurement[1] > -0.01 * 3)
  expect_true(resultMeasurement[2] < 0.5 * 3 + 0.05 * 3 && resultMeasurement[2] > -0.5 * 3 - 0.05 * 3)
  expect_true(resultMeasurement[3] < 0.01 * 3 && resultMeasurement[3] > -0.01 * 3)
  expect_true(resultMeasurement[4] < 0.5 * 3 + 0.05 * 3 && resultMeasurement[4] > -0.5 * 3 - 0.05 * 3)
  expect_true(resultMeasurement[5] < 0.01 * 3 && resultMeasurement[5] > -0.01 * 3)
  expect_true(resultMeasurement[6] < 0.5 * 3 + 0.05 * 3 && resultMeasurement[6] > -0.5 * 3 - 0.05 * 3)
})

test_that("ten simulation step with acceleration on x axis with noise", {
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
  
  acceleration_process_noise = data.frame(
    x = 0.5,
    y = 0.5,
    z = 0.5
  )
  
  acceleration_sensor_noise = data.frame(
    x = 0.01,
    y = 0.01,
    z = 0.01,
    ax = 0.05,
    ay = 0.05,
    az = 0.05
  )
  
  model = getGuanglongAccelerationModel(
    initialIdleState,
    initialIdleBelief,
    initial_matrixHandToLocal,
    initial_gravity,
    acceleration_process_noise, 
    acceleration_sensor_noise
  )
  
  setAccelerationSimulationModell(model)
  setAccelerationSimulationProcessNoise(acceleration_process_noise)
  setAccelerationSimulationSensorNoise(acceleration_sensor_noise)
  
  x_k = as.double(initialIdleState)
  
  resultState = Reduce(generateNextAccelerationFrame, 1:10, x_k)
  resultMeasurement = generateNextAccelerationMeasurement(resultState)
  
  
  expect_true(resultState[1] < 0.5 * 3 * 0.014**2 * 10**2 && resultState[1] > -0.5 * 3 * 0.014**2 * 10**2) 
  expect_true(resultState[2] < 0.5 * 3 * 0.014 * 10**2 && resultState[2] > -0.5 * 3 * 0.014 * 10**2) 
  expect_true(resultState[3] < 0.5 * 3 * 10**2 && resultState[3] > -0.5 * 3 * 10**2) 
  expect_true(resultState[4] < 0.5 * 3 * 0.014**2 * 10**2 && resultState[4] > -0.5 * 3 * 0.014**2 * 10**2) 
  expect_true(resultState[5] < 0.5 * 3 * 0.014 * 10**2 && resultState[5] > -0.5 * 3 * 0.014 * 10**2) 
  expect_true(resultState[6] < 0.5 * 3 * 10**2 && resultState[6] > -0.5 * 3 * 10**2) 
  expect_true(resultState[7] < 0.5 * 3 * 10**2 * 0.014**2&& resultState[7] > -0.5 * 3 * 0.014**2 * 10**2) 
  expect_true(resultState[8] < 0.5 * 3 * 10**2 * 0.014 && resultState[8] > -0.5 * 3 * 0.014 * 10**2) 
  expect_true(resultState[9] < 0.5 * 3 * 10**2 && resultState[9] > -0.5 * 3 * 10**2) 
  
  expect_true(resultMeasurement[1] < 0.01 * 3 * 10**2 && resultMeasurement[1] > -0.01 * 3 * 10**2)
  expect_true(resultMeasurement[2] < 0.5 * 3 + 0.05 * 3 * 10**2 && resultMeasurement[2] > -0.5 * 3 - 0.05 * 3 * 10**2)
  expect_true(resultMeasurement[3] < 0.01 * 3 * 10**2 && resultMeasurement[3] > -0.01 * 3 * 10**2)
  expect_true(resultMeasurement[4] < 0.5 * 3 + 0.05 * 3 * 10**2 && resultMeasurement[4] > -0.5 * 3 - 0.05 * 3 * 10**2)
  expect_true(resultMeasurement[5] < 0.01 * 3 && resultMeasurement[5] > -0.01 * 3 * 10**2)
  expect_true(resultMeasurement[6] < 0.5 * 3 + 0.05 * 3 * 10**2 && resultMeasurement[6] > -0.5 * 3 - 0.05 * 3 * 10**2)
})
