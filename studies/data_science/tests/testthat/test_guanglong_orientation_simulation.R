library(testthat)
library(usethis)

source("D:\\Projekte\\Digital-Identity\\data_science\\scripts\\functions.R")
source("D:\\Projekte\\Digital-Identity\\data_science\\scripts\\models.R")
source("D:\\Projekte\\Digital-Identity\\data_science\\scripts\\initial_data.R")
source("D:\\Projekte\\Digital-Identity\\data_science\\scripts\\simulation.R")

test_that("idle state no rotation no angular velocity no noise", {
  initial_orientation_state = c(1,0,0,0,0,0,0)
  initial_orientation_belief = diag(7)
  orientation_process_noise = data.frame(
    vx = 0.0,
    vy = 0.0,
    vz = 0.0
  )
  
  orientation_sensor_noise = data.frame(
    vx = 0.00,
    vy = 0.00,
    vz = 0.00
  )
  
  model = getGuanglongOrientationModel(
    initialState = initial_orientation_state, 
    initialBelief = initial_orientation_belief,
    processNoise = orientation_process_noise,
    sensorNoise = orientation_sensor_noise
    )
  
  setOrientationSimulationModell(processOrientationModel)
  setOrientationSimulationProcessNoise(orientation_process_noise)
  setOrientationSimulationSensorNoise(orientation_sensor_noise)
  
  x_k = as.double(initial_orientation_state)
  
  resultState = generateNextOrientationFrame(x_k)
  resultMeasurement = generateNextOrientationMeasurement(resultState)
  
  expect_equal(resultState[1,1], 1)
  expect_equal(resultState[2,1], 0) 
  expect_equal(resultState[3,1], 0) 
  expect_equal(resultState[4,1], 0) 
  expect_equal(resultState[5,1], 0) 
  expect_equal(resultState[6,1], 0) 
  expect_equal(resultState[7,1], 0)
  
  expect_equal(resultMeasurement[1,1], 0)
  expect_equal(resultMeasurement[2,1], 0) 
  expect_equal(resultMeasurement[3,1], 0) 
  
})

test_that("idle state no rotation no angular velocity with noise", {
  initial_orientation_state = c(1,0,0,0,0,0,0)
  initial_orientation_belief = diag(7)
  orientation_process_noise = data.frame(
    vx = 0.05,
    vy = 0.05,
    vz = 0.05
  )
  
  orientation_sensor_noise = data.frame(
    vx = 0.01,
    vy = 0.01,
    vz = 0.01
  )
  
  model = getGuanglongOrientationModel(
    initialState = initial_orientation_state, 
    initialBelief = initial_orientation_belief,
    processNoise = orientation_process_noise,
    sensorNoise = orientation_sensor_noise
  )
  
  setOrientationSimulationModell(processOrientationModel)
  setOrientationSimulationProcessNoise(orientation_process_noise)
  setOrientationSimulationSensorNoise(orientation_sensor_noise)
  
  x_k = as.double(initial_orientation_state)
  
  resultState = generateNextOrientationFrame(x_k)
  resultMeasurement = generateNextOrientationMeasurement(resultState)
  
  expect_true(resultState[1] == 1)
  expect_true(resultState[2] == 0) 
  expect_true(resultState[3] == 0) 
  expect_true(resultState[4] == 0) 
  expect_true(resultState[5] < 0.05 * 3 && resultState[5] > -0.05 * 3 && resultState[5] != 0)
  expect_true(resultState[6] < 0.05 * 3 && resultState[6] > -0.05 * 3 && resultState[6] != 0)
  expect_true(resultState[7] < 0.05 * 3 && resultState[7] > -0.05 * 3 && resultState[7] != 0)
  
  expect_true(resultMeasurement[1] > -0.05 * 3 -0.01 * 3 && resultMeasurement[1] < +0.05 * 3 +0.01 * 3 && resultState[5] != 0)
  expect_true(resultMeasurement[2] > -0.05 * 3 -0.01 * 3 && resultMeasurement[2] < +0.05 * 3 +0.01 * 3 && resultState[6] != 0)
  expect_true(resultMeasurement[3] > -0.05 * 3 -0.01 * 3 && resultMeasurement[3] < +0.05 * 3 +0.01 * 3 && resultState[7] != 0)
  
})

test_that("single simulation step with 0.1 deg angular velocity on x in idealized environment", {
  
  # This example illustrates, that the quaternion behaves linear
  
  initial_orientation_state = c(1,0,0,0, 0.1 / 0.014 * pi / 180, 0, 0)
  initial_orientation_belief = diag(7)
  orientation_process_noise = data.frame(
    vx = 0.0,
    vy = 0.0,
    vz = 0.0
  )
  
  orientation_sensor_noise = data.frame(
    vx = 0.00,
    vy = 0.00,
    vz = 0.00
  )
  
  model = getGuanglongOrientationModel(
    initialState = initial_orientation_state, 
    initialBelief = initial_orientation_belief,
    processNoise = orientation_process_noise,
    sensorNoise = orientation_sensor_noise
    )
  
  setOrientationSimulationModell(processOrientationModel)
  setOrientationSimulationProcessNoise(orientation_process_noise)
  setOrientationSimulationSensorNoise(orientation_sensor_noise)
  
  x_k = as.double(initial_orientation_state)
  
  resultState = generateNextOrientationFrame(x_k)
  resultMeasurement = generateNextOrientationMeasurement(resultState)
  
  expect_equal(resultState[1], 0.9999, tolerance = 0.0001)
  expect_equal(resultState[2], 0.0008, tolerance = 0.0001) 
  expect_equal(resultState[3], 0, tolerance = 0.0001) 
  expect_equal(resultState[4], 0, tolerance = 0.0001) 
  expect_equal(resultState[5], deg2rad(0.1 / 0.014), tolerance = 0.0001) 
  expect_equal(resultState[6], 0) 
  expect_equal(resultState[7], 0)
  
  expect_equal(resultMeasurement[1], deg2rad(0.1 / 0.014))
  expect_equal(resultMeasurement[2], 0) 
  expect_equal(resultMeasurement[3], 0) 
  
})

test_that("single simulation step with 0.1 deg angular velocity on y in idealized environment", {
  initial_orientation_state = c(1,0,0,0, 0 , deg2rad(0.1 / 0.014), 0)
  initial_orientation_belief = diag(7)
  orientation_process_noise = data.frame(
    vx = 0.0,
    vy = 0.0,
    vz = 0.0
  )
  
  orientation_sensor_noise = data.frame(
    vx = 0.00,
    vy = 0.00,
    vz = 0.00
  )
  
  model = getGuanglongOrientationModel(
    initialState = initial_orientation_state, 
    initialBelief = initial_orientation_belief,
    processNoise = orientation_process_noise,
    sensorNoise = orientation_sensor_noise)
  
  setOrientationSimulationModell(processOrientationModel)
  setOrientationSimulationProcessNoise(orientation_process_noise)
  setOrientationSimulationSensorNoise(orientation_sensor_noise)
  
  x_k = as.double(initial_orientation_state)
  
  resultState = generateNextOrientationFrame(x_k)
  resultMeasurement = generateNextOrientationMeasurement(resultState)
  
  expect_equal(resultState[1], 0.9999, tolerance = 0.0001)
  expect_equal(resultState[2], 0, tolerance = 0.0001) 
  expect_equal(resultState[3], 0.0008, tolerance = 0.0001) 
  expect_equal(resultState[4], 0, tolerance = 0.0001) 
  expect_equal(resultState[5], 0) 
  expect_equal(resultState[6], deg2rad(0.1 / 0.014), tolerance = 0.0001) 
  expect_equal(resultState[7], 0)
  
  expect_equal(resultMeasurement[1], 0)
  expect_equal(resultMeasurement[2], deg2rad(0.1 / 0.014)) 
  expect_equal(resultMeasurement[3], 0) 
  
})

test_that("single simulation step with 0.1 deg angular velocity on z in idealized environment", {
  initial_orientation_state = c(1,0,0,0, 0,0, deg2rad(0.1 / 0.014))
  initial_orientation_belief = diag(7)
  orientation_process_noise = data.frame(
    vx = 0.0,
    vy = 0.0,
    vz = 0.0
  )
  
  orientation_sensor_noise = data.frame(
    vx = 0.00,
    vy = 0.00,
    vz = 0.00
  )
  
  model = getGuanglongOrientationModel(
    initialState = initial_orientation_state, 
    initialBelief = initial_orientation_belief,
    processNoise = orientation_process_noise,
    sensorNoise = orientation_sensor_noise)
  
  setOrientationSimulationModell(processOrientationModel)
  setOrientationSimulationProcessNoise(orientation_process_noise)
  setOrientationSimulationSensorNoise(orientation_sensor_noise)
  
  x_k = as.double(initial_orientation_state)
  
  resultState = generateNextOrientationFrame(x_k)
  resultMeasurement = generateNextOrientationMeasurement(resultState)
  
  expect_equal(resultState[1], 0.9999, tolerance = 0.0001)
  expect_equal(resultState[2], 0, tolerance = 0.0001) 
  expect_equal(resultState[3], 0, tolerance = 0.0001) 
  expect_equal(resultState[4], 0.0008, tolerance = 0.0001) 
  expect_equal(resultState[5], 0) 
  expect_equal(resultState[6], 0) 
  expect_equal(resultState[7], deg2rad(0.1 / 0.014), tolerance = 0.0001)
  
  expect_equal(resultMeasurement[1], 0)
  expect_equal(resultMeasurement[2], 0) 
  expect_equal(resultMeasurement[3], deg2rad(0.1 / 0.014)) 
  
})

test_that("single simulation step with 0.1 deg angular velocity on x and z in idealized environment", {
  initial_orientation_state = c(1,0,0,0, 0, deg2rad(0.1 / 0.014), deg2rad(0.1 / 0.014))
  initial_orientation_belief = diag(7)
  orientation_process_noise = data.frame(
    vx = 0.0,
    vy = 0.0,
    vz = 0.0
  )
  
  orientation_sensor_noise = data.frame(
    vx = 0.00,
    vy = 0.00,
    vz = 0.00
  )
  
  model = getGuanglongOrientationModel(
    initialState = initial_orientation_state, 
    initialBelief = initial_orientation_belief,
    processNoise = orientation_process_noise,
    sensorNoise = orientation_sensor_noise)
  
  setOrientationSimulationModell(processOrientationModel)
  setOrientationSimulationProcessNoise(orientation_process_noise)
  setOrientationSimulationSensorNoise(orientation_sensor_noise)
  
  x_k = as.double(initial_orientation_state)
  
  resultState = generateNextOrientationFrame(x_k)
  resultMeasurement = generateNextOrientationMeasurement(resultState)
  
  expect_equal(resultState[1], 0.9999, tolerance = 0.0001)
  expect_equal(resultState[2], 0, tolerance = 0.0001) 
  expect_equal(resultState[3], 0.0008, tolerance = 0.0001) 
  expect_equal(resultState[4], 0.0008, tolerance = 0.0001) 
  expect_equal(resultState[5], 0 ) 
  expect_equal(resultState[6], deg2rad(0.1 / 0.014), tolerance = 0.0001) 
  expect_equal(resultState[7], deg2rad(0.1 / 0.014), tolerance = 0.0001)
  
  expect_equal(resultMeasurement[1], 0)
  expect_equal(resultMeasurement[2], deg2rad(0.1 / 0.014)) 
  expect_equal(resultMeasurement[3], deg2rad(0.1 / 0.014)) 
  
})

test_that("3 simulation step with 0.1 deg angular velocity on x with noise", {
  initial_orientation_state = c(1,0,0,0, deg2rad(0.1 / 0.014), 0, 0)
  initial_orientation_belief = diag(7)
  orientation_process_noise = data.frame(
    vx = 0.2,
    vy = 0.2,
    vz = 0.2
  )
  
  orientation_sensor_noise = data.frame(
    vx = 0.01,
    vy = 0.01,
    vz = 0.01
  )
  
  model = getGuanglongOrientationModel(
    initialState = initial_orientation_state, 
    initialBelief = initial_orientation_belief,
    processNoise = orientation_process_noise,
    sensorNoise = orientation_sensor_noise
  )
  
  setOrientationSimulationModell(processOrientationModel)
  setOrientationSimulationProcessNoise(orientation_process_noise)
  setOrientationSimulationSensorNoise(orientation_sensor_noise)
  
  x_k = as.double(initial_orientation_state)
  
  resultState = Reduce(generateNextOrientationFrame, 1:3, x_k)
  resultMeasurement = generateNextOrientationMeasurement(resultState)
  
  expect_equal(resultState[1], 0.9999, tolerance = 0.01)
  expect_equal(resultState[2], 0.0087, tolerance = 0.01) 
  expect_equal(resultState[3], 0, tolerance = 0.01) 
  expect_equal(resultState[4], 0, tolerance = 0.01) 
  expect_true(resultState[5] > -deg2rad(0.1 / 0.014) - 0.2 * 3 * 3 && resultState[5] < deg2rad(0.1 / 0.014) + 0.2 * 3 * 3 ) 
  expect_true(resultState[6] > 0 - 0.2 * 3 * 3 && resultState[5] < 0 + 0.2 * 3 * 3 ) 
  expect_true(resultState[7] > 0 - 0.2 * 3 * 3 && resultState[5] < 0 + 0.2 * 3 * 3 )
  
  expect_true(resultMeasurement[1] > -deg2rad(0.1 / 0.014) - 0.2 * 3 * 3 && resultState[5] < deg2rad(0.1 / 0.014) + 0.2 * 3 * 3 ) 
  expect_true(resultMeasurement[2] > 0 - 0.2 * 3 * 3 + 0.01 * 3 * 3 && resultState[5] < 0 + 0.2 * 3 * 3 + 0.01 * 3 * 3 ) 
  expect_true(resultMeasurement[3] > 0 - 0.2 * 3 * 3 + 0.01 * 3 * 3 && resultState[5] < 0 + 0.2 * 3 * 3 + 0.01 * 3 * 3 )
  
}) 

test_that("10 simulation step with 0.1 deg angular velocity on x in idealized environment", {
  initial_orientation_state = c(1,0,0,0, deg2rad(0.1 / 0.014), 0, 0)
  initial_orientation_belief = diag(7)
  orientation_process_noise = data.frame(
    vx = 0.0,
    vy = 0.0,
    vz = 0.0
  )
  
  orientation_sensor_noise = data.frame(
    vx = 0.00,
    vy = 0.00,
    vz = 0.00
  )
  
  model = getGuanglongOrientationModel(
    initialState = initial_orientation_state, 
    initialBelief = initial_orientation_belief,
    processNoise = orientation_process_noise,
    sensorNoise = orientation_sensor_noise
  )
  
  setOrientationSimulationModell(processOrientationModel)
  setOrientationSimulationProcessNoise(orientation_process_noise)
  setOrientationSimulationSensorNoise(orientation_sensor_noise)
  
  x_k = as.double(initial_orientation_state)
  
  resultState = Reduce(generateNextOrientationFrame, 1:10, x_k)
  resultMeasurement = generateNextOrientationMeasurement(resultState)
  
  expect_equal(resultState[1], 0.9999, tolerance = 0.0001)
  expect_equal(resultState[2], 0.0087, tolerance = 0.0001) 
  expect_equal(resultState[3], 0, tolerance = 0.0001) 
  expect_equal(resultState[4], 0, tolerance = 0.0001) 
  expect_equal(resultState[5], deg2rad(0.1 / 0.014), tolerance = 0.0001) 
  expect_equal(resultState[6], 0) 
  expect_equal(resultState[7], 0)
  
  expect_equal(resultMeasurement[1], deg2rad(0.1 / 0.014))
  expect_equal(resultMeasurement[2], 0) 
  expect_equal(resultMeasurement[3], 0) 
}) 
