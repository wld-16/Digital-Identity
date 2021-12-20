
# Uncomment to install packages
# install.packages("testthat")


library(testthat)

initialIdleState = c(0,0,1,0,0,0,0,0,0)
initialIdleBelief = diag(9)
initial_gravity = data.frame(
  x = 0,
  y = 0,
  z = 9.81
)

nx = 0
ny = 1
nz = 0
#idleRotationMatrix = quaternion_rotation_matrix(1,0,0,0)
phi = pi/2
rotationMatrixByY = quaternion_rotation_matrix(cos(phi/2),nx * sin(phi/2), ny * sin(phi/2), nz * sin(phi/2))

initial_matrixHandToLocal = data.frame(
  mx_x = rotationMatrixByY[1,1], my_x = rotationMatrixByY[1,2], mz_x = rotationMatrixByY[1,3],
  mx_y = rotationMatrixByY[2,1], my_y = rotationMatrixByY[2,2], mz_y = rotationMatrixByY[2,3],
  mx_z = rotationMatrixByY[3,1], my_z = rotationMatrixByY[3,2], mz_z = rotationMatrixByY[3,3]
)


idleAccModel = getGuanglongAccelerationModel(initialIdleState, initialIdleBelief,initial_matrixHandToLocal , initial_gravity) 
print(idleAccModel$Ad)

x_1 = idleAccModel$Ad %*% initialIdleState
print(x_1)


test