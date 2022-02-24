x_initial_state_q0 = 1.0
x_initial_state_q1 = 0.0
x_initial_state_q2 = 0.0
x_initial_state_q3 = 0.0

x_initial_state_euler_vx = 1.0
x_initial_state_euler_vy = 1.0
x_initial_state_euler_vz = 1.0

initial_belief_00_q0 = 0.5
initial_belief_11_q1 = 0.5
initial_belief_22_q2 = 0.5
initial_belief_33_q3 = 0.5
initial_belief_44_euler_vx = 0.5
initial_belief_55_euler_vy = 0.5
initial_belief_66_euler_vz = 0.5

x_initial_state_px = 0.0
x_initial_state_vx = 1.0
x_initial_state_ax = 1.0
x_initial_state_py = 0.0
x_initial_state_vy = 1.0
x_initial_state_ay = 1.0
x_initial_state_pz = 0.0
x_initial_state_vz = 1.0
x_initial_state_az = 1.0

initial_belief_px_00 = 0.05
initial_belief_vx_11 = 0.5
initial_belief_ax_22 = 0.5
initial_belief_py_33 = 0.05
initial_belief_vy_44 = 0.5
initial_belief_ay_55 = 0.5
initial_belief_pz_66 = 0.05
initial_belief_vz_77 = 0.5
initial_belief_az_88 = 0.5

x_initial_yaw_velocity = 51.2142
x_initial_pitch_velocity = -59.64433
x_initial_roll_velocity = 29.44991

initial_gravity_x = -0.862976
initial_gravity_y = 0.491126
initial_gravity_z = 0.117758

initial_rotation_matrix_11 = 0.6510104
initial_rotation_matrix_12 = 0.6506626
initial_rotation_matrix_13 = 0.6508916
initial_rotation_matrix_21 = 0.650859
initial_rotation_matrix_22 = 0.6506928
initial_rotation_matrix_23 = 0.6510568
initial_rotation_matrix_31 = 0.6507239
initial_rotation_matrix_32 = 0.6507714
initial_rotation_matrix_33 = 0.6511943

initial_orientation_state = c(
  x_initial_state_q0, 
  x_initial_state_q1,
  x_initial_state_q2,
  x_initial_state_q3,
  x_initial_yaw_velocity,
  x_initial_pitch_velocity,
  x_initial_roll_velocity
)

initial_orientation_belief = diag(
  c(
    initial_belief_00_q0,  
    initial_belief_11_q1,
    initial_belief_22_q2,
    initial_belief_33_q3,
    initial_belief_44_euler_vx,
    initial_belief_55_euler_vy,
    initial_belief_66_euler_vz
  )
)

initial_matrixHandToLocal = data.frame(
  mx_x = initial_rotation_matrix_11, my_x = initial_rotation_matrix_12, mz_x = initial_rotation_matrix_13,
  mx_y = initial_rotation_matrix_21, my_y = initial_rotation_matrix_22, mz_y = initial_rotation_matrix_23,
  mx_z = initial_rotation_matrix_31, my_z = initial_rotation_matrix_32, mz_z = initial_rotation_matrix_33
)

initial_gravity = data.frame(
  x = initial_gravity_x,
  y = initial_gravity_y,
  z = initial_gravity_z
)

initial_acceleration_state = c(
  x_initial_state_px,  
  x_initial_state_vx,
  x_initial_state_ax,
  x_initial_state_py,  
  x_initial_state_vy,
  x_initial_state_ay,
  x_initial_state_pz,  
  x_initial_state_vz,
  x_initial_state_az
)

initial_acceleration_belief = diag(c(
  initial_belief_px_00,
  initial_belief_vx_11,
  initial_belief_ax_22,
  initial_belief_py_33,
  initial_belief_vy_44,
  initial_belief_ay_55,
  initial_belief_pz_66,
  initial_belief_vz_77,
  initial_belief_az_88
))

acceleration_process_noise = data.frame(x = 0.05, y = 0.05, z = 0.05)
acceleration_sensor_noise = data.frame(x = 0.05, y = 0.05, z = 0.05, ax = 0.009810001, ay = 0.009810001, az = 0.009810001)

orientation_process_noise = data.frame(vx = 0.05, vy = 0.05, vz = 0.05)
orientation_sensor_noise = data.frame(vx = 0.5, vy = 0.5, vz = 0.5)

processAccelerationModel = getGuanglongAccelerationModel(initial_acceleration_state, initial_acceleration_belief, initial_matrixHandToLocal, initial_gravity, acceleration_process_noise, acceleration_sensor_noise)
processOrientationModel = getGuanglongOrientationModel(initial_orientation_state, initial_orientation_belief, orientation_process_noise, orientation_sensor_noise)
