install.packages("dlm")

library(RMThreshold)
library(ggplot2)
library(RSpincalc)
library(reshape2)
library(dlm)


test = matrix(data = c(
  1,0,0,dt,0,0,dt*dt/2,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,1,0,0,dt,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,1,0,0,dt,0,0,dt*dt/2,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,1,0,0,dt,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
  0,0,1,0,0,dt,0,0,dt*dt/2,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,1,0,0,dt,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,dt*dt/2,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
  0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,dt*dt/2,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
  0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,0,0,dt*dt/2,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,dt,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
)
,nrow = 18, ncol = 18)

a = matrix(data = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 18, ncol = 1) ## state vector
H = matrix(data = c(
  1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0
), nrow = 6, ncol = 18) ## observation/ measurment vector 

Q = add.Gaussian.noise(matrix(data = 1,nrow = 18, ncol = 18), mean = 50, stddev = 5) ## white noise as process noise
initial_P = diag(18) * 200

x_sigma = 0.05; xy_sigma = 0; xz_sigma = 0; xyaw_sigma = 0; xpitch_sigma = 0; xroll_sigma = 0
yx_sigma = 0; y_sigma = 0.05; yz_sigma = 0; yyaw_sigma = 0; ypitch_sigma = 0; yroll_sigma = 0
zx_sigma = 0; zy_sigma = 0; z_sigma = 0.5; zyaw_sigma = 0; zpitch_sigma = 0; zroll_sigma = 0
yawx_sigma = 0; yawy_sigma = 0; yawz_sigma = 0; yaw_sigma=0.05; yawpitch_sigma=0; yawroll_sigma = 0
pitchx_sigma = 0; pitchy_sigma = 0; pitchz_sigma = 0; pitchyaw_sigma = 0; pitch_sigma=0.05; pitchroll_sigma=0
rollx_sigma = 0; rolly_sigma = 0; rollz_sigma = 0; rollyaw_sigma = 0; rollpitch_sigma = 0; roll_sigma = 0.05

R = matrix(data = c(x_sigma**2, xy_sigma, xz_sigma, xyaw_sigma, xpitch_sigma, xroll_sigma,
                    yx_sigma, y_sigma**2, yz_sigma, yyaw_sigma, ypitch_sigma, yroll_sigma,
                    zx_sigma, zy_sigma, z_sigma**2, zyaw_sigma, zpitch_sigma, zroll_sigma,
                    yawx_sigma, yawy_sigma, yawz_sigma, yaw_sigma**2, yawpitch_sigma, yawroll_sigma,
                    pitchx_sigma, pitchy_sigma, pitchz_sigma, pitchyaw_sigma, pitch_sigma**2, pitchroll_sigma,
                    rollx_sigma, rolly_sigma, rollz_sigma, rollyaw_sigma, rollpitch_sigma, roll_sigma**2
), nrow = 6, ncol = 6)

process_model <- list()
process_model$m0
process_model$C0
process_model$FF = H
process_model$V
process_model$GG = F
process_model$W = R


#elapsed_time = tm - proc.time()[[3]]
#print(paste("elapsed: ", elapsed_time, " seconds", sep=""))

## an ARIMA fit
fit3 <- arima(presidents, c(3, 0, 0))
predict(fit3, 12)
# reconstruct this

pr <- KalmanForecast(12, process_model)
pr$pred + fit3$coef[4]
sqrt(pr$var * fit3$sigma2)
# and now do it year by year
mod <- fit3$model
for(y in df) {
  pr <- KalmanForecast(4, mod, TRUE)
  print(list(pred = pr$pred + fit3$coef["intercept"], 
             se = sqrt(pr$var * fit3$sigma2)))
  mod <- attr(pr, "mod")
}