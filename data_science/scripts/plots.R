install.packages("tidyverse")
library(readr)
library(tidyverse)

Accelerometer <- read_delim("D:/Projekte/Digital-Identity/data_science/phybox_data/Accelerometer.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

ggplot(data=Accelerometer) + geom_point(mapping = aes(x = `Time (s)`, y = `Acceleration x (m/s^2)`))
ggplot(data=Accelerometer) + geom_point(mapping = aes(x = `Time (s)`, y = `Acceleration y (m/s^2)`))
ggplot(data=Accelerometer) + geom_point(mapping = aes(x = `Time (s)`, y = `Acceleration z (m/s^2)`))

Gyroscope <- read_delim("D:/Projekte/Digital-Identity/data_science/phybox_data/Gyroscope.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

ggplot(data=Gyroscope) + geom_point(mapping = aes(x = `Time (s)`, y = `Gyroscope x (rad/s)`))
ggplot(data=Gyroscope) + geom_point(mapping = aes(x = `Time (s)`, y = `Gyroscope y (rad/s)`))
ggplot(data=Gyroscope) + geom_point(mapping = aes(x = `Time (s)`, y = `Gyroscope z (rad/s)`))

Magnetometer <- read_delim("D:/Projekte/Digital-Identity/data_science/phybox_data/Magnetometer.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

ggplot(data=Magnetometer) + geom_point(mapping = aes(x = `Time (s)`, y = `Magnetic field x (µT)`))
ggplot(data=Magnetometer) + geom_point(mapping = aes(x = `Time (s)`, y = `Magnetic field y (µT)`))
ggplot(data=Magnetometer) + geom_point(mapping = aes(x = `Time (s)`, y = `Magnetic field z (µT)`))

