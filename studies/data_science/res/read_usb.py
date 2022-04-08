#!/usr/bin/python

import serial
import string

output = " "
ser = serial.Serial('/dev/ttyUSB0', 1152000, 8, 'N',1,1,False,True,False)
while True:
  print "----"
  while output != "":
    output = ser.readline()
    print output
  output = " "