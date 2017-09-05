########################
#Spring flight length###
########################

source()
source()
######
#Data#
######
spring_flight_length <- read.csv("spring_flight_length.csv")

###Codling moth###
#Length 75% - 25 %

CM_25_75 <- spring_flight_length$CM.75 - spring_flight_length$CM.25
CM_10_95 <- spring_flight_length$CM.95 - spring_flight_length$CM.10
plot(CM_25_75,type='l')
plot(CM_10_95,type='b')

#-------------------------------#
###Obliquebanded leafroller###
#Length 75% - 25 %

OB_25_75 <- spring_flight_length$OB.75 - spring_flight_length$OB.25
OB_10_95 <- spring_flight_length$OB.95 - spring_flight_length$OB.10
plot(OB_25_75,type='b')
plot(OB_10_95,type='b')









