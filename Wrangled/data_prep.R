##############################
#Preparing data for analysis#
#############################
library(here)
library(ggplot2)
library(reshape2)
library(rootSolve)
require(zoo)
require(corrplot)

#Run this first before continuing on to the analysis 
setwd('C:\\Users\\pak\\Desktop\\NAO_FINAL_2\\Species_Data')
###############
#Species Data#
##############

#---------------------------------------------#

#Codling Moth#
#Cleaning up data
CM_main <- read.csv('CM.2.csv')
###Formats the date, codling moth has "Weekday, "month day", "Year###
CM_main$Date <- as.Date(CM_main$Date, format = "%A, %b %d, %Y") 
CM_avg <- cbind.data.frame(CM_main$Date,
                           rowMeans(CM_main[,2:ncol(CM_main)],na.rm=TRUE))
colnames(CM_avg) <- c("Date","Mean")

#---------------------------------------------#

#Obliquebanded leafroller#
OB_main <- read.csv('OBLR.2.csv')
###Formats the date, oblique banded leafroller has month.num/day/Year
OB_main$Date <- as.Date(OB_main$Date, format = "%m/%d/%Y") 
OB_avg <- cbind.data.frame(OB_main$Date,
                           rowMeans(OB_main[,2:ncol(OB_main)],na.rm=TRUE))
colnames(OB_avg) <- c("Date","Mean")

#---------------------------------------------#

#Tufted apple bud moth 
TA_main <- read.csv('TABM.2.csv')
### Formats the date, Tufted apple bud moth has month.num/day/Year
TA_main$Date <- as.Date(TA_main$Date, format = "%m/%d/%Y") 
TA_avg <- cbind.data.frame(TA_main$Date,
                           rowMeans(TA_main[,2:ncol(TA_main)],na.rm=TRUE))
colnames(TA_avg) <- c("Date","Mean")

#---------------------------------------------#
#Oriental Fruit Moth (Peach) 
of_main.PCH <- read.csv('ofm.2_PCH.csv',na.strings=c("NA","NaN", " ") )
#Oriental Fruit Moth (Apple)
of_main.APP <- read.csv('ofm.2_APP.csv')

###Formats the date, oriental fruit moth has "Weekday, "month day", "Year###

###Formats the date, oriental fruit moth has "Weekday, "month day", "Year###

of_main.PCH$Date <- as.Date(of_main.PCH$Date, format = "%A, %b %d, %Y") 
of_main.APP$Date <- as.Date(of_main.APP$Date, format = "%A, %b %d, %Y") 

of_avg.PCH <- cbind.data.frame(of_main.PCH$Date,
                               rowMeans(of_main.PCH[,2:ncol(of_main.PCH)],na.rm=TRUE))
colnames(of_avg.PCH) <- c("Date","Mean")

of_avg.APP <- cbind.data.frame(of_main.APP$Date,
                               rowMeans(of_main.APP[,2:ncol(of_main.APP)],na.rm=TRUE))
colnames(of_avg.APP) <- c("Date","Mean")

#---------------------------------------------#
#Red banded leafroller 

RB_main <- read.csv('RBLR.2.csv',stringsAsFactors = FALSE)
###Formats the date, oriental fruit moth has "Weekday, "month day", "Year###
RB_main$Date <- as.character(RB_main$Date)

RB_main$Date <- as.Date(RB_main$Date, format = "%m/%d/%Y") 
RB_avg <- cbind.data.frame(RB_main$Date,
                           rowMeans(RB_main[,2:ncol(RB_main)],na.rm=TRUE))
colnames(RB_avg) <- c("Date","Mean")


#---------------------------------------------#
#---------------------------------------------#
#---------------------------------------------#

##############
#TEMPERATURE#
#############

ftocel <- function(x){
  C=(x-32)*(5/9)
  return(C)
}


setwd('C:\\Users\\pak\\Desktop\\NAO_FINAL_2\\Climate_Data')

here()
temp.main.dat<- read.csv("big.temp_week.csv")
temp.main.datC <- temp.main.dat
temp.main.datC<- temp.main.datC[,-8]


temp.main.datC$rain <- as.numeric(temp.main.datC$rain)
temp.main.datC$snow <- as.numeric(temp.main.datC$snow)

temp.main.datC$Date<- as.Date(temp.main.datC$Date, format = "%m/%d/%Y")

###Convert everything from Farenheit to Celcius 
temp.main.datC$tmax<-ftocel(temp.main.datC$tmax)
temp.main.datC$tmin <- ftocel(temp.main.dat$tmin)

#-----------------------------------------------------------#

##################################################################################
###Sets up the degree day based on different base-thresholds                     #
###of the species: (in celcius, L = lower threshold, U = upper threshold)        #          
### CM: L- 10                                                                    #
###     U- 31.1                                                                  #
### OB: L- 6.1                                                                   #
###     U - 32.7                                                                 #
### TA: L- 7.2                                                                   #
###     U - 32.7                                                                 #
### OF: L- 7.2                                                                   # 
### OF: U - 32.2                                                                 #
### RB: L - 6.1                                                                  #
###     U- 32.7                                                                  #
##################################################################################
##################################################################################
###Takes the minimum temperature and the maximum temperature###
###############################################################

dd.43<-  function(x){
  
  tmp<- x
  tmp$tmax[tmp$tmax > 32.7] <- 32.7
  dd.r=(((x$tmax+x$tmin)/2) - 6.1) 
  return(dd.r)
}


###Tufted Apple bud moth and Oriental Fruit MOth 
dd.45 <- function(x, species){
  if (species == 'TA'){
    tmp<- x
    tmp$tmax[tmp$tmax > 32.7] <- 32.7
    dd.r=(((x$tmax+x$tmin)/2) - 7.2)} 
  if (species == 'OF'){
    tmp <- x
    tmp$tmax[tmp$tmax > 32.2] <- 32.2
    dd.r = ((x$tmax+ x$tmin/2)-7.2 )
  }
  return(dd.r)
}

dd.50<- function(x){
  tmp<- x
  tmp$tmax[tmp$tmax > 31.1111] <- 31.1111
  dd.r=(((tmp$tmax+tmp$tmin)/2) - 10) 
  return(dd.r)}

#-------------------------------------------------#
phi.43 <- data.frame(dd.43(temp.main.datC))
phi.43[phi.43<0]<-0

phi.45.ta<- data.frame(dd.45(temp.main.datC,'TA' ))
phi.45.ta[phi.45.ta<0] <- 0

phi.45.of <- data.frame(dd.45(temp.main.datC,'OF'))
phi.45.of[phi.45.of<0] <- 0

phi.50 <- data.frame(dd.50(temp.main.datC))
phi.50[phi.50<0] <-0

##########################################################
##########################################################


##################################################
####Put everything back in ##
##################################################
phi.43.main <- cbind(temp.main.datC,phi.43)
phi.45.main.ta<- cbind(temp.main.datC,phi.45.ta)
phi.45.main.of <- cbind(temp.main.datC, phi.45.of)
phi.50.main <-cbind(temp.main.datC,phi.50)
#########################
####Renamed everything###
#########################
colnames(phi.43.main)[[8]]="phi"
colnames(phi.45.main.ta)[[8]]= "phi"
colnames(phi.45.main.of)[[8]]= "phi"
colnames(phi.50.main)[[8]]= "phi"

#############################
###Accumulated the phi days##
############################
phi.43.main$phi.sc = cumsum(phi.43.main$phi)
phi.45.main.ta$phi.sc = cumsum(phi.45.main.ta$phi)
phi.45.main.of$phi.sc = cumsum(phi.45.main.of$phi)
phi.50.main$phi.sc = cumsum(phi.50.main$phi)
#########################################
###Added the Years and Months as factors#
#########################################

phi.43.main$month = as.numeric(format(phi.43.main$Date, "%m"))
phi.43.main$year = as.numeric(format(phi.43.main$Date, "%Y"))


phi.45.main.of$month = as.numeric(format(phi.45.main.of$Date, "%m"))
phi.45.main.of$year = as.numeric(format(phi.45.main.of$Date, "%Y"))

phi.45.main.ta$month = as.numeric(format(phi.45.main.ta$Date, "%m"))
phi.45.main.ta$year = as.numeric(format(phi.45.main.ta$Date, "%Y"))

phi.50.main$month = as.numeric(format(phi.50.main$Date, "%m"))
phi.50.main$year = as.numeric(format(phi.50.main$Date, "%Y"))


##############################
###ALTERING the temperature###
##############################
###############################################################
###So instead of accumulating degree days from 1981 to 2015...#
###Accumulate degree days only in each year                   #
###############################################################
#Making a copy of the original temp.main.dat
temp.main.dat2 <- temp.main.datC

#Adds a year and month
temp.main.dat2$year = as.numeric(format(temp.main.dat2$Date, "%Y"))
temp.main.dat2$month = as.numeric(format(temp.main.dat2$Date,"%m"))

#Average temperature
temp.main.dat2$avg <- (temp.main.dat2$tmin + temp.main.dat2$tmax)/2

temp.main.dat2 <- subset(temp.main.dat2, temp.main.dat2$year != 1980)

#Split based on temperature
split.temp2 <- split(temp.main.dat2, temp.main.dat2$year)


#################
#PHI- 43#########
#################

phi.43.year=NULL

for (i in seq(1,36)){
  tmp = split.temp2[[i]]
  tmp$tmax[tmp$tmax > 32.7] <- 32.7
  tmp$phi <-(tmp$tmax+tmp$tmin)/2 - 6.1
  tmp$phi[tmp$phi<0]<-0
  tmp$phi.cum <- cumsum(tmp$phi)
  
  phi.43.year[[i]] = tmp
}
phi.43.year2 <- do.call(rbind, phi.43.year)
phi.43.year3 <- phi.43.year2[,c(1,12)]

###############
#PHI-45########
###############
phi.45.year =NULL
for (i in seq(1,36)){
  tmp = split.temp2[[i]]
  tmp$tmax[tmp$tmax > 32.7] <- 32.7
  tmp$phi <-(tmp$tmax+tmp$tmin)/2 - 7.2
  tmp$phi[tmp$phi<0]<-0
  tmp$phi.cum <- cumsum(tmp$phi)
  
  phi.45.year[[i]] = tmp
}
phi.45.year2 <- do.call(rbind, phi.45.year)
phi.45.year3 <- phi.45.year2[,c(1,12)]

#######################
#PHI-45- for OF########
#######################
phi.45.year.of =NULL
for (i in seq(1,36)){
  tmp = split.temp2[[i]]
  tmp$tmax[tmp$tmax > 32.2] <- 32.2
  tmp$phi <-(tmp$tmax+tmp$tmin)/2 - 7.2
  tmp$phi[tmp$phi<0]<-0
  tmp$phi.cum <- cumsum(tmp$phi)
  
  phi.45.year.of[[i]] = tmp
}
phi.45.year2.of <- do.call(rbind, phi.45.year.of)
phi.45.year3.of <- phi.45.year2.of[,c(1,12)]



#PHI-50
phi.50.year =NULL
for (i in seq(1,36)){
  tmp = split.temp2[[i]]
  tmp$tmax[tmp$tmax > 31.1111] <- 31.1111
  tmp$phi <-(tmp$tmax+tmp$tmin)/2 - 10
  tmp$phi[tmp$phi<0]<-0
  tmp$phi.cum <- cumsum(tmp$phi)
  
  phi.50.year[[i]] = tmp
}
phi.50.year2 <- do.call(rbind, phi.50.year)
phi.50.year3 <- phi.50.year2[,c(1,12)]

################################################
###Merging temperature and phermone trap data ##
###############################################
cm.phi<-merge(phi.50.main,CM_avg, by = "Date", all = T)
cm.phi <- na.omit(cm.phi)

ob.phi <- merge(phi.43.main, OB_avg, by= "Date",all =T)
ob.phi <- na.omit(ob.phi)

tabm.phi<-merge(phi.45.main.ta,TA_avg, by = "Date", all = T)
tabm.phi <- na.omit(tabm.phi)

ofm.phi.PCH<-merge(phi.45.main.of,of_avg.PCH, by = "Date",all=TRUE)
ofm.phi.PCH <- na.omit(ofm.phi.PCH)

ofm.phi.APP<-merge(phi.45.main.of,of_avg.APP, by = "Date", all = T)
ofm.phi.APP <- na.omit(ofm.phi.APP)

rblr.phi<-merge(phi.43.main,RB_avg, by = "Date", all = T)
rblr.phi <- na.omit(rblr.phi)

################################################################
################################################################
cm.phi.new <- merge(cm.phi, phi.50.year3, by= 'Date')        ###
##############################################################
##############################################################
ob.phi.new <- merge(ob.phi, phi.43.year3, by='Date')         #   
##############################################################
##############################################################
ta.phi.new <- merge(tabm.phi, phi.45.year3,by='Date')        #
##############################################################
###Oriental Fruit Moth- Peach ###################################
of.phi.new.PCH <- merge(ofm.phi.PCH, phi.45.year3.of, by= 'Date')#
##################################################################
###Oriental Fruit Moth- Apple ####################################
of.phi.new.APP <- merge(ofm.phi.APP, phi.45.year3.of, by='Date')##
##################################################################
##################################################################
###Redbanded leafroller ##########################################
##################################################################
rb.phi.new <- merge(rblr.phi, phi.43.year3,by='Date')
##############RUN THE ANALYSES HERE################################
###################################################################
