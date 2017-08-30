##############################
#Preparing data for analysis#
#############################
#library(here)
library(ggplot2)
library(reshape2)
library(rootSolve)
require(zoo)
require(corrplot)

#Run this first before continuing on to the analysis 
#setwd('C:\\Users\\pak\\Desktop\\NAO_FINAL_2\\Species_Data')

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

