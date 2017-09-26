require(lubridate)
require(pls)
require(plsVarSel)
require(ggplot2)
require(scales)
library(gridExtra)
library(matrixStats)

#####################################
###Partial least squares analysis###
#####################################
##################################
####Monthly temperatures##########
##################################

M_PLS_CM <- subset(temp.main.dat2, temp.main.dat2$year > 1982 &
                     temp.main.dat2$year < 2017)
rownames(M_PLS_CM)<- seq(1, nrow(M_PLS_CM))

M_PLS_CM <- M_PLS_CM[-c(1:243),]
rownames(M_PLS_CM)<- seq(1, nrow(M_PLS_CM))

M_PLS_CM <- M_PLS_CM[-c(11931:12175),]
CM_TEMP_M <- subset(M_PLS_CM, M_PLS_CM$month != 5 &
                      M_PLS_CM$month != 6 &
                      M_PLS_CM$month != 7&
                      M_PLS_CM$month != 8)

CM_TEMP_M_agg <- aggregate(CM_TEMP_M$tmin, by = list(CM_TEMP_M$month,
                                                     CM_TEMP_M$year),
                           'mean')


CAST_CM_TEMP<-dcast(CM_TEMP_M_agg ,
                          Group.2~ Group.1)

CM_WINT<-na.omit(CAST_CM_TEMP[,6:9])
CM_SPRING<- na.omit(CAST_CM_TEMP[,2:5])

CM_ALL_M_CAST<-cbind(CM.first.spring$julian,
              seq(1984, 2016), CM_WINT,CM_SPRING)
colnames(CM_ALL_M_CAST)[1:2] <- c("Julian","Year")

mvr_mCM<- mvr(Julian~., data =CM_ALL_M_CAST[,c(1, 3:10)],validation='CV',scale=TRUE,method="oscorespls")
summary(mvr_mCM)
mvrmCM_VIP<-cbind.data.frame(c("Sep","Oct","Nov","Dec",
                               "Jan","Feb","Mar","Apr"),
                            VIP(mvr_mCM,opt.comp=1))
colnames(mvrmCM_VIP) <- c('Month','VIP')
mvrmCM_VIP$greater <- ifelse(mvrmCM_VIP$VIP > 1, TRUE, FALSE)

mvrmCM_VIP$Month <- as.character(mvrmCM_VIP$Month)
mvrmCM_VIP$Month <- factor(mvrmCM_VIP$Month, 
                           levels = unique(mvrmCM_VIP$Month))

M_VIP_CM<-ggplot(mvrmCM_VIP, aes(x= Month,y= VIP, fill = greater))+
  geom_bar(stat= 'identity')+geom_hline(yintercept = 1)+
  scale_fill_manual(values = c('gray84','seagreen4'))+
  guides(fill=FALSE)+theme_Publication()+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Cydia Pomonella")
####################################################
###Regression coefficent for monthly temperatures###
####################################################

RC_CM_M<-cbind.data.frame(mvrmCM_VIP$Month,
                         RC(mvr_mCM, opt.comp=1))
colnames(RC_CM_M)<- c("Month","RC")

plot(RC_CM_M$Month, RC_CM_M$RC)

RC_VIP_CM_M <- cbind.data.frame(mvrmCM_VIP[,2:3], RC_CM_M)

RC_VIP_CM_M$negopos<- 'insig'
RC_VIP_CM_M$negopos[RC_VIP_CM_M$greater == TRUE & RC_VIP_CM_M$RC > 0 ]<- 'POS'
RC_VIP_CM_M$negopos[RC_VIP_CM_M$greater == TRUE & 
                      RC_VIP_CM_M$RC < 0 ]<- 'NEG'

M_RC_CM <- ggplot(RC_VIP_CM_M, aes(x= Month, y= RC))+ 
  geom_bar(aes(fill = negopos),stat='identity')+
  scale_fill_manual(values = c('gray84','dodgerblue4','firebrick3'))+
  theme_Publication()+
  geom_hline(yintercept = 0)+
  guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

grid.arrange(M_VIP_CM, M_RC_CM,ncol=1)


###Third graph showing the average temperature with the standard error
###bar

colMeans(CM_ALL_M_CAST)[3:10]

colSds(as.matrix(CM_ALL_M_CAST))[3:10]

nrow(CM_ALL_M_CAST) #33

se_CM_month <- colSds(as.matrix(CM_ALL_M_CAST))[3:10]/sqrt(33)

se_month_CM <- cbind.data.frame(c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr"),
                     colMeans(CM_ALL_M_CAST)[3:10],
                     
                     se_CM_month)
colnames(se_month_CM)<- c("Month","Mean","SE")
se_month_CM$Lower <- se_month_CM$Mean - se_month_CM$SE
se_month_CM$Upper <- se_month_CM$Mean + se_month_CM$SE
se_month_CM$Important <- 0
se_month_CM$Important <- as.factor(se_month_CM$Important)

se_month_CM$Month <- factor(se_month_CM$Month,
                           levels = unique(se_month_CM$Month))


Monthly_mean_1<-ggplot(se_month_CM, aes(x= as.factor(Month), y= Mean,
                       group=Month))+
         geom_bar(stat='identity',aes(fill=Important))+
  geom_errorbar(aes(ymin=Lower, ymax=Upper, width=0.2))+
  scale_fill_manual(values = c('gray84','dodgerblue4','firebrick3'))+
  theme_bw()+theme_Publication()+ xlab("Month") +
  guides(fill=FALSE)
  

###Boxplot- version 2
CM_ALL_M_CAST
colnames(CM_ALL_M_CAST)[3:10]<- c("Sep","Oct","Nov",
                         "Dec","Jan","Feb","Mar","Apr")
CM_ALL_M_MELT <- melt(CM_ALL_M_CAST[,3:10])


colnames(CM_ALL_M_MELT) <- c('Month',"Tmin")

CM_ALL_M_MELT $Month <- factor(CM_ALL_M_MELT $Month,
                            levels = unique(CM_ALL_M_MELT $Month))


CM_ALL_M_MELT$Important = 0
CM_ALL_M_MELT$Important[CM_ALL_M_MELT$Month =="Mar"]=1
CM_ALL_M_MELT$Important[CM_ALL_M_MELT$Month =="Sep"]=2

colnames(CM_ALL_M_MELT) <- c('Month',"Tmin",'Important')


M_TEMP_CM<-ggplot(CM_ALL_M_MELT, aes(x= Month, y= Tmin))+
  geom_boxplot(aes(fill=as.factor(Important)))+
  scale_fill_manual(values = c('gray84','dodgerblue4','firebrick3'))+
  theme_bw()+theme_Publication()+ xlab("Month") +
  guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



grid.arrange(M_VIP_CM, M_RC_CM, M_TEMP_CM,ncol=1)











########################
###Daily temperatures###
########################
###########################################################
###Daily resolution - only looking at  September to April##
###########################################################

#Editing data 

CM_TEMP_Daily <- subset(temp.main.dat2, year >1982 & year < 2017)
row.names(CM_TEMP_Daily)<- seq(1, nrow(CM_TEMP_Daily))
CM_TEMP_Daily <- CM_TEMP_Daily[-(1:243),]
row.names(CM_TEMP_Daily)<- seq(1, nrow(CM_TEMP_Daily))
CM_TEMP_Daily <- CM_TEMP_Daily[-(11931:12175),]




###Gettitng rid of 5,6,7,8
#Getting rid of May to August
CM_TEMP_Daily <- subset(CM_TEMP_Daily, CM_TEMP_Daily$month != 5 &
                          CM_TEMP_Daily$month != 6 &
                          CM_TEMP_Daily$month != 7&
                          CM_TEMP_Daily$month != 8)

#Figure out the julian day
CM_TEMP_Daily$Julian <- yday(CM_TEMP_Daily$Date)


#Only looking at tmin
CM_TEMP_DAILY_SUB <- CM_TEMP_Daily[,c(8,11,5)]

CM_CAST<-dcast(CM_TEMP_DAILY_SUB,year~ Julian)

CM_CAST<-CM_CAST[,-367]

CM_CAST_ALL <- cbind(CM_CAST_ALL[,1], CM_CAST)

CM.first.spring$julian



















###WEEKLY
###Only looking at tmin
temp.dat.tmin <- temp.main.dat2[, c(1,5,8,9)]

temp.dat_W_CM <- subset(temp.dat.tmin, temp.dat.tmin$year > 1982 &
                          temp.dat.tmin$year < 2017)

temp.dat_W_CM$Julian <- yday(temp.dat_W_CM$Date)
###The leap years between 1983 - 2016
###1984, 1988, 1992, 1996, 2000,2004, 2008,2012,2016


LEAP_Y <- seq(1984, 2016, by = 4)
no_months <- seq(5,8, by =1)
LEAP_YEAR_temp_W_CM <-subset(temp.dat_W_CM ,temp.dat_W_CM $year %in% LEAP_Y)
LEAP_YEAR_temp_W_CM <- subset(LEAP_YEAR_temp_W_CM,LEAP_YEAR_temp_W_CM$Julian != 60)
LEAP_YEAR_temp_W_CM <- subset(LEAP_YEAR_temp_W_CM,!(
                              LEAP_YEAR_temp_W_CM$month %in% no_months ))



NONLEAP_temp_W_CM <- subset(temp.dat_W_CM, !(temp.dat_W_CM$year %in% LEAP_Y))
NONLEAP_temp_W_CM <- subset(NONLEAP_temp_W_CM,!(
                                NONLEAP_temp_W_CM$month %in% no_months))

temp_W_CM_final <- rbind(NONLEAP_temp_W_CM , LEAP_YEAR_temp_W_CM)
       

temp_W_CM_final <- temp_W_CM_final[order(temp_W_CM_final$Date),] 
rownames(temp_W_CM_final) <- seq(1,8228)
temp_W_CM_final <- temp_W_CM_final[-c(1:100), ]
rownames(temp_W_CM_final) <- seq(1,nrow(temp_W_CM_final))
temp_W_CM_final <- temp_W_CM_final[-c(8007: 8128),]

#Dear jesus, i'm so sorry oh my god this is terrible code

mavg.7day <- SMA(temp_W_CM_final$tmin, n=7)  # Simple moving average
mavg.11day <- SMA(temp_W_CM_final$tmin, n = 11) #Simple moving average
mavg.15day <- SMA(temp_W_CM_final$tmin, n = 15) #Simple moving average

CM_mavg_7<-cbind.data.frame(temp_W_CM_final$Date,mavg.7day)
CM_mavg_11 <- cbind.data.frame(temp_W_CM_final$Date, mavg.11day)
CM_mavg_15<- cbind.data.frame(temp_W_CM_final$Date, mavg.15day)

CM_mavg_7<- CM_mavg_7[-c(1:20),]
CM_mavg_11<- CM_mavg_11[-c(1:20),]
CM_mavg_15 <- CM_mavg_15[-c(1:20),]
plot(as.Date(CM_mavg_7[1:242,1],format="%Y-%m-%d"),
     CM_mavg_7[1:242,2],type='l')
lines(as.Date(CM_mavg_7[1:242,1],format="%Y-%m-%d"),CM_mavg_11[1:242,2],type='l',col='red')
lines(as.Date(CM_mavg_7[1:242,1],format="%Y-%m-%d"),CM_mavg_15[1:242,2],type='l',col='blue')

CM_mavg_7$id <- rep(seq(1,242),33)
CM_mavg_7$year <- as.numeric(format(CM_mavg_7$`temp_W_CM_final$Date`, '%Y'))
colnames(CM_mavg_7)<- c("Date","Tmin","ID","Year")


WEEK_CM_7_CAST <- dcast(CM_mavg_7,Year~ID)

CM_mavg_7_2 <- CM_mavg_7[,c(4,3,2)]

SPREADED_CM_1<-spread(CM_mavg_7_2, ID, Tmin)
SPREADED_CM_a <- na.omit(SPREADED_CM_1[,2:123])
SPREADED_CM_b <- na.omit(SPREADED_CM_1[,124:243])

SPREADED_CM_FINAL <- cbind.data.frame(seq(1984,2016),CM.first.spring$julian,
                                      SPREADED_CM_a, SPREADED_CM_b)
colnames(SPREADED_CM_FINAL) <- c("Year","Julian",as.Date(Date_CM))

Date_CM<- (seq(as.Date("1984/9/1"),
                                as.Date("1985/4/30"), "days"))


mvr_SMOOTHED_7_CM<- mvr(Julian~., data =SPREADED_CM_FINAL[,c(2, 3:244)],
                      validation='CV',scale=TRUE,method="oscorespls")
summary(mvr_SMOOTHED_7_CM)

###MAKING THE VIP GRAPH

mvrmCM_VIP_SMOOTHED_7<-cbind.data.frame(c(Date_CM),
                             VIP(mvr_SMOOTHED_7_CM,opt.comp=1))
colnames(mvrmCM_VIP_SMOOTHED_7) <- c('Days','VIP')

mvrmCM_VIP_SMOOTHED_7$greater <- ifelse(mvrmCM_VIP_SMOOTHED_7$VIP >0.8, TRUE, FALSE)

D7_VIP_CM<-ggplot(mvrmCM_VIP_SMOOTHED_7, aes(x= Days,y= VIP, fill = greater))+
  geom_bar(stat= 'identity')+geom_hline(yintercept = 1)+
  scale_fill_manual(values = c('gray84','seagreen4'))+
  guides(fill=FALSE)+theme_Publication()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Cydia Pomonella")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_date(date_breaks = "1 week")
###Regression coefficent 

RC_CM_D7<-cbind.data.frame(mvrmCM_VIP_SMOOTHED_7$Day,
                          RC(mvr_SMOOTHED_7_CM, opt.comp=1))
colnames(RC_CM_D7)<- c("Day","RC")

barplot(RC_CM_D7$RC)

RC_VIP_CM_D7<- cbind.data.frame(mvrmCM_VIP_SMOOTHED_7[,2:3], RC_CM_D7)

RC_VIP_CM_D7$negopos<- 'insig'
RC_VIP_CM_D7$negopos[RC_VIP_CM_D7$greater == TRUE & RC_VIP_CM_D7$RC > 0 ]<- 'POS'
RC_VIP_CM_D7$negopos[RC_VIP_CM_D7$greater == TRUE & 
                       RC_VIP_CM_D7$RC < 0 ]<- 'NEG'

D7_RC_CM <- ggplot(RC_VIP_CM_D7, aes(x= Day, y= RC))+ 
  geom_bar(aes(fill = negopos),stat='identity')+
  scale_fill_manual(values = c('gray84','dodgerblue4','firebrick3'))+
  theme_Publication()+
  geom_hline(yintercept = 0)+
  guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_date(date_breaks = "1 week")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

grid.arrange(D7_VIP_CM, D7_RC_CM,ncol=1)

#######################################
########################################
####Try smoothing out by 11


CM_mavg_11$id <- rep(seq(1,242),33)
CM_mavg_11$year <- as.numeric(format(CM_mavg_11$`temp_W_CM_final$Date`, '%Y'))
colnames(CM_mavg_11)<- c("Date","Tmin","ID","Year")


WEEK_CM_11_CAST <- dcast(CM_mavg_11,Year~ID)

CM_mavg_11_2 <- CM_mavg_11[,c(4,3,2)]

SPREADED_CM11_1<-spread(CM_mavg_11_2, ID, Tmin)
SPREADED_CM11_a <- na.omit(SPREADED_CM11_1[,2:123])
SPREADED_CM11_b <- na.omit(SPREADED_CM11_1[,124:243])

SPREADED_CM11_FINAL <- cbind.data.frame(seq(1984,2016),CM.first.spring$julian,
                                      SPREADED_CM11_a, SPREADED_CM11_b)
colnames(SPREADED_CM11_FINAL) <- c("Year","Julian",as.Date(Date_CM))

Date_CM<- (seq(as.Date("1984/9/1"),
               as.Date("1985/4/30"), "days"))


mvr_SMOOTHED_11_CM<- mvr(Julian~., data =SPREADED_CM11_FINAL[,c(2, 3:244)],
                        validation='CV',scale=TRUE,method="oscorespls")
summary(mvr_SMOOTHED_11_CM)

###MAKING THE VIP GRAPH

mvrmCM_VIP_SMOOTHED_11<-cbind.data.frame(c(Date_CM),
                                        VIP(mvr_SMOOTHED_11_CM,opt.comp=1))
colnames(mvrmCM_VIP_SMOOTHED_11) <- c('Days','VIP')

mvrmCM_VIP_SMOOTHED_11$greater <- ifelse(mvrmCM_VIP_SMOOTHED_11$VIP >1, TRUE, FALSE)

D11_VIP_CM<-ggplot(mvrmCM_VIP_SMOOTHED_11, aes(x= Days,y= VIP, fill = greater))+
  geom_bar(stat= 'identity')+geom_hline(yintercept = 1)+
  scale_fill_manual(values = c('gray84','seagreen4'))+
  guides(fill=FALSE)+theme_Publication()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Cydia Pomonella")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_date(date_breaks = "1 week")
###Regression coefficent 

RC_CM_D7<-cbind.data.frame(mvrmCM_VIP_SMOOTHED_7$Day,
                           RC(mvr_SMOOTHED_7_CM, opt.comp=1))
colnames(RC_CM_D7)<- c("Day","RC")

barplot(RC_CM_D7$RC)

RC_VIP_CM_D7<- cbind.data.frame(mvrmCM_VIP_SMOOTHED_7[,2:3], RC_CM_D7)

RC_VIP_CM_D7$negopos<- 'insig'
RC_VIP_CM_D7$negopos[RC_VIP_CM_D7$greater == TRUE & RC_VIP_CM_D7$RC > 0 ]<- 'POS'
RC_VIP_CM_D7$negopos[RC_VIP_CM_D7$greater == TRUE & 
                       RC_VIP_CM_D7$RC < 0 ]<- 'NEG'

D7_RC_CM <- ggplot(RC_VIP_CM_D7, aes(x= Day, y= RC))+ 
  geom_bar(aes(fill = negopos),stat='identity')+
  scale_fill_manual(values = c('gray84','dodgerblue4','firebrick3'))+
  theme_Publication()+
  geom_hline(yintercept = 0)+
  guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_date(date_breaks = "1 week")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


grid.arrange(D_VIP_CM, D7_RC_CM,ncol=1)

