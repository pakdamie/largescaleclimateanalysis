require(lubridate)
require(pls)
require(plsVarSel)
require(ggplot2)
require(scales)
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

ggplot(mvrmCM_VIP, aes(x= Month,y= VIP, fill = greater))+
  geom_bar(stat= 'identity')+geom_hline(yintercept = 1)+
  scale_fill_manual(values = c('grey','red'))+theme_bw()  






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

###ACTUAL ANALYSIS OF CODLING MOTH (Julian day)
###
CM_PLS <- read.csv("CM_MANUAL_JULIAN_PLS.csv",stringsAsFactors = FALSE)


###SMOOTHING IT OUT FOR EACH YEAR - EXAMPLE
julian_day <-(c(244:365,1:120 ))

JULIAN_DAY_TEMP_CM <- cbind.data.frame(colnames(CM_PLS[,3:244]),
                                 t(CM_PLS[1,3:244]),
                                as.vector(smooth(
                                   as.vector(t(CM_PLS[1,3:244])),
                                   twiceit=TRUE)))
rownames(JULIAN_DAY_TEMP_CM) <- seq(1, length(julian_day))
colnames(JULIAN_DAY_TEMP_CM)<-c('JD','tmin','smooth')


ggplot(JULIAN_DAY_TEMP_CM , 
       aes(x= JD, y= tmin,group=1))+
  geom_point()+
  geom_line()+
  geom_point(aes(x= JD,y=smooth),col='red')+
  geom_line(aes(x= JD,y=smooth),col='red')

###SMOOTHING 

Smoothed_df <-matrix(NA, nrow = 33, ncol = 242)

for (i in seq(1,33)){
  temp <- CM_PLS[i,]
  Smoothed_df[i,]<-  as.vector(smooth(
    as.vector(t(temp[,3:244])), twiceit=TRUE))
  
}

SMOOTH_CM_data<-  cbind.data.frame(seq(1984,2016),
                                   CM.first.spring$julian,
                                   Smoothed_df)
colnames(SMOOTH_CM_data)<- c("Year","Julian",
                             colnames(CM_PLS[,3:244]))


CM_month <- plsr(Julian~., data = SMOOTH_CM_data[,2:244],
                 validation = "CV",scale=TRUE,
                 method="oscorespls")
summary(CM_month)

VIP_CM_SMOOTH<-data.frame(julian_day,VIP(CM_month,opt.comp=1))

VIP_CM_SMOOTH$realdate <- (seq(as.Date("1984/9/1"),
                               as.Date("1985/4/30"), "days"))
colnames(VIP_CM_SMOOTH) <- c('julian','vip','realdate')
VIP_CM_SMOOTH$great <- ifelse(VIP_CM_SMOOTH$vip >= 1, TRUE,FALSE)

ggplot(VIP_CM_SMOOTH, 
  aes(x = realdate, y= vip, fill = great))+geom_bar(stat='identity',
  color='black')+
  theme(axis.text.x = element_text(angle = 45, size = 9))+
  theme(axis.ticks.length=unit(0.5,"cm"))+
  scale_x_date(date_breaks = "2 week", date_labels =  "%b %d") +
  geom_hline(yintercept=1)+
  scale_fill_manual(values = c('grey','red'))+theme_bw()

mvrCM<- mvr(Julian~., data = CM_PLS[,2:244],validation='CV',scale=TRUE,method="oscorespls")
summary(mvrCM)
mvrCMVIP<-VIP(mvrCM,opt.comp=3, p= 242)


mvrVIP_CM_SMOOTH<-data.frame(
  julian_day,mvrCMVIP)

mvrVIP_CM_SMOOTH$realdate <- (seq(as.Date("1984/9/1"),
                               as.Date("1985/4/30"), "days"))
colnames(mvrVIP_CM_SMOOTH) <- c('julian','vip','realdate')
mvrVIP_CM_SMOOTH$great <- 
  ifelse(mvrVIP_CM_SMOOTH$vip >= 1, TRUE,FALSE)

ggplot(mvrVIP_CM_SMOOTH, 
       aes(x = realdate, y= vip, fill = great))+geom_bar(stat='identity',
                                                         color='black')+
  theme(axis.text.x = element_text(angle = 45, size = 9))+
  theme(axis.ticks.length=unit(0.5,"cm"))+
  scale_x_date(date_breaks = "2 week", date_labels =  "%b %d") +
  geom_hline(yintercept=1)+
  scale_fill_manual(values = c('grey','red'))+theme_bw()


RC_CM_SMOOTH<-data.frame(julian_day,
                         RC(mvrCM, opt.comp=1))

RC_CM_SMOOTH$realdate <- (seq(as.Date("1984/9/1"),
                               as.Date("1985/4/30"), "days"))

colnames(RC_CM_SMOOTH)<- c('Julian','RC','Date')


ggplot(RC_CM_SMOOTH, 
  aes(x = Date, y= RC))+
  geom_point()+geom_line()+
  theme(axis.text.x = element_text(angle = 45, size = 9))+
  scale_x_date(date_breaks = "2 week", date_labels =  "%b %d")+
  geom_hline(yintercept =0)+
  theme_bw()

