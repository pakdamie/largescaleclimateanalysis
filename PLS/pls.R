require(lubridate)
x = as.Date('2010-06-10')
yday(x)


#####################################
###Partial least squares analysis###
#####################################

####Monthly resolution


CM_CAST_ALL

CM_month <- plsr(Julian~., data = CM_CAST_ALL[,2:10],validation = "LOO",scale=TRUE,
                 method="oscorespls")
summary(CM_month)

test <- VIP(CM_month)
 barplot(colMeans(test),type='l')
 abline(h= 0.8)



plot(CM_month, plottype='coef',ncomp= 1:3 ,legendpos = 'bottomleft',
     lbaels='numbers',xlab='nm')

oscorespls.fit(as.matrix(CM_CAST_ALL[,1]), as.matrix(CM_CAST_ALL[,2:10]),
               ncomp=8)

###Daily resolution - only looking at  September to April

CM_TEMP_Daily <- subset(temp.main.dat2, year >1982 & year < 2017)
row.names(CM_TEMP_Daily)<- seq(1, nrow(CM_TEMP_Daily))
CM_TEMP_Daily <- CM_TEMP_Daily[-(1:243),]
row.names(CM_TEMP_Daily)<- seq(1, nrow(CM_TEMP_Daily))
CM_TEMP_Daily <- CM_TEMP_Daily[-(11931:12175),]

CM_TEMP_Daily <- subset(CM_TEMP_Daily, CM_TEMP_Daily$month != 5 &
                          CM_TEMP_Daily$month != 6 &
                          CM_TEMP_Daily$month != 7&
                          CM_TEMP_Daily$month != 8)



###Gettitng rid of 5,6,7,8

CM_TEMP_Daily$Julian <- yday(CM_TEMP_Daily$Date)




CM_TEMP_DAILY_SUB <- CM_TEMP_Daily[,c(8,11,5)]

CM_CAST<-dcast(CM_TEMP_DAILY_SUB,year~ Julian)



CM_CAST<-CM_CAST[,-367]

CM_CAST_ALL <- cbind(CM_CAST_ALL[,1], CM_CAST)

CM.first.spring$julian

###
CM_PLS <- read.csv("CM_MANUAL_JULIAN_PLS.csv",stringsAsFactors = FALSE)

CM_month <- plsr(Julian~., data = CM_PLS[,2:244],validation = "LOO",scale=TRUE,
                 method="oscorespls")
summary(CM_month)

a<-VIP(CM_month)
b<- data.frame(colnames(a),colMeans(a))

b$realdate <- format(seq(as.Date("1984/9/1"), as.Date("1985/4/30"), "days"),'%m-%d')
colnames(b) <- c('julian','vip','realdate')
b$great <- ifelse(b$vip >= 0.8, TRUE,FALSE)
ggplot(b, aes(x = realdate, y= vip, fill = great))+geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle = 45, size = 9))
  
class(b$realdate)

