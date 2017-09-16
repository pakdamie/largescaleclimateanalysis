######################
#SPRING FLIGHT GRAPHS#
######################
#packages

library(ggplot2)
library(gridExtra)


#1) THIS INCLUDES THE GRAPHING OF THE ANNUAL SPRING FLIGHT OF ALL FIVE SPECIES

###GENERAL CODE
theme_springflight <- 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        plot.title=element_text( size=12, vjust=0),
        axis.text.x=element_text(size=12, angle=90,vjust=0.5, hjust=1),
       axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=12, vjust=0),
       axis.title.y=element_text(size=12, vjust=0))
 
#bitch to work with theme_publication()
#---------------------------------------------------------------------------#


#Codling moth 

CM_FLIGHT <- ggplot(CM.first.spring, aes(x= year, y= julian))+
  geom_point(size=1.5)+geom_line()+theme_springflight+
  labs(x= "Year", y= "Julian day")+
  labs(title = "Cydia pomonella")+scale_x_continuous(breaks=seq(1984,2016,4),
                                                    labels = seq(1984,2016,4))+
  ylim(70,170); CM_FLIGHT
#Oblique banded leafroller


OB_FLIGHT<-ggplot(ob.first.spring, aes(x= year, y= julian))+
  geom_point(size=1.5)+geom_line()+theme_springflight+
  labs(x= "Year", y= "Julian day")+
  labs(title = "Choristoneura rosaceana")+
  scale_x_continuous(breaks =seq(1981,2016,by=5))+
  ylim(70,170)
OB_FLIGHT

TA_FLIGHT <-  ggplot(ta.first.spring, aes(x= year, y= julian))+
  geom_point(size=1.5)+geom_line()+theme_springflight+
  labs(x= "Year", y= "Julian day")+
  labs(title = "Platynota idaeusalis")+
  scale_x_continuous(breaks =seq(1981,2016,by=5))+
  ylim(70,170)
TA_FLIGHT

#--------------------------------------------------#
#############################
#Oriental fruit moth - Peach#
#############################

#OFMP_FLIGHT<-ggplot(na.omit(ofm.first.spring_PCH), aes(x= year, y= julian ))+
 # geom_point(size=2)+geom_line()+theme_springflight+
#  labs(x= "Year", y= "Julian day")+
#  labs(title = "Oriental Fruit Moth- Peach")+scale_x_continuous(breaks=seq(1999,2016,2))
#OFMP_FLIGHT

#OFMA_FLIGHT<-ggplot(na.omit(ofm.first.spring_APP), aes(x= year, y= julian ))+
 # geom_point(size=2)+geom_line()+ 
#  labs(x= "Year", y= "Julian day")+theme_springflight+
#  labs(title = "Oriental Fruit Moth- Apple")+scale_x_continuous(breaks=seq(1981,2016,4))
#OFMA_FLIGHT

###AGGREGATED

of_PCH <- ofm.first.spring_PCH[,c(3,5)]
of_PCH$ id <- 'Peach'
of_APP <- ofm.first.spring_APP[,c(3,5)]
of_APP$id <- 'Apple'

of_ALL <- rbind(of_PCH, of_APP)

######################
#Oriental fruit moth#
#####################

OFM_FLIGHT<-ggplot(of_ALL, aes(x= year, y= julian, group =id))+  
  geom_point(size=1.5)+geom_line(aes(linetype=id))+theme_springflight+
  labs(x= "Year", y= "Julian day")+labs(title = "Grapholita molesta")+
    scale_x_continuous(breaks =seq(1981,2016,by=5))+
  ylim(70,170)
OFM_FLIGHT

#######################
#Red banded leafroller#
#######################
RB_FLIGHT <-ggplot(rb.first.spring, aes(x= year, y= julian ))+
  geom_point(size=1.5)+geom_line()+
  labs(x= "Year", y= "Julian day")+theme_springflight+
  labs(title = "Argyrotaenia velutina")+scale_x_continuous(breaks=seq(1981,2016,5))+
  ylim(70,170)
RB_FLIGHT

grid.arrange(CM_FLIGHT, OB_FLIGHT, TA_FLIGHT,OFM_FLIGHT, RB_FLIGHT,ncol=3 )

###ANOTHER WAY TO ARRANGE: BI VERSUS MULTI?

###########################################
#############################################