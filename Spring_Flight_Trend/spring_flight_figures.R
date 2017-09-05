######################
#SPRING FLIGHT GRAPHS#
######################

#1) THIS INCLUDES THE GRAPHING OF THE ANNUAL SPRING FLIGHT OF ALL FIVE SPECIES

###GENERAL CODE
#theme_springflight <- 
#  theme_bw()+
#  theme(panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        axis.line.x = element_line(color="black"),
#        axis.line.y = element_line(color="black"),
#        plot.title=element_text( size=12, vjust=0),
#        axis.text.x=element_text(size=12, angle=90,vjust=0.5, hjust=1),
#       axis.text.y=element_text(size=12),
#        axis.title.x=element_text(size=12, vjust=0),
#        axis.title.y=element_text(size=12, vjust=0))
 
#I think I like theme_publication() better
#---------------------------------------------------------------------------#


#Codling moth 

CM_FLIGHT <- ggplot(na.omit(CM.first.spring), aes(x= year, y= julian))+
  geom_point(size=2)+geom_line()+theme_Publication()+
  labs(x= "Year", y= "Julian day")+
  labs(title = "Cydia pomonella")+scale_x_continuous(breaks=seq(1984,2016,4)); CM_FLIGHT
#Oblique banded leafroller


OB_FLIGHT<-ggplot(na.omit(ob.first.spring), aes(x= year, y= julian))+
  geom_point(size=2)+geom_line()+theme_Publication()+
  labs(x= "Year", y= "Julian day")+
  labs(title = "Choristoneura rosaceana")+scale_x_continuous(breaks=seq(1981,2016,4)) 
OB_FLIGHT

TA_FLIGHT <-  ggplot(na.omit(ta.first.spring), aes(x= year, y= julian))+
  geom_point(size=2)+geom_line()+theme_Publication()+
  labs(x= "Year", y= "Julian day")+
  labs(title = "Platynota idaeusalis")+scale_x_continuous(breaks=seq(1981,2016,4))
TA_FLIGHT


OFMP_FLIGHT<-ggplot(na.omit(ofm.first.spring_PCH), aes(x= year, y= julian ))+
  geom_point(size=2)+geom_line()+ theme_Publication()+
  labs(x= "Year", y= "Julian day")+
  labs(title = "Oriental Fruit Moth- Peach")+scale_x_continuous(breaks=seq(1999,2016,2))
OFMP_FLIGHT

OFMA_FLIGHT<-ggplot(na.omit(ofm.first.spring_APP), aes(x= year, y= julian ))+
  geom_point(size=2)+geom_line()+ 
  labs(x= "Year", y= "Julian day")+ theme_Publication()+
  labs(title = "Oriental Fruit Moth- Apple")+scale_x_continuous(breaks=seq(1981,2016,4))
OFMA_FLIGHT

OFM_FLIGHT



RB_FLIGHT <-ggplot(na.omit(rb.first.spring), aes(x= year, y= julian ))+
  geom_point(size=2)+geom_line()+
  labs(x= "Year", y= "Julian day")+ theme_Publication()+
  labs(title = "Argyrotaenia velutina")+scale_x_continuous(breaks=seq(1981,2016,4))
RB_FLIGHT

grid.arrange(CM_FLIGHT, OB_FLIGHT, TA_FLIGHT )