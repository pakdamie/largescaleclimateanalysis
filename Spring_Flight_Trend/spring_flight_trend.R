#Spring flight phenology-trend data#
#This will be figuring out where the 25th percent is


#MAKE SURE THIS IS ALWAYS AT 25 FOR ALL ANALYSIS
DD.func <- function (x)
  exp(-exp((-r*x + b))) - .25

################
#CODLING MOTH #
###############

cm.split.new <- split(cm.phi.new, cm.phi.new$year)


first.gen.cm_man <- NULL
for (i in seq(1, 33)){
  temp = cm.split.new [[i]]
  a = subset(temp, temp$phi.cum <674)
  a$sum <- cumsum(a$Mean)
  a$prop <- (a$sum/sum(a$Mean))
  first.gen.cm_man[[i]]=a
  }
##################
cm.nls_man= NULL
for (i in seq(length(first.gen.cm_man))){
  if (nrow(first.gen.cm_man[[i]]) != 0){
  cm.nls_man[[i]]<- nls(prop~(exp(-exp((-r*phi.cum + b)))),
  data = first.gen.cm_man[[i]],
  start = list(r=.001, b=.001 ))
  }
else {
cm.nls_man[[i]] = NA
  }
}

#Pulls out the coefficient 
param.cm_man <- NULL
for (i in seq(1,length(cm.nls_man))){
  if (length(cm.nls_man[[i]]) != 1) {
  param.cm_man[[i]] = c(coef(cm.nls_man[[i]]), as.integer(i+1983))
  print(i+1983)
  }
  else {
    param.cm_man[[i]]= NA
  }
}

param.cm2_man <- data.frame(do.call(rbind,param.cm_man))

solved.cm.first_man = NULL
  for (i in seq(1, 33)){
    if (is.na(param.cm2_man [i,1]) == FALSE) {
    tmp= param.cm2_man[i,]
    r= tmp$r
    b=  tmp$b
    tmp2=uniroot(DD.func, c(1,1000))
solved.cm.first_man[[i]]=tmp2$root}
else{
solved.cm.first_man[[i]]= NA
}}
###
marco_man=NULL
for (i in seq(1,length(solved.cm.first_man))){
  if (is.na(solved.cm.first_man[[i]])== FALSE){
  marco_man[[i]]=(which.min(abs(phi.50.year[[i+3]]$phi.cum-
  solved.cm.first_man[[i]])))
  }
  else{
  marco_man[[i]]= NA
  }}

Date.cm_man=NULL
for(i in seq(1,length(solved.cm.first_man))){
  if (is.na(solved.cm.first_man[[i]])== FALSE){
  Date.cm_man[[i]]=as.character((phi.50.year[[i+3]][marco_man[[i]],]$Date))
  }
  else{
  Date.cm_man[[i]]= NA
  }}


CM.first.spring<- data.frame(Date.cm_man, solved.cm.first_man)
CM.first.spring$Date.cm_man <- as.Date(Date.cm_man, format = "%Y-%m-%d")
CM.first.spring$year <-as.numeric(format(CM.first.spring$Date, "%Y"))
CM.first.spring$month <- as.numeric(format(CM.first.spring$Date, "%m"))
CM.first.spring$julian <- as.numeric(format(CM.first.spring$Date,"%j"))

#write.table(CM.first.spring$julian, sep="\t", row.names=FALSE, col.names=FALSE)

#---------------------------------------------#
###############################################

###Obliquebanded leaforller 

ob.split.new <- split(ob.phi.new, ob.phi.new$year)

#This will be figuring out where the 25th percent is
#MAKE SURE THIS IS ALWAYS AT 25 FOR ALL ANALYSIS
DD.func <- function (x)
  exp(-exp((-r*x + b))) - .25

################


####
first.gen.ob_man<- NULL

for (i in seq(1, 36)){
  temp = ob.split.new [[i]]
  a = subset(temp, temp$phi.cum <1051)
  a$sum <- cumsum(a$Mean)
  a$prop <- (a$sum/sum(a$Mean))
  first.gen.ob_man[[i]]=a
}

ob.nls_man= NULL
for (i in seq(length(first.gen.ob_man))){
  
  ob.nls_man[[i]]<- try(nls(prop~(exp(-exp((-r*phi.cum + b)))),
                            data = first.gen.ob_man[[i]],
                            start = list(r=.001, b=.001 )))
}


param.ob_man <- NULL
for (i in seq(1,length(ob.nls_man))){
  if (length(ob.nls_man[[i]]) != 1) {
    param.ob_man[[i]] = c(coef(ob.nls_man[[i]]), as.integer(i+1980))
  }
  else {
    param.ob_man[[i]]= NA
  }}


param.ob2_man <- data.frame(do.call(rbind,param.ob_man))


solved.ob.first_man = NULL
for (i in seq(1, 36)){
  if (is.na(param.ob2_man [i,1]) == FALSE) {
    tmp= param.ob2_man[i,]
    r= tmp$r
    b=  tmp$b
    tmp2=(uniroot(DD.func, c(1,1000),  extendInt="yes"))
    solved.ob.first_man[[i]]=tmp2$root}
  else{
    solved.ob.first_man[[i]]= NA
  }}


###
marco_man=NULL
for (i in seq(1,length(solved.ob.first_man))){
  if (is.na(solved.ob.first_man[[i]])== FALSE){
    marco_man[[i]]=(which.min(abs(phi.43.year[[i]]$phi.cum-
                                    solved.ob.first_man[[i]])))
  }
  else{
    marco_man[[i]]= NA
  }}

Date.ob_man=NULL
for(i in seq(1,length(solved.ob.first_man))){
  if (is.na(solved.ob.first_man[[i]])== FALSE){
    Date.ob_man[[i]]=as.character((phi.43.year[[i]][marco_man[[i]],]$Date))
  }
  else{
    Date.ob_man[[i]]= NA
  }}


ob.first.spring<- data.frame(Date.ob_man, solved.ob.first_man)
ob.first.spring$Date.ob_man <- as.Date(Date.ob_man, format = "%Y-%m-%d")
ob.first.spring$year <-as.numeric(format(ob.first.spring$Date, "%Y"))
ob.first.spring$month <- as.numeric(format(ob.first.spring$Date, "%m"))
ob.first.spring$julian <- as.numeric(format(ob.first.spring$Date,"%j"))

ob.first.spring[1,] <- NA #1981
#ob.first.spring[11,]<- NA #1991/already done automatically
ob.first.spring[25,] <- NA #2005
ob.first.spring[36,] <- NA # 2016 

ob.first.spring$year <- seq(1981, 2016,1)

write.table(ob.first.spring$julian, sep="\t", row.names=FALSE, col.names=FALSE)

#-------------------------------------#

############################
###TUFTED APPLE BUD MOTH ###
############################
#This will be figuring out where the 25th percent is
#MAKE SURE THIS IS ALWAYS AT 25 FOR ALL ANALYSIS
DD.func <- function (x)
  exp(-exp((-r*x + b))) - .25


ta.split.new <- split(ta.phi.new, ta.phi.new$year)

###
###
first.gen.ta_man<- NULL

for (i in seq(1, 36)){
  temp = ta.split.new [[i]]
  a = subset(temp, temp$phi.cum <1010.033)
  a$sum <- cumsum(a$Mean)
  a$prop <- (a$sum/sum(a$Mean))
  first.gen.ta_man[[i]]=a
}




ta.nls_man= NULL
for (i in seq(1,36)){
  
  try(ta.nls_man[[i]]<- (nls(prop~(exp(-exp((-r*phi.cum + b)))),
                             data = first.gen.ta_man[[i]],
                             start = list(r=.001, b=.001 ))))
}
ta.nls_man[[1]]<- NA

param.ta_man <- NULL
for (i in seq(1,length(ta.nls_man))){
  if (length(ta.nls_man[[i]]) != 1) {
    param.ta_man[[i]] = c(coef(ta.nls_man[[i]]), as.integer(i+1980))
  }
  else {
    param.ta_man[[i]]= NULL
  }}

param.ta_man[1]<- NA
param.ta2_man <- data.frame(do.call(rbind,param.ta_man))


solved.ta.first_man = NULL
for (i in seq(1, 36)){
  if (is.na(param.ta2_man [i,1]) == FALSE) {
    tmp= param.ta2_man[i,]
    r= tmp$r
    b=  tmp$b
    tmp2=(uniroot(DD.func, c(1,1000),  extendInt="yes"))
    solved.ta.first_man[[i]]=tmp2$root}
  else{
    solved.ta.first_man[[i]]= NA
  }}


###
marco_man=NULL
for (i in seq(1,length(solved.ta.first_man))){
  if (is.na(solved.ta.first_man[[i]])== FALSE){
    marco_man[[i]]=(which.min(abs(phi.45.year[[i]]$phi.cum-
                                    solved.ta.first_man[[i]])))
  }
  else{
    marco_man[[i]]= NA
  }}

Date.ta_man=NULL
for(i in seq(1,length(solved.ta.first_man))){
  if (is.na(solved.ta.first_man[[i]])== FALSE){
    Date.ta_man[[i]]=as.character((phi.45.year[[i]][marco_man[[i]],]$Date))
  }
  else{
    Date.ta_man[[i]]= NA
  }}


ta.first.spring<- data.frame(Date.ta_man, solved.ta.first_man)
ta.first.spring$Date.ta_man <- as.Date(Date.ta_man, format = "%Y-%m-%d")
ta.first.spring$year <-as.numeric(format(ta.first.spring$Date, "%Y"))
ta.first.spring$month <- as.numeric(format(ta.first.spring$Date, "%m"))
ta.first.spring$julian <- as.numeric(format(ta.first.spring$Date,"%j"))

ta.first.spring[22,] <- NA
ta.first.spring[27,] <- NA
ta.first.spring[28,] <- NA

ta.first.spring$year <- seq(1981, 2016, 1)

#write.table(ta.first.spring$julian, sep="\t", row.names=FALSE, col.names=FALSE)

#------------------------------------------------------------------#
##################################
###Oriental Fruit Moth- Peach ###
#################################

##############
####PEACH ###
##############

#This will be figuring out where the 25th percent is
#MAKE SURE THIS IS ALWAYS AT 25 FOR ALL ANALYSIS
DD.func <- function (x)
  exp(-exp((-r*x + b))) - .25


of.split.new_PCH <- split(of.phi.new.PCH,of.phi.new.PCH$year)



#########################################################
first.gen.ofm_PCH<- NULL

for (i in seq(1, 18)){
  temp = of.split.new_PCH [[i]]
  a = subset(temp, temp$phi.cum <432.06667)
  a$sum <- cumsum(a$Mean)
  a$prop <- (a$sum/sum(a$Mean))
  first.gen.ofm_PCH[[i]]=a
}


ofm.nls_PCH= NULL
for (i in seq(length(first.gen.ofm_PCH))){
  
  ofm.nls_PCH[[i]]<- try(nls(prop~(exp(-exp((-r*phi.cum + b)))),
                             data = first.gen.ofm_PCH[[i]],
                             start = list(r=.001, b=.001 )))
}


param.ofm_PCH<- NULL
for (i in seq(1,length(ofm.nls_PCH))){
  if (length(ofm.nls_PCH[[i]]) != 1) {
    param.ofm_PCH[[i]] = c(coef(ofm.nls_PCH[[i]]), as.integer(i+1998))
  }
  else {
    param.ofm_PCH[[i]]= NA
  }}



param.ofm2_PCH <- data.frame(do.call(rbind,param.ofm_PCH))


solved.ofm.first_PCH = NULL
for (i in seq(1, 18)){
  if (is.na(param.ofm2_PCH [i,1]) == FALSE) {
    tmp= param.ofm2_PCH[i,]
    r= tmp$r
    b=  tmp$b
    tmp2=(uniroot(DD.func, c(1,1000),  extendInt="yes"))
    solved.ofm.first_PCH[[i]]=tmp2$root}
  else{
    solved.ofm.first_PCH[[i]]= NA
  }}


###
marco_PCH=NULL
for (i in seq(1,18)){
  if (is.na(solved.ofm.first_PCH[[i]])== FALSE){
    marco_PCH[[i]]=(which.min(abs(phi.45.year.of[[i+18]]$phi.cum-
                                    solved.ofm.first_PCH[[i]])))
  }
  else{
    marco_PCH[[i]]= NA
  }}

Date.ofm_PCH=NULL
for(i in seq(1,length(solved.ofm.first_PCH))){
  if (is.na(solved.ofm.first_PCH[[i]])== FALSE){
    Date.ofm_PCH[[i]]=as.character((phi.45.year[[i+18]][marco_PCH[[i]],]$Date))
  }
  else{
    Date.ofm_PCH[[i]]= NA
  }}


ofm.first.spring_PCH<- data.frame(Date.ofm_PCH, solved.ofm.first_PCH)
ofm.first.spring_PCH$Date.ofm_PCH <- as.Date(Date.ofm_PCH, format = "%Y-%m-%d")
ofm.first.spring_PCH$year <-as.numeric(format(ofm.first.spring_PCH$Date, "%Y"))
ofm.first.spring_PCH$month <- as.numeric(format(ofm.first.spring_PCH$Date, "%m"))
ofm.first.spring_PCH$julian <- as.numeric(format(ofm.first.spring_PCH$Date,"%j"))

write.table(ofm.first.spring_PCH$julian, sep="\t", row.names=FALSE, col.names=FALSE)
###########
###APPLE###
###########

#This will be figuring out where the 25th percent is
#MAKE SURE THIS IS ALWAYS AT 25 FOR ALL ANALYSIS
DD.func <- function (x)
  exp(-exp((-r*x + b))) - .25

of.split.new_APP <- split(of.phi.new.APP,of.phi.new.APP$year)



#########################################################
first.gen.ofm_APP<- NULL

for (i in seq(1, 36)){
  temp = of.split.new_APP [[i]]
  a = subset(temp, temp$phi.cum <456.0556)
  a$sum <- cumsum(a$Mean)
  a$prop <- (a$sum/sum(a$Mean))
  first.gen.ofm_APP[[i]]=a
}


ofm.nls_APP= NULL
for (i in seq(length(first.gen.ofm_APP))){
  
  ofm.nls_APP[[i]]<- try(nls(prop~(exp(-exp((-r*phi.cum + b)))),
                             data = first.gen.ofm_APP[[i]],
                             start = list(r=.001, b=.001 )))
}


param.ofm_APP<- NULL
for (i in seq(1,length(ofm.nls_APP))){
  if (length(ofm.nls_APP[[i]]) != 1) {
    param.ofm_APP[[i]] = c(coef(ofm.nls_APP[[i]]), as.integer(i+1980))
  }
  else {
    param.ofm_APP[[i]]= NA
  }}



param.ofm2_APP <- data.frame(do.call(rbind,param.ofm_APP))


solved.ofm.first_APP = NULL
for (i in seq(1, 36)){
  if (is.na(param.ofm2_APP [i,1]) == FALSE) {
    tmp= param.ofm2_APP[i,]
    r= tmp$r
    b=  tmp$b
    tmp2=(uniroot(DD.func, c(1,1000),  extendInt="yes"))
    solved.ofm.first_APP[[i]]=tmp2$root}
  else{
    solved.ofm.first_APP[[i]]= NA
  }}


###
marco_APP=NULL
for (i in seq(1,length(solved.ofm.first_APP))){
  if (is.na(solved.ofm.first_APP[[i]])== FALSE){
    marco_APP[[i]]=(which.min(abs(phi.45.year.of[[i]]$phi.cum-
                                    solved.ofm.first_APP[[i]])))
  }
  else{
    marco_APP[[i]]= NA
  }}

Date.ofm_APP=NULL
for(i in seq(1,length(solved.ofm.first_APP))){
  if (is.na(solved.ofm.first_APP[[i]])== FALSE){
    Date.ofm_APP[[i]]=as.character((phi.45.year[[i]][marco_APP[[i]],]$Date))
  }
  else{
    Date.ofm_APP[[i]]= NA
  }}


ofm.first.spring_APP<- data.frame(Date.ofm_APP, solved.ofm.first_APP)
ofm.first.spring_APP$Date.ofm_APP <- as.Date(Date.ofm_APP, format = "%Y-%m-%d")
ofm.first.spring_APP$year <-as.numeric(format(ofm.first.spring_APP$Date, "%Y"))
ofm.first.spring_APP$month <- as.numeric(format(ofm.first.spring_APP$Date, "%m"))
ofm.first.spring_APP$julian <- as.numeric(format(ofm.first.spring_APP$Date,"%j"))

write.table(ofm.first.spring_APP$julian, sep="\t", row.names=FALSE, col.names=FALSE)

#--------------------------------------------------#
############################
###Red banded leafroller###
###########################


#This will be figuring out where the 25th percent is
#MAKE SURE THIS IS ALWAYS AT 25 FOR ALL ANALYSIS
DD.func <- function (x)
  exp(-exp((-r*x + b))) - .25
###


rb.split.new <- split(rb.phi.new, rb.phi.new$year)

first.gen.rb_man<- NULL

for (i in seq(1, 35)){
  temp = rb.split.new [[i]]
  a = subset(temp, temp$phi.cum <500.7444)
  a$sum <- cumsum(a$Mean)
  a$prop <- (a$sum/sum(a$Mean))
  first.gen.rb_man[[i]]=a
}


rb.nls_man= NULL
for (i in seq(length(first.gen.rb_man))){
  
  rb.nls_man[[i]]<- try(nls(prop~(exp(-exp((-r*phi.cum + b)))),
                            data = first.gen.rb_man[[i]],
                            start = list(r=.001, b=.001 )))
}


param.rb_man <- NULL
for (i in seq(1,length(rb.nls_man))){
  if (length(rb.nls_man[[i]]) != 1) {
    param.rb_man[[i]] = c(coef(rb.nls_man[[i]]), as.integer(i+1980))
  }
  else {
    param.rb_man[[i]]= NA
  }}



param.rb2_man <- data.frame(do.call(rbind,param.rb_man))



solved.rb.first_man = NULL
for (i in seq(1, 35)){
  if (is.na(param.rb2_man [i,1]) == FALSE) {
    tmp= param.rb2_man[i,]
    r= tmp$r
    b=  tmp$b
    tmp2=(uniroot(DD.func, c(1,1000),  extendInt="yes"))
    solved.rb.first_man[[i]]=tmp2$root}
  else{
    solved.rb.first_man[[i]]= NA
  }}


###
marco_man=NULL
for (i in seq(1,length(solved.rb.first_man))){
  if (is.na(solved.rb.first_man[[i]])== FALSE){
    marco_man[[i]]=(which.min(abs(phi.43.year[[i]]$phi.cum-
                                    solved.rb.first_man[[i]])))
  }
  else{
    marco_man[[i]]= NA
  }}

Date.rb_man=NULL
for(i in seq(1,length(solved.rb.first_man))){
  if (is.na(solved.rb.first_man[[i]])== FALSE){
    Date.rb_man[[i]]=as.character((phi.43.year[[i]][marco_man[[i]],]$Date))
  }
  else{
    Date.rb_man[[i]]= NA
  }}


rb.first.spring<- data.frame(Date.rb_man, solved.rb.first_man)
rb.first.spring$Date.rb_man <- as.Date(Date.rb_man, format = "%Y-%m-%d")
rb.first.spring$year <-as.numeric(format(rb.first.spring$Date, "%Y"))
rb.first.spring$month <- as.numeric(format(rb.first.spring$Date, "%m"))
rb.first.spring$julian <- as.numeric(format(rb.first.spring$Date,"%j"))

rb.first.spring[5,]<- NA
rb.first.spring[6,]<- NA
rb.first.spring[7,]<- NA
rb.first.spring[9,]<- NA
rb.first.spring[10,]<-NA

rb.first.spring$year <- seq(1981, 2015, 1)

#write.table(rb.first.spring$julian, sep="\t", row.names=FALSE, col.names=FALSE)


