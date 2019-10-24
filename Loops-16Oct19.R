library(tidyverse)
library(plyr)

load("fish_data.Rdata")
#test subset to make sure all functions inside the "ddply" umbrella function work
d<-fish[fish$depth_fac=="Deep",]

#ddply
nd<- ddply(.data=fish, .variables="depth_fac",function(x){
  z<-unique(x$depth_fac)
  #ifelse(test=z=="Deep",yes=50,no=25) 
  depth_condition<-function(y){
    if(z=="Deep")
      q<-50
    else if(z=="Mid")
      q<-25
    else
      q<-15
  }
  
x$depth_z<-depth_condition(y=z)
return(x)
}, .inform=T,.progress = "text")

#Test if depth_condition works
test=depth_condition(y="Mid")
test

#Test return of correct values
unique(nd$depth_z)
print(depth_condition(y="Deep"))

#list all the physical data files in a given directory

batch_data<-list.files("batch_data",full=TRUE,pattern="ISIIS")

phy<-adply(.data=batch_data, .margins=1, function(file) {
  
  #read the data
d<-read.table(file, sep="\t", skip=10, header=TRUE, fileEncoding="ISO-8859-1",
                stringsAsFactors=FALSE, quote="\"",check.names=FALSE,
              encoding="UTF-8",na.strings=9999.99)


#clean names
head<-names(d)
head<-str_replace(head, "\\(.*\\)", "")
head<-str_trim(head)
head<-make.names(head)
head<-tolower(head)
head<-str_replace(head,fixed(",."),".")

#assign names
names(d) = head

#create a proper date time format
date<-scan(file,what="character",skip=1,nlines=1,quiet=TRUE)

d$date<-date[2]

d$dateTime<-str_c(d$date, d$ttime, sep=" ")

d$dateTime<-as.POSIXct(strptime(d$dateTime,format="%m/%d/%y %H:%M:%OS",
                                tz="America/New_York"))

  return(d)
},.inform=T,.progress = "text")

10-21-2019------------For Loop

#for loop
#Starting with temperature conversion

t<-data.frame(f_deg=seq(0,100,1))
t$c_deg<-NA
t$K_deg<-NA

for(i in 1:nrow(t)) {
  t[i,]$c_deg<-(t[i,]$f_deg - 32) * (9/5)
  t[i,]$K_deg<-(t[i,]$c_deg - 273.15)
}

OR

for(i in 1:10 {
  t[i,]$c_deg<-(t[i,]$f_deg - 32) * (9/5)
  t[i,]$K_deg<-(t[i,]$c_deg - 273.15)
}

#combine with if else, set a floor for temp

t<-data.frame(f_deg=seq(0,100,1))
t$c_deg<-NA
t$K_deg<-NA
t$rel_temp<-NA

for(i in 1:nrow(t)) {
  t[i,]$c_deg<-(t[i,]$f_deg - 32) * (9/5)
  t[i,]$K_deg<-(t[i,]$c_deg - 273.15)
  
  t[i,]$rel_temp<-ifelse(test=t[i,]$c_deg < 0,
                         yes="cold",
                         no="not cold")
}

#
t<-data.frame(f_deg=seq(0,100,1))
t$c_deg<-NA
t$K_deg<-NA
t$rel_temp<-NA

goldilocks<-function(x) {
  if (x<=0)
    t[i,]$rel_temp<-"frozen"
  
  else if(x>0 & x<=50)
    t[i,]$rel_temp<-"cold"
  
  else if(x>50 & x<=70)
    t[i,]$rel_temp<-"warm"
  
  else if(x>50 & x<=70)
    t[i,]$rel_temp<-"hot"
  
}

for(i in 1:nrow(t)) {
  
  t[i,]$c_deg<-(t[i,]$f_deg - 32) * (9/5)
  t[i,]$K_deg<-(t[i,]$c_deg - 273.15)
  t[i,]$rel_temp<-goldilocks(x=t[i,]$c_deg)
}

#conditional statement

t<-data.frame(f_g=seq(0,1000,1))
y<-seq(1,10,0.5)
x<-seq(1,20,1)

d<-data.frame(interation = seq(1,10,0.5))

for(i in 1:length(y)) {
  for(k in 1:10) {
    
  d$output<-y[i] + x[k]
  }
}

10-23-2019----------GG plots

library(ggplot2)

load("fish_data.Rdata")

fish.deep<-fish[fish$depth_fac =="Deep",]

plot(x=fish.deep$parcel.start.lon,
     y=fish.deep$parcel.start.lat)

hist(log10(fish.parcel.density.m3))

#ggplot functions

ggplot(data=mpg, aes(x=displ,y=hwy))+
geom_point()

ggplot(data=mpg, aes(x=displ,y=hwy))+
geom_point(colour="red")

ggplot(data=mpg, aes(x=displ,y=hwy, colour=class))+
  geom_point()

ggplot(data=mpg, aes(x=cty,y=hwy, colour=class))+
  geom_point()

#to check the no. of classes
length(unique(mpg$class))

#for manually changing colours in the plot
ggplot(data=mpg, aes(x=displ,y=hwy, colour=class))+
  geom_point()+
  scale_colour_manual(values=c("firebrick","dodgerblue", "darkgreen", 
                             "goldenrod", "cornsilk2", "chocolate2", 
                             "deeppink"))

#ggplot2---line geom

ggplot(data=mpg, aes(x=displ,y=hwy))+
  geom_line()

ggplot(data=mpg, aes(x=displ,y=hwy))+
  geom_line()+
facet_wrap(~class) 

ggplot(data=mpg, aes(x=displ,y=hwy))+
  geom_point()+
  facet_wrap(~class, ncol=1)

ggplot(data=mpg, aes(x=displ,y=hwy))+
  geom_point()+
  facet_wrap(~class, nrow=4)

#Add a smoother
ggplot(data=mpg, aes(x=displ,y=hwy))+
  geom_point()+
  geom_smooth()

ggplot(data=mpg, aes(x=displ,y=hwy))+
  geom_point()+
  geom_smooth(method="lm")

#Histograms
ggplot(data=mpg, aes(x=displ,fill=drv))+
  geom_histogram()

ggplot(data=mpg, aes(x=displ,fill=drv))+
  geom_histogram(binwidth=0.5)

ggplot(data=mpg, aes(x=displ,colour=drv))+
  geom_freqpoly(binwidth=0.5)

