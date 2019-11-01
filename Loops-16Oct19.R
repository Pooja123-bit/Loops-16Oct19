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

10-28-2019

library(ggplot2)

#One geom:
data(economics)
e<- economics

unemploy<- ggplot(data-e, aes(x=date,y=unemploy)) + geom_line()

#multiple geoms
data("presidential")
pres<-presidential

ggplot(e) + 
  geom_rect(data=pres, aes(xmin= start, xmax= end, fill= party),
ymin=-Inf, ymax=Inf, alpha=0.2) + 
  geom_vline(data=pres,
             aes(xintercept=as.numeric(start)),colour="grey50",alpha=0.5)+
  geom_text(data=pres, aes(x=start, y=2500,
                           label=name), size=3, vjust=0, hjust=0,nudge_x=50)

ggplot(e) + 
  geom_rect(data=pres, aes(xmin= start, xmax= end, fill= party),
            ymin=-Inf, ymax=Inf, alpha=0.2) + 
  scale_fill_manual(values=c("dodgerblue", "firebrick3"))
  geom_vline(data=pres,
             aes(xintercept=as.numeric(start)),colour="grey50",alpha=0.5)+
  geom_text(data=pres, aes(x=start, y=2500,
                           label=name), size=3, vjust=0, hjust=0,nudge_x=50)
  
caption<-paste(strwrap("Unemployment rates in the U.S. have varied a lot 
over the years",20),
                       collapse="\n")
               yrng<-range(e$unemploy)
               xrng<-range(e$date)
ggplot(e) + 
geom_rect(data=pres, aes(xmin= start, xmax= end, fill= party),
ymin=-Inf, ymax=Inf, alpha=0.2) + 
scale_fill_manual(values=c("dodgerblue", "firebrick3")) + 
geom_vline(data=pres,
aes(xintercept=as.numeric(start)),colour="grey50",alpha=0.5)+
#geom_text(data=pres, aes(x=start, y=2500,
#label=name), size=3, vjust=0, hjust=0,nudge_x=50) +
  annotate("text", x=xrng[1], y=yrng[2],label=caption, hjust=0, vjust=1, 
           size=4)

caption<-paste(strwrap("Unemployment rates in the U.S. have varied a lot 
over the years",40),
               collapse="\n")
yrng<-range(e$unemploy)
xrng<-range(e$date)
date<-as.Date("1960-01-01")

ggplot(e) + 
  geom_line(aes(x=date,y=unemploy))+
  geom_rect(data=pres, aes(xmin= start, xmax= end, fill= party),
            ymin=-Inf, ymax=Inf, alpha=0.2) + 
  scale_fill_manual(values=c("dodgerblue", "firebrick3")) + 
  geom_vline(data=pres,
             aes(xintercept=as.numeric(start)),colour="grey50",alpha=0.5)+
  #geom_text(data=pres, aes(x=start, y=2500,
  #label=name), size=3, vjust=0, hjust=0,nudge_x=50) +
  annotate("text", x=xrng[1], y=yrng[2],label=caption, hjust=0, vjust=1, 
           size=4)

#Bar graph challenge----
#Stacked bar with grouped bar

load("fish_data.Rdata")
library(tidyverse)

fs<-fish %>% group_by(area_fac, depth_fac, yr_fac) %>% 
  summarise(parcel.count=length(parcel.id))

ggplot(data = fs) + 
  geom_bar(aes(x=area_fac, y=parcel.count, fill= depth_fac),
           position="stack",
           stat="Identity")

#For getting up and down
ggplot(data = fs) +  
  geom_bar(aes(x=area_fac, y=parcel.count, fill= depth_fac),
           position="stack",
           stat="Identity")  +
  facet_grid(yr_fac~.)

#grouped bar-----
ggplot(data = fs) +  
  geom_bar(aes(x=area_fac, y=parcel.count, fill= depth_fac),
           position="dodge",
           stat="Identity") 
           
#Using the 'ddply' function to create multiply plots----

10-30-2019

library(plyr)
library(tidyverse)
load("fish_data.Rdata")
ggplot(fish, aes(parcel.length.m, parcel.density.m3, color=depth_fac))+
      geom_point()

#Using 'ddply' to plot multiple objets-----
ddply(.data=fish, .variables="depth_fac", function(x){
pl= ggplot(x, aes(parcel.length.m, parcel.density.m3, color=depth_fac))+
  geom_point()
  name=unique(x$depth_fac)
  ggsave(filename= paste0(name,'.tiff'),
         plot=pl, width=4, height=3, units='in',
         dpi=600, compression='lzw')
}, .progress="text")

##
ddply(.data=fish, .variables="depth_fac", function(x){
  name=unique(x$depth_fac)
  pl= ggplot(x, aes(parcel.length.m, parcel.density.m3, color=depth_fac))+
    geom_point()+
  xlab('Parcel length (m)')+
  ylab(expression(paste('Parcel Density(',m^3,')')))+
    ggtitle(name)
  ggsave(filename= paste0(name,'.tiff'),
         plot=pl, width=4, height=3, units='in',
         dpi=600, compression='lzw')
}, .progress="text")

##
ggplot(fish, aes(parcel.length.m, parcel.density.m3))+
  geom_point()+
  ylab(expression(paste('Parcel Density(',m^3,')')))+
  facet_wrap(~depth_fac)

##plotting 3 variables----
data("mtcars")

ggplot(mtcars, aes(vs, cyl, fill=mpg))+
geom_tile()

ggplot(mtcars, aes(wt,mpg))+
  geom_point(size=mtcars$cyl)

#with title in graph
ggplot(mtcars, aes(wt,mpg))+
  geom_point(aes(size=mtcars$cyl))

#with title in graph alongwith color
ggplot(mtcars, aes(wt,mpg))+
  geom_point(aes(size=mtcars$cyl, color=hp))

#with title in graph, color & grids
ggplot(mtcars, aes(wt,mpg))+
  geom_point(aes(size=cyl, color=hp))+
  theme_linedraw()

#with title in graph, grids + Change colors
ggplot(mtcars, aes(wt,mpg))+
  geom_point(aes(color=hp))+
  scale_color_continuous(type='viridis')+
  theme_bw()
