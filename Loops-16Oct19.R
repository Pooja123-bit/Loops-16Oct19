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