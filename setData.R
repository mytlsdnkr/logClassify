library(stringr)
library(tm)

setwd("/root/workspace");

getwd();
get_group <- function(i,group){
  a<-str_split(i,"-")
  a<-unlist(a)
  parsing<-str_trim(a[2])
  alert_group<-str_split(parsing,",")
  alert_group<-unlist(alert_group)
  group<-c(group,alert_group)
  return(group);
  
}

get_level <- function(i){
  description<-str_split(i,"->")
  description<-unlist(description)
  alert_level<-str_split(description,"level")
  alert_level<-unlist(alert_level)
  alert_level<-str_trim(alert_level[2])
  aa<-str_sub(alert_level,start=1,end=1)
  aa<-as.numeric(aa)
  return(aa);
}

get_description <- function(i,description){
  alert_description<-str_split(i,"->")
  alert_description<-unlist(alert_description)
  alert_description<-str_trim(alert_description[2])
  # description<-str_replace_all(description,'','')
  alert_description<-removeWords(alert_description,stopwords("en"))
  description<-c(description,alert_description)
  print(description)
  return(description);
  
  
}




text<-readLines("alert_25.log");
count<- -1
load("group.txt")
description<-vector()

for(i in text){
  level<-vector()
  if(nchar(i)!=0){
    count<-count+1
    if(count==0){
      group<-get_group(i,group)
    }else if(count==2){
      level<-get_level(i)
      description<-get_description(i,description)
    }else{
      next;
    }
  }else{
    count<--1
  }
  
}
group<-unique(group)
description<-unique(description)
group<-group[group!=""]
save(group,file="group.txt")
save(description,file="description.txt")


