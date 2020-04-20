library(stringr)
library(tm)
library(filesstrings)

setwd("/root/workspace/logClassify");

get_group <- function(i,group){
  a<-str_split(i,"-")
  a<-unlist(a)
  parsing<-str_trim(a[2])
  alert_group<-str_split(parsing,",")
  alert_group<-unlist(alert_group)
  group<-c(group,alert_group)
  return(group);
  
}

get_description <- functions(i,description){
  alert_description<-str_split(i,"->")
  alert_description<-unlist(alert_description)
  alert_description<-str_trim(alert_description[2])
  # description<-str_replace_all(description,'','')
  alert_description<-removeWords(alert_description,stopwords("en"))
  description<-c(description,alert_description)
  return(description);
  
  
}

preprocessing_description<-function(description){
  description<-str_split(description," ")
  description<-unlist(description)
  description<-description[description!=""]
  description<-unique(description)
  
  description<-gsub("(","",description,fixed=TRUE)
  description<-gsub(")","",description,fixed=TRUE)
  description<-gsub("'","",description,fixed=TRUE)
  description<-gsub(":","",description,fixed=TRUE)
  description<-gsub(".","",description,fixed=TRUE)
  
  for(i in description){
    if(nchar(i)<=2){
      description<-description[description!=i]
    }
    else{
      next
    }
    
  }
  
  return(description)
  
  
  
}





setwd("/root/workspace/logClassify");
load("data/group.txt")
load("data/description.txt")
dir<-list.files(path="log",pattern=NULL)
for(i in dir){
  dpath<-paste(c("log/"),i,sep="")
  setwd(dpath)
  fileList<-list.files(path=".",pattern=NULL)
  
  for(j in fileList){
    text<-readLines(j,encoding="euc-kr");
    
    count<- -1
    for(k in text){
      print(k)
      if(nchar(k)!=0){
        count<-count+1
        if(count==0){
          group<-get_group(k,group)
        }else if(count==2){
          description<-get_description(k,description)
        }else{
          next;
        }
      }else{
        count<--1
      }
      
    }
    
    
  }
  setwd("/root/workspace/logClassify");
  cmd<-paste(c("mv -f "),dpath,sep="")
  cmd<-paste(cmd," usedData",sep="")
  print(cmd)
  system(cmd)
  
  
}



description<-preprocessing_description(description)


group<-unique(group)
description<-unique(description)
group<-group[group!=""]
save(group,file="data/group.txt")
save(description,file="data/description.txt")
