library(stringr)
library(tm)
library(filesstrings)
library(stringi)

setwd("/home/park/workspace/logClassify");

get_group <- function(i,group){
  a<-str_split(i,"-")
  a<-unlist(a)
  parsing<-str_trim(a[2])
  alert_group<-str_split(parsing,",")
  alert_group<-unlist(alert_group)
  group<-c(group,alert_group)
  return(group);
  
}

get_description <- function(i,description){
  alert_description<-str_split(i,"->")
  alert_description<-unlist(alert_description)
  alert_description<-str_trim(alert_description[2])
  # description<-str_replace_all(description,'','')
  alert_description<-removeWords(alert_description,stopwords("en"))
  description<-c(description,alert_description)
  return(description);
  
  
}
preprocessing_group<-function(group){
  group<-unique(group)
  group<-group[group!=""]
  group <- group[!is.na(group)]
  count <- 0
  for(i in group){
    group[count]<-str_trim(i)
    count <- count+1
  }
  for(num in group){
    if(nchar(num,type="width")<=2){
      group<-group[group!=num]
    }else{
      next
    }
  }
  
  return(group)
  
  
  
  
}

preprocessing_description<-function(description){
  current_description <- vector()
  description <- na.omit(description)
  description<-description[description!=""]
  description<-unique(description)
  description<-gsub("(","",description,fixed=TRUE)
  description<-gsub(")","",description,fixed=TRUE)
  description<-gsub("'","",description,fixed=TRUE)
  description<-gsub(":","",description,fixed=TRUE)
  description<-gsub(".","",description,fixed=TRUE)
for(i in description){
  imsi <- strsplit(i," ",fixed=TRUE)
  imsi <- unlist(imsi)
  for(k in imsi){
    imsi_vector <- vector()
    if(nchar(k,type="width")<=2){
      imsi_vector<-imsi[imsi!=k]
    }
  }
  current_description <- c(current_description,imsi_vector)
}
  current_description <- unique(current_description)
  
  for(i in current_description){
    if(nchar(i,type="width")<=2){
      current_description <- current_description[current_description!=i]
    }
  }
  return(current_description)
  
}


setwd("/home/park/workspace/logClassify");
group <- vector();
description <- vector();
dir<-list.files(path="log",pattern=NULL)
for(i in dir){
  dpath<-paste(c("log/"),i,sep="")
  setwd(dpath)
  fileList<-list.files(path=".",pattern=NULL)
  
  for(j in fileList){
    
    text<-readLines(j,encoding="utf-8");
    
    count<- -1
    for(k in text){
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
  setwd("/home/park/workspace/logClassify");
  cmd<-paste(c("mv -f "),dpath,sep="")
  cmd<-paste(cmd," usedData",sep="")
  system(cmd)
}

group <- preprocessing_group(group)
description <- preprocessing_description(description)


save(group,file="data/group.txt")
save(current_description,file="data/description.txt")
