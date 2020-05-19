library(stringr)
library(tm)
library(ngram)


setwd("/home/park/workspace/logClassify");
load("data/group.txt")
load("data/description.txt")

print(group)
print(current_description)


dir<-list.files(path="usedData",pattern=NULL)
print(dir)
for(i in dir){
  dpath<-paste(c("usedData/"),i,sep="")
  setwd(dpath)
  fileList<-list.files(path=".",pattern=NULL)
  print(fileList)
  for(j in fileList){
    text<-readLines(j,encoding="utf-8");
    print(text)
    break
    count<- -1
    for(k in text){
      current_group <- vector()
      current_description <- vector()
      if(nchar(k)!=0){
        count<-count+1
        if(count==0){
          current_group<-get_group(k,current_group)
          print(current_group)
          break
        }else if(count==2){
          description<-get_description(k,description)
        }else{
          next;
        }
      }else{
        count<--1
      }
      
    }
    
    break
    
    
  }
  
  break
  
  
}



