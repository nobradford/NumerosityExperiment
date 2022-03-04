library(tidyverse)
library(ggplot2)
setwd("C:/Users/nora/Desktop/jspsych/data/")
fileNames<- list.files(path = ".",pattern = "session")#list of raw data files
df= data.frame()
for (x in fileNames){
  data<-read_csv(x)#load df for single person
  f<-str_split(x, "-")
  fileName<-f[[1]][2]
  
  ##data rearranging -
    #separate into new dfs based on question type
  response<-data%>%
    filter(stimulus == '<p> Which side had more items?</p><p>select 1 for left and 2 for right</p>')%>%
    select(key_press,rt,workerId)%>%
    dplyr::rename(rtImage=rt)
  response$trials <- 1:nrow(response) 
  conf<-data%>%
    filter(stimulus == '<p>How confident do you feel in your choice?</p>')%>%
    select(response,rt)%>%
    dplyr::rename(rtConf=rt,
                  conf=response)%>%
    dplyr::mutate(confZ= (conf-mean(conf))/sd(conf)) #z score of confidence
  conf$trials <- 1:nrow(conf) 
  stim<-data%>%
    filter(rt=='null')%>%
    select(stimulus)
  stim$trials <- 1:nrow(stim) 
    #merge into one df
  newdata<-merge(stim,conf, by = 'trials')
  newdata<-merge(newdata,response, by = 'trials')
  
  ## separate stimuli images and extract var+num from stim names
  # initialize lists
  varRight = c()
  varLeft = c()
  numRight = c()
  numLeft = c()
  # extract values from string
  for (x in 1:nrow(newdata)){
    stim1<-str_split(newdata$stimulus[x], "_")[[1]]
    varLeft = c(varLeft,stim1[2])
    numLeft= c(numLeft,stim1[4])
    varRight = c(varRight,stim1[6])
    numRight = c(numRight,stim1[8])
  }
  #save as new variables in df
  newdata$varLeft<-as.numeric(unlist(varLeft))#they were all lists...so had to unlist them 
  newdata$numLeft<-as.numeric(unlist(numLeft))
  newdata$varRight<-as.numeric(unlist(varRight))
  newdata$numRight<-as.numeric(unlist(numRight))
  
  ## key press/correctness-
  
  #relabel keypress and mark as type 1 correct or incorrect
  newdata<-newdata%>%
    mutate(key_press = ifelse(key_press == 49, "Left", #key press values from js keyboard
                              ifelse(key_press== 50,"Right", "NA")))%>% 
    #relabel keypress as either left or right
    dplyr::mutate(correct = ifelse(numLeft > numRight & key_press == "Left", "1", 
                                   ifelse(numRight > numLeft & key_press == "Right", "1", 
                                          ifelse(numRight == numLeft, "2", "0"))))
  
  ## variance 
  #label variance pairings and conditions
  newdata<-newdata%>%
    dplyr::mutate(varSide = ifelse(varRight>varLeft, "Right",
                                        ifelse(varLeft>varRight,"Left","Equal")))%>%
    dplyr::mutate(varRatio = ifelse(varRight>varLeft, varRight/varLeft,
                                    ifelse(varRight<varLeft,varLeft/varRight,"1")))%>%
    dplyr::mutate(numRatio = as.numeric(unlist(ifelse(numRight>numLeft, numRight/numLeft,
                                    ifelse(numRight<numLeft,numLeft/numRight,"1")))))%>%
    dplyr::mutate(condition = ifelse(varRight<varLeft & numRight>numLeft |varLeft<varRight & numLeft>numRight , "congruent",
                                     ifelse(varRight>varLeft & numRight>numLeft |varLeft>varRight & numLeft>numRight, "incongruent", "na")))
  newdata<-newdata%>%
    dplyr::mutate(numRatio = cut(numRatio, breaks = c(0, 1.05, 1.3,2), labels = c("1", "1.2", "1.4")))
  
  ## spit out clean file
  write.csv(newdata,paste("C:/Users/nora/Desktop/jspsych/data/clean_",fileName))
}
  


