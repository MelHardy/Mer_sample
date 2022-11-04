##Test with 6 participants and 2 binary endpoints (mortality at 30 days and clinical/micro response) 
##and 2 columns with time to event endpoints (death or clinical/micro response)
install.packages("tidyverse")
install.packages("summarytools")
library(tidyverse)
library(summarytools)

##Read in test data file with columns id, intervention (control 1,treatment 2), mortality (binary,1,0), 
##clinical micro response (binary, 1,0) with non-sequential id's.
data <-read_csv("wrtest_unseq.csv")

##Set up vector for first endpoint
wrtest <- c()

##Create a loop for the first endpoint and calculate wins (1), losses (-1) and draws (0)
##this script calculates wins/losses on the single TTE endpoint
for (i in data$id[which(data$intervention == 1)]){
  for (j in data$id[which(data$intervention == 2)]){ 
    if (data$days_mortality[data$id == i]>data$days_mortality[data$id == j]){
      wrtest <- c(wrtest,1)}
    else if (data$days_mortality[data$id == i]<data$days_mortality[data$id == j]){
      wrtest <- c(wrtest,-1)}
    else if (data$days_mortality[data$id == i]==data$days_mortality[data$id == j]){
      wrtest <- c(wrtest,0)}
  }
  wrtest  
}

mortality <-data.frame(wrtest)

mortality$wrtest <- factor(mortality$wrtest, levels=c(-1,0,1), labels=c("loss", "draw", "win"))
freq(mortality$wrtest)

##Set up a second vector for a second endpoint
wrtest2 <- c()

##Create a loop that calculate the wins (2), losses (-2) and draws (0) for the second TTE endpoint 
##clinical/microbilogical response
for (i in data$id[which(data$intervention == 1)]){
  for (j in data$id[which(data$intervention == 2)]){ 
    if(data$days_mortality[data$id == i]==data$days_mortality[data$id == j]){{
      if(data$days_cmresponse[data$id == i]>data$days_cmresponse[data$id == j]){
        wrtest2 <- c(wrtest2,2)}
      else if(data$days_cmresponse[data$id == i]<data$days_cmresponse[data$id == j]){
        wrtest2 <- c(wrtest2,-2)}
      else if(data$days_cmresponse[data$id == i]==data$days_cmresponse[data$id == j])
        wrtest2 <- c(wrtest2,0)}}
  }
  wrtest2  
}

tt_cmresponse <-data.frame(wrtest2)

tt_cmresponse$wrtest <- factor(tt_cmresponse$wrtest, levels=c(-2,0,2), labels=c("loss", "draw", "win"))
freq(tt_cmresponse$wrtest)
