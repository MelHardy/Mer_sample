##Test with 6 participants and 2 binary endpoints (mortality at 30 days and clinical/micro response) 
##and 2 columns with time to event endpoints (death or clinical/micro response)
install.packages("tidyverse")
install.packages("summarytools")
library(tidyverse)
library(summarytools)

##Read in test data file with columns id, intervention (control 1,treatment 2), mortality (binary,1,0), clinical micro response (binary, 1,0)
data <-read_csv("wrtest.csv")

##Set up vector for first endpoint
wrtest <- c()

##Create a loop for the first binary endpoint and calculate wins (1), losses (-1) and draws (0)
for (id.control in data$id[which(data$intervention == 1)]){
  for (id.treatment in data$id[which(data$intervention == 2)]){ 
    if(data$mortality_30d[id.control]<data$mortality_30d[id.treatment]){
      wrtest <- c(wrtest,-1)}
    else if(data$mortality_30d[id.control]>data$mortality_30d[id.treatment]){
      wrtest <- c(wrtest,1)}
    else if(data$mortality_30d[id.control]==data$mortality_30d[id.treatment]){
      wrtest <- c(wrtest,0)}
  }
  wrtest  
}

mortality <-data.frame(wrtest)

mortality$wrtest <- factor(mortality$wrtest, levels=c(-1,0,1), labels=c("loss", "draw", "win"))
freq(mortality$wrtest)

##Set up a second vector for a second endpoint
wrtest2 <- c()

##Create a loop for the second binary endpoint that calculates the wins (2), losses (-2)
##and draws (0) for the second endpoint clinical/microbilogical response
for (id.control in data$id[which(data$intervention == 1)]){
  for (id.treatment in data$id[which(data$intervention == 2)]){ 
    if(data$mortality_30d[id.control]==data$mortality_30d[id.treatment]){{
      if(data$cm_response[id.control]<data$cm_response[id.treatment]){
        wrtest2 <- c(wrtest2,2)}
      else if(data$cm_response[id.control]>data$cm_response[id.treatment]){
        wrtest2 <- c(wrtest2,-2)}
      else if(data$cm_response[id.control]==data$cm_response[id.treatment])
        wrtest2 <- c(wrtest2,0)}}
  }
  wrtest2  
}

cm_response <-data.frame(wrtest2)

cm_response$wrtest <- factor(cm_response$wrtest, levels=c(-2,0,2), labels=c("loss", "draw", "win"))
freq(cm_response$wrtest)

