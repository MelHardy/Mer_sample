##Import of MERINO sample
library(tidyverse)
test <-read_csv("MERINO-DOOR_sample.csv")
View(test)

##Filter out duplicate data entries in screening_id column by excluding "--1" 
##and "--2"
library(dplyr)
unique_id <- test %>% filter(!grepl('--1|--2',screening_id))
View(unique_id)

##Filter enrolled participants only by including all "1" entries in exclusion
enrolled <- unique_id %>% filter(!grepl('1',exclusion))
View(enrolled)

##Filter enrolled participants to baseline data only by including all rows with "start_patient_arm_1" in redcap_event_name
baseline_data <- enrolled %>% filter(grepl('start_patient_arm_1',redcap_event_name))
View(baseline_data)

##Descriptive statistics
install.packages("jmv")
install.packages("skimr")
install.packages("summarytools")

library(skimr)

summary(baseline_data)
skim(baseline_data)

##age
library(jmv)
descriptives(data=baseline_data, vars=c(age))

##gender
library(summarytools)
baseline_data %>% 
  group_by(gender) %>%
  summarise(sum(gender))

baseline_data$gender <- factor(baseline_data$gender, levels=c(1,2), labels=c("Female","Male"))
freq(baseline_data$gender)

##treatment arm
baseline_data$intervention <- factor(baseline_data$intervention, levels=c(1,2), labels=c("Meropenem","Piperacillin/Tazobactam"))
freq(baseline_data$intervention)

contTables(data=baseline_data, rows=gender, cols=intervention)

##all-cause-mortality at 30 days
baseline_data$death_30days <- factor(baseline_data$death_30days, levels=c(1,2), labels=c("Alive","Dead"))
freq(baseline_data$death_30days)

contTables(data=baseline_data, rows=death_30days, cols=intervention)

##Clinical and microbiologic success at day 4 after randomization
##(survival plus resolution of fever and leukocytosis plus sterilization of blood cultures)

##death at 7 days
baseline_data$death_7days <- factor(baseline_data$death_7days, levels=c(1,2), labels=c("Alive","Dead"))
freq(baseline_data$death_7days)

contTables(data=baseline_data, rows=death_7days, cols=intervention)

##sterilisation of blood cultures
enrolled$intervention <- factor(enrolled$intervention, levels=c(1,2), labels=c("Meropenem","Piperacillin/Tazobactam"))
freq(enrolled$intervention)

enrolled$day_3_blood_culture_result <- factor(enrolled$day_3_blood_culture_result, levels=c(1,2,3), labels=c("Yes","No","Not Collected"))
freq(enrolled$day_3_blood_culture_result)

