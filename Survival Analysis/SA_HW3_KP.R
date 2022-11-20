# import libraries
library(survival)
library(foreign)
library(ggplot2)
library(survminer)
library(rms)
library(flexsurv)
library(dplyr)
library(tidyverse)
library(ciTools)
library(here)
library(visreg)
library(cmprsk)

# read in data
df = read.csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv")

# subset to motor failure
df.motor = df %>% filter(reason == 2)

# make data long
df.motor$id = seq(1,112)

df.long = pivot_longer(df.motor, cols=h1:h48, names_to="hvars", values_to="status")
df.long = df.long %>% 
  rowwise() %>%
  mutate(hvars = gsub("h","", hvars))
  
# censor observations past failure hour
df.long$hvars = as.numeric(df.long$hvars)
df.long.censored = df.long %>% filter(hvars <= hour & !is.na(status))

# create failure variable (1 at time of failure)
df.long.censored = df.long.censored %>%
  group_by(id) %>%
  mutate(fail = ifelse(hvars == hour, 1, 0))

df.long.censored = df.long.censored %>% 
  group_by(id) %>%
  mutate(
    start  = row_number()-1,
    stop   = row_number()) %>%
  arrange(id, stop)


# create function to count 12 consecutive hours and reset at 0
CountRunning <- function(status_list){
  count = 0
  count_list = c()
  
  for (i in status_list){
    if (i==0){
      if (count!=0){
        count <- 0
      }
    }
    else{
      count <- count + 1
    }
    count_list <- append(count_list, c(count))
  }
  return(count_list)
}


# apply function to long data frame
df.long.count <- df.long.censored %>% 
  group_by(id) %>%
  mutate(consec_run = CountRunning(status)) %>%
  mutate(flag12 = ifelse(consec_run >= 12, 1, 0))

# select variables and reorder
df.final = df.long.count %>%
  select(id, backup:survive, hour, reason, hvars:stop, fail:flag12)

# export csv
write.csv(df.final, "/Users/kelsypeil/Desktop/AA502/Survival Analysis/survhw3.csv")
