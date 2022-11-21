### Survival Analysis HW2 ### 

#imoprt library
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

# load in dataset
hurricane = read.csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv",header = T)
#check missing value
sapply(data, function(x) sum(is.na(x)))

#Target variabels: Start, Stop, motor_failure, time-dependent: 12hours indicator

#check & correct target variables:
data <- hurricane %>% mutate(motor_fail = ifelse(reason==2,1,0))

# Variable Selection 
full.model <- coxph(Surv(hour, motor_fail) ~ backup + age + 
                      bridgecrane + servo + gear + trashrack + slope + elevation, 
                    data = data)

back.model <- step(full.model, direction = "backward")
summary(back.model)
#coxph(formula = Surv(hour, motor_fail) ~ age + servo + trashrack + slope, data = data)


# create Pump variable - Prepare data for modeling
data$Pump <- seq(1,nrow(data))

# make data long
m.long = pivot_longer(data, cols=h1:h48, names_to="hvars", values_to="status")
m.long = m.long %>% 
  rowwise() %>%
  mutate(hvars = gsub("h","", hvars))

# censor observations past failure hour
m.long$hvars = as.numeric(m.long$hvars)
m.long.censored = m.long %>% filter(hvars <= hour)

# create start and stop variables 
dat <- m.long.censored %>% group_by(Pump) %>% 
  mutate(start = seq(0,length(hvars)-1)) %>%
  mutate(stop = seq(1,length(hvars))) %>%
  select(-reason2,-survive)

# correct missing value based on "hour"
#dat <- dat%>%filter(is.na(status)) %>%
dat$status[is.na(dat$status)==TRUE] = 0
#is.na(dat$status)
# create 12 hours indicator variable
# the 12 hours indicator created based on if the status is consecutively being 1 for every 12 hours before that point
# the indicator of every time point is 1 if the previous 12 hours are constantly being 1 and
# the indicator is 0 if the previous 12 hours are not constantly being 1
# For example, indicator of h13 -- looking at if the status of h2-h13 are all 1
indicator <- NULL
for (i in 1:length(dat$hvars)) {
  if (i<12){
    dat$indicator[i] = 0
  } else {
    dat$indicator[i] = ifelse(sum(dat$status[(i-11):i])==12,1,0)
  }
}

# Build model
pump.ph2 <- coxph(Surv(start, stop, motor_fail) ~ age + factor(servo) + factor(trashrack) + slope+indicator, data = dat)
summary(pump.ph2)


# Check assumption 
# linearity - age, slope
pump.ph <- coxph(Surv(start,stop,motor_fail) ~ age + servo + trashrack + slope, data = dat)

visreg(pump.ph, "age", xlab = "age", ylab = "partial residuals",gg = TRUE, band = FALSE) +  
  geom_smooth(col = "red", fill = "red") + theme_bw() 
visreg(pump.ph, "slope", xlab = "Slope", ylab = "partial residuals",gg = TRUE, band = FALSE) +  
  geom_smooth(col = "red", fill = "red") + theme_bw()
## Martingale residuals...Linearity
pump.lin <- coxph(Surv(start,stop,motor_fail) ~  age + slope, data = dat)
survminer::ggcoxfunctional(pump.lin,data=data)

# check PH assumption
# Proportional Hazard Test - Schoenfeld Residuals 
pump.ph.zph <- cox.zph(pump.ph, transform = "identity")
pump.ph.zph

dat %>% filter(stop <= start) %>%select(Pump,start, stop)
