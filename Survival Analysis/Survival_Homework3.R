# Import Libraries
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

# Read in Hurricane Dataset
hurricane = read.csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv",header = T)

#check missing value
sapply(data, function(x) sum(is.na(x)))

## NOTES:
# Target variables: Start, Stop, motor_failure
# Main effects: age, servo, backup, slope, trashrack, bridgecrane, gear, elevation
# Time-dependent variable: 12hours indicator

# Create target variable for motor failure
data <- hurricane %>% mutate(motor_fail = ifelse(reason==2,1,0))

##### Check Assumptions ########################################################
full.model <- coxph(Surv(hour, motor_fail) ~ backup + age + 
                      factor(bridgecrane) + factor(servo) + factor(gear) + 
                      slope + elevation, 
                    data = data)

### Check linearity assumption of continuous variables
visreg(full.model, "age", xlab = "age", ylab = "partial residuals",
       gg = TRUE, band = FALSE) +  
  geom_smooth(col = "red", fill = "red") + 
  theme_bw() 

visreg(full.model, "slope", xlab = "Slope", ylab = "partial residuals",
       gg = TRUE, band = FALSE) +  
  geom_smooth(col = "red", fill = "red") + 
  theme_bw()

## Martingale residuals 
pump.lin <- coxph(Surv(hour,motor_fail) ~  age + slope, data = data)
survminer::ggcoxfunctional(pump.lin,data=data)

### Check proportional hazard assumption using Schoenfeld Residuals 
pump.ph.zph <- cox.zph(full.model, transform = "identity")
pump.ph.zph

##### Variable Selection of Main Effects #######################################
back.model <- step(full.model, direction = "backward", 
                   k = qchisq(0.03, 1, lower.tail = FALSE))
summary(back.model)


##### Create Time Dependent Variable (Running for 12 hours) ####################
# create index variable for each of the 770 pumps
data$Pump <- seq(1,nrow(data))

# make data long (row per hour)
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
dat$status[is.na(dat$status)==TRUE] = 0

# create indicator variable for running 12 or more hours
## NOTES:
## 0 if motor running for 11 hours or less 
## 1 when running for 12 consecutive hours prior to failure time
## 1 every consecutive hour after 12
indicator <- NULL
for (i in 1:length(dat$hvars)) {
  if (i<12){
    dat$indicator[i] = 0
  } else {
    dat$indicator[i] = ifelse(sum(dat$status[(i-11):i])==12,1,0)
  }
}


###### New PH Model (now including indicator) ##################################
# Select main effects model
new.model <- coxph(Surv(start, stop, motor_fail) ~ 
                     age + factor(servo) + slope + factor(indicator), 
                    data = dat)

summary(new.model)



