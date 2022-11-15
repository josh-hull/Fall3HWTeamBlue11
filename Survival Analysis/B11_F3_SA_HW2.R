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
data = read.csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv",header = T)
sample = data[,c("survive","hour","reason")]
#check missing value
sapply(data, function(x) sum(is.na(x)))

#Target variabels: Hour, survive
#check & correct target variables:
data %>% select(reason, hour,survive) %>%filter(hour==48)
dat <- data %>% mutate(flood_fail = ifelse(reason==1,1,0))
dat$index = 1:nrow(dat)


###################### AFT Model Building  #####################################
#Use only main effects in this model (No interactions and no H1-H48 variables).
#Select an appropriate distribution for your model using both 
#graphical approaches and statistical tests
## remove reason2, h1-h48 
d <- dat[,-c(9:57,59:60)]

#check multicollinearity first
vars <- c('backup', "bridgecrane", "servo", "gear", "trashrack", "elevation","flood_fail","age","slope","hour")
library(corrplot)
mydata <- d[vars]
mydata.cor = cor(mydata, method = c("spearman"))
corrplot(mydata.cor)
# use VIF to check 
library(car)
h.lm = lm(flood_fail ~., data=d)
vif(h.lm) # all <10


#check distribution -- graphically 
### weibull distribution
flood.aft.w <- flexsurvreg(Surv(hour, flood_fail) ~ backup+age+bridgecrane+servo+gear+trashrack
                           +slope+elevation, data = d, dist = "weibull")

plot(flood.aft.w, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour (0-48)", ylab = "Cumulative Hazard", main = "Weibull Distribution")


### exponential distribution
flood.aft.e <- flexsurvreg(Surv(hour, flood_fail) ~ backup+age+bridgecrane+servo+gear+trashrack
                           +slope+elevation, data = d, dist = "exp")

plot(flood.aft.e, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Exponential Distribution")


### gamma distribution
flood.aft.g <- flexsurvreg(Surv(hour, flood_fail) ~ backup+age+bridgecrane+servo+gear+trashrack
                           +slope+elevation, data = d, dist = "gamma")

plot(flood.aft.g, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Gamma Distribution")

### log-logistic distribution
flood.aft.ll <- flexsurvreg(Surv(hour, flood_fail) ~ backup+age+bridgecrane+servo+gear+trashrack
                            +slope+elevation, data = d, dist = "llogis")

plot(flood.aft.ll, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Log-Logistic Distribution")

### log-normal distribution
flood.aft.ln <- flexsurvreg(Surv(hour, flood_fail) ~ backup+age+bridgecrane+servo+gear+trashrack
                            +slope+elevation, data = d,dist = "lognormal")

plot(flood.aft.ln, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Log-Normal Distribution")
# prefer: gammar, weibull, log-logistic

#check distribution -- statistical test
# Goodness-of-Fit Tests 
like.e = flood.aft.e$loglik
like.w <- flood.aft.w$loglik
like.ln <- flood.aft.ln$loglik
like.g = flood.aft.g$loglik
like.ll = flood.aft.ll$loglik
print(paste(like.e,like.w,like.ln,like.g,like.ll))

pval.e.w = pchisq((-2*(like.e-like.w)), 1,lower.tail=F)
pval.w.g = pchisq((-2*(like.w-like.g)), 1,lower.tail=F)
pval.ln.g = pchisq((-2*(like.ln-like.g)), 1,lower.tail=F)
pval.e.g = pchisq((-2*(like.e-like.g)), 1,lower.tail=F)

Tests = c('Exp vs. Weibull','Wei vs. Gam', 'LogN vs. Gam','Exp vs. Gam')
P_values = c(pval.e.w,pval.w.g, pval.ln.g,pval.e.g)
cbind(Tests, P_values) # select: Weibull 

# Modeling & variable selection
full.aft.ln = survreg(Surv(hour, flood_fail) ~ backup+bridgecrane+servo+gear+trashrack
                      +elevation+age+slope, data = d, dist = 'weibul')
summary(full.aft.ln)
## empty model
empty.model <- survreg(Surv(hour, flood_fail) ~ 1, data = d, dist = 'weibul')

# backward selection @ alpha=0.03
alpha = 0.03 
back.model <- step(full.aft.ln, scope=list(lower=empty.model,upper=full.aft.ln),
                   direction = "backward",
                   k=qchisq(alpha, 1, lower.tail = FALSE))
summary(back.model)

#survreg(formula = Surv(hour, flood_fail) ~ backup + servo + slope, 
#        data = d, dist = "weibul")

#Building final model
final.model <- survreg(formula = Surv(hour, flood_fail) ~ backup + servo + slope,
                       data = d, dist = "weibul")
summary(final.model)

survprob.actual = 1 - psurvreg(d$hour, mean = predict(final.model, type = "lp"), scale = final.model$scale, distribution=final.model$dist)

#### Upgrade Impact and Cost Optimization ($2.5 MM Budget) #####################
new_time.backup = qsurvreg(1 - survprob.actual, mean = predict(final.model, type = "lp") + coef(final.model)['backup'], scale = final.model$scale, distribution = final.model$dist)

d$new_time.backup = new_time.backup
d$diff.backup = d$new_time.backup - d$hour

#add servo
new_time.servo = qsurvreg(1 - survprob.actual, mean = predict(final.model, type = "lp") + coef(final.model)['servo'], scale = final.model$scale, distribution = final.model$dist)
d$new_time.servo = new_time.servo
d$diff.servo = d$new_time.servo - d$hour

#creating dataframe for determining upgrade
upgrade <- d %>% filter(flood_fail==1)
upgrade <- upgrade %>% mutate(new_time.backup = ifelse(new_time.backup >= 48, 48, new_time.backup)) %>% mutate(new_time.servo = ifelse(new_time.servo >= 48, 48, new_time.servo))
upgrade <- upgrade %>% mutate(diff.backup = new_time.backup-hour) %>% mutate(diff.servo = new_time.servo-hour)
upgrade <- upgrade %>% mutate(backup_upgrade_value = (upgrade$diff.backup)/10) %>% mutate(servo_upgrade_value = (upgrade$diff.servo)/15)
upgrade <- upgrade %>% filter(backup==0 | servo==0)
upgrade$value <- 0
upgrade <- upgrade %>% mutate(value = ifelse(backup==1 & servo==0, servo_upgrade_value, value))
upgrade <- upgrade %>% mutate(value = ifelse(backup==0 & servo==1, backup_upgrade_value, value))
upgrade <- upgrade %>% mutate(value = ifelse(backup==0 & servo==0 & backup_upgrade_value>servo_upgrade_value, backup_upgrade_value, value))
upgrade <- upgrade %>% mutate(value = ifelse(backup==0 & servo==0 & backup_upgrade_value<servo_upgrade_value, servo_upgrade_value, value))
upgrade <- upgrade %>% mutate(upgrade_type = ifelse(value==backup_upgrade_value, 'backup', 'servo'))
upgrade <- upgrade %>% mutate(cost = ifelse(upgrade_type=='backup', 100, 150))
upgrade <- upgrade %>% mutate(impact = ifelse(upgrade_type=='backup', diff.backup, diff.servo))

#simplifying to relevant columns
upgrade_simple <- upgrade %>% select(index, hour, upgrade_type, impact, value, cost, backup, servo) %>% arrange(-value)

#identifying pumps for upgrade
sum(upgrade_simple$cost[1:18]) + sum(upgrade_simple$cost[21]) + sum(upgrade_simple$cost[24])
sum(upgrade_simple$impact[1:18]) + sum(upgrade_simple$impact[21]) + sum(upgrade_simple$impact[24])
upgrade_list <- rbind(upgrade_simple[1:18,], upgrade_simple[21,], upgrade_simple[24,])






