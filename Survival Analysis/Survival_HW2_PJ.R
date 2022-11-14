# HW2 

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

#Target variabels: Hour, flood_fail
#check & correct target variables:
data %>% select(reason, hour,survive) %>%filter(hour==48)
dat <- data %>% mutate(flood_fail = ifelse(reason==1,1,0))


######################################## HW2  ##############################################
#Use only main effects in this model (No interactions and no H1-H48 variables).
#Select an appropriate distribution for your model using both 
#graphical approaches and statistical tests
## remove reason2, h1-h48 
d <- dat[,-c(9:57,60)]

#check multicollinearity first
vars <- c("reason",'backup', "bridgecrane", "servo", "gear", "trashrack", "elevation","flood_fail","age","slope","hour")
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
                          +slope+elevation+reason, data = d, dist = "weibull")
plot(flood.aft.w, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Weibull Distribution")

### exponential distribution
flood.aft.e <- flexsurvreg(Surv(hour, flood_fail) ~ backup+age+bridgecrane+servo+gear+trashrack
                           +slope+elevation+reason, data = d, dist = "exp")

plot(flood.aft.e, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Exponential Distribution")

### gamma distribution
flood.aft.g <- flexsurvreg(Surv(hour, flood_fail) ~ backup+age+bridgecrane+servo+gear+trashrack
                           +slope+elevation+reason, data = d, dist = "gamma")

plot(flood.aft.g, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Gamma Distribution")

### log-logistic distribution
flood.aft.ll <- flexsurvreg(Surv(hour, flood_fail) ~ backup+age+bridgecrane+servo+gear+trashrack
                            +slope+elevation+reason, data = d, dist = "llogis")

plot(flood.aft.ll, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Log-Logistic Distribution")

### log-normal distribution
flood.aft.ln <- flexsurvreg(Surv(hour, flood_fail) ~ backup+age+bridgecrane+servo+gear+trashrack
                            +slope+elevation+reason, data = d,dist = "lognormal")

plot(flood.aft.ln, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "Cumulative Hazard", main = "Log-Normal Distribution")
# prefer: gammar, weibull, log-logistic, log-normal

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
full.aft.ln = survreg(Surv(hour, flood_fail) ~ reason+backup+bridgecrane+servo+gear+trashrack
                       +elevation+age+slope, data = d, dist = 'weibull')
summary(full.aft.ln)
## empty model
empty.model <- survreg(Surv(hour, flood_fail) ~ 1, data = d, dist = 'weibull')

# backward selection @ alpha=0.03
alpha = 0.03 
back.model <- step(full.aft.ln, scope=list(lower=empty.model,upper=full.aft.ln),
                     direction = "backward",
                   k=qchisq(alpha, 1, lower.tail = FALSE))
summary(back.model)

#survreg(formula = Surv(hour, flood_fail) ~ backup + servo + slope, 
#        data = d, dist = "weibul")

model.w <- survreg(formula = Surv(hour, flood_fail) ~ backup + servo + slope,
                     data = d, dist = "weibul")
summary(model.w)

#Include a table of significant variables ranked by p-value.
#Interpret the effects of the most significant variable.

# Q2
survprob.actual = 1 - psurvreg(d$hour,
                               mean = predict(model.w, type = "lp"),
                               scale = model.w$scale, distribution =model.w$dist)
head(survprob.actual, n = 10)

# Predicted Change in Event Time -- backup, servo, slope
## backup
new_time.backup = qsurvreg(1 - survprob.actual,
                    mean = predict(model.w, type = "lp") +
                      coef(model.w)['backup'],
                    scale = model.w$scale,
                    distribution = model.w$dist)

d$new_time.backup = new_time.backup
d$diff.backup = d$new_time.backup - d$hour

impact.backup=data.frame(d$hour, d$new_time.backup, d$diff.backup,d$flood_fail,d$backup)
colnames(impact.backup)=c("O.time","N.time","Diff.backup","Flood_fail","Backup")

impact.backup2=subset(impact.backup,Flood_fail==1 & Backup==0)
head(impact.backup2)

## servo
new_time.servo = qsurvreg(1 - survprob.actual,
                          mean = predict(model.w, type = "lp") +
                            coef(model.w)['servo'],
                          scale = model.w$scale,
                          distribution = model.w$dist)
d$new_time.servo = new_time.servo
d$diff.servo = d$new_time.servo - d$hour

impact.servo=data.frame(d$hour, d$new_time.servo, d$diff.servo,d$flood_fail,d$servo)
colnames(impact.servo)=c("O.time","N.time","Diff.servo","Flood_fail","servo")

impact.servo2=subset(impact.servo,Flood_fail==1 & servo==0)
head(impact.servo2)


# pumps need to be upgrade
upgrade <- d %>% filter(reason == 1)

# upgrade which variable for each pump 
### upgrade servo or backup
a <- upgrade %>% filter(backup==0 & servo==0) %>% select(backup,servo,new_time.backup,new_time.servo,diff.backup,diff.servo)
sel_bp <- a %>% filter(new_time.backup > 48) 
sel_ser <- a%>%anti_join(sel_bp,by="new_time.backup")
### upgrade backup –- $100K
b = upgrade %>% filter(backup==0 & servo==1) %>% select(backup,servo,new_time.backup,new_time.servo,diff.backup,diff.servo)
b %>% filter(new_time.backup > 48)
### upgrade servo -- $150K
c = upgrade %>% filter(backup==1 & servo==0) %>% select(backup,servo,new_time.backup,new_time.servo,diff.backup,diff.servo)
c %>% filter(new_time.servo > 48)

#2.5m=2500