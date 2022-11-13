# Libraries
library(survival)
library(survminer)
library(dplyr)
library(foreign)
library(ggplot2)
library(rms)
library(flexsurv)
library(ciTools)
library(here)
library(visreg)
library(cmprsk)

# Read in the data
hurricane = read.csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv")

# Create flood failure variable
library(dplyr)
hurricane = hurricane %>%
  mutate(flood_failure = ifelse(reason == 1, 1, 0))

# Subset hurricane data set to remove H1-H48, survive, reason, reason2
hurricane2 = hurricane[,-c(9:57, 59:60)]

# Check multicollinearity
library(car)
h.lm = lm(flood_failure ~., data=hurricane2)
vif(h.lm) # all <1.5, good

# Check Weibull distribution
aft.w <- flexsurvreg(Surv(hour, flood_failure) ~ backup + age + 
                             bridgecrane + servo + gear + trashrack +
                             slope + elevation + hour, 
                           data = hurricane2, dist = "weibull")

plot(aft.w, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Weibull Distribution")

# Check exponential distribution
aft.e <- flexsurvreg(Surv(hour, flood_failure) ~ backup + age + 
                             bridgecrane + servo + gear + trashrack +
                             slope + elevation + hour, 
                           data = hurricane2, dist = "exp")

plot(aft.e, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Exponential Distribution")


# Check gama distribution
aft.g <- flexsurvreg(Surv(hour, flood_failure) ~ backup + age + 
                             bridgecrane + servo + gear + trashrack +
                             slope + elevation + hour, 
                           data = hurricane2, dist = "gamma")

plot(aft.g, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Gamma Distribution")


# Check log-logistic distribution
aft.ll <- flexsurvreg(Surv(hour, flood_failure) ~ backup + age + 
                             bridgecrane + servo + gear + trashrack +
                             slope + elevation + hour, 
                           data = hurricane2, dist = "llogis")

plot(aft.ll, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Log-Logistic Distribution")


# Check log-normal distribution
aft.ln <- flexsurvreg(Surv(hour, flood_failure) ~ backup + age + 
                              bridgecrane + servo + gear + trashrack +
                              slope + elevation + hour, 
                            data = hurricane2, dist = "lognormal")

plot(aft.ln, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Log-Normal Distribution")


# Get log-liklihood for distributions
like.w = aft.w$loglik # -465.3174
like.e = aft.e$loglik # -639.511
like.g = aft.g$loglik # 486.462
like.ll = aft.ll$loglik # -474.0128
like.ln = aft.ln$loglik # -496.1132

# Statistical tests of nested models
pval.e.g = pchisq((-2*(like.e-like.g)), 2,lower.tail=F)
pval.w.g = pchisq((-2*(like.w-like.g)), 1,lower.tail=F)
pval.w.e = pchisq((-2*(like.w-like.e)), 1,lower.tail=F)
pval.ln.g = pchisq((-2*(like.ln-like.g)), 1,lower.tail=F)

Tests = c('Exp vs. Gam', 'Wei vs. Gam', 'Wei vs Exp', 'LogN vs. Gam')
P_values = c(pval.e.g, pval.w.g, pval.w.e, pval.ln.g)
cbind(Tests, P_values)

# Variable selection
summary(aft.e)




