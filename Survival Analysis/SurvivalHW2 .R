#Mahad Munawar
#Survival HW 2
#Blue Team 11
#11/14/22

library(survival)
library(dplyr)
library(survminer)
library(flexsurv)

hurricane <- read.csv("https://raw.githubusercontent.com/sjsimmo2/Survival/master/hurricane.csv")

#add target variable named flood to mark whether the failure reason = 1 which is flood 
hurricane$flood <- ifelse(hurricane$reason == 1, 1, 0)

#remove h1-h48
hurricane1 <- hurricane[ -c(9:57, 59:60) ]

hurricane_aft_ln <- survreg(Surv(hour, flood) ~ ., data = hurricane1, dist = 'lognormal')
summary(hurricane_aft_ln)

#checking distributions

#weibull


hurricane.aft.w <- flexsurvreg(Surv(hour, flood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation  , data = hurricane1, dist = 'weibull')
plot(hurricane.aft.w, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", xlab = "week", ylab = "Cumulative Hazard", main = "Weibull Distribution")

hurricane.aft.e <- flexsurvreg(Surv(hour, flood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation  , data = hurricane1, dist = 'exp')
plot(hurricane.aft.e, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", xlab = "week", ylab = "Cumulative Hazard", main = "Exponential Distribution")

hurricane.aft.w <- flexsurvreg(Surv(hour, flood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation   , data = hurricane1, dist = 'gamma')
plot(hurricane.aft.w, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", xlab = "week", ylab = "Cumulative Hazard", main = "Gamma Distribution")

hurricane.aft.ll <- flexsurvreg(Surv(hour, flood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation, data = hurricane1, dist = 'llogis')
plot(hurricane.aft.ll, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", xlab = "week", ylab = "Cumulative Hazard", main = "Log Logistic Distribution")

