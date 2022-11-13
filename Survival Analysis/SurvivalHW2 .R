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


like.e = flexsurvreg(Surv(hour, flood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation , data = hurricane1, dist = "exp")$loglik
like.w <- flexsurvreg(Surv(hour, flood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation  , data = hurricane1, dist = 'weibull')$loglik
like.ln <- flexsurvreg(Surv(hour, flood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation  , data = hurricane1, dist = "lnorm")$loglik
like.g =flexsurvreg(Surv(hour, flood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation  , data = hurricane1, dist = "gamma")$loglik
like.ll = flexsurvreg(Surv(hour, flood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation  , data = hurricane1,dist = "llogis")$loglik


pval.e.g = pchisq((-2*(like.e-like.g)), 2,lower.tail=F)
pval.w.g = pchisq((-2*(like.w-like.g)), 1,lower.tail=F)
pval.ln.g = pchisq((-2*(like.ln-like.g)), 1,lower.tail=F)
Tests = c('Exp vs. Gam', 'Wei vs. Gam', 'LogN vs. Gam') 


P_values = c(pval.e.g, pval.w.g, pval.ln.g)
cbind(Tests, P_values)

#####full model
hurricane.aft.w = survreg(Surv(hour, flood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation  , data = hurricane1, dist = 'weibull')
summary(hurricane.aft.w)

#backwards selection
#full model. Using hurricane.aft.w in its place
# full.aft.w = survreg(Surv(hour, flood) ~ backup + age + bridgecrane + servo + gear + trashrack + slope + elevation
#                       , data = hurricane1, dist = 'weibul')
# summary(full.aft.ln)


######empty model
empty.model <- survreg(Surv(hour, flood) ~ 1, data = hurricane1, dist = 'weibul')

#run f
alpha = 0.03 
back.model <- step(hurricane.aft.w, scope=list(lower=empty.model,upper=hurricane.aft.w),
                   direction = "backward",
                   k=qchisq(alpha, 1, lower.tail = FALSE))
summary(back.model)

######selected model########
#survreg(formula = Surv(hour, flood) ~ backup + servo + slope, data = hurricane1, dist = "weibull")