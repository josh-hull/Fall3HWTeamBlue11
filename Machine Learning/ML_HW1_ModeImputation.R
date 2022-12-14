##### Peijia 
# Phase I
# import library
library(dplyr)
library(tidyverse)
library(caret)
library(leaps)
library(glmnet)
library(ggplot2)
library(earth)
library(mgcv)
# load data 
ins_t <- read.csv("D:/$$Course$$/NCSU/Fall/AA502/ML/Homework1_ML/insurance_t.csv")
str(ins_t)

#check for missing values
sapply(ins_t, function(x) sum(is.na(x)))
data = ins_t

# Impute missing value 
### categorical variable
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

data$INV[is.na(data$INV)] <- mode(data$INV)
data$CC[is.na(data$CC)] <- mode(data$CC)
sum(is.na(data$INV))
sum(is.na(data$INV))
unique(data$CCPURC)
data$CCPURC[is.na(data$CCPURC)] <- mode(data$CCPURC)
sum(is.na(data$CCPURC))
### continuous variable
library(cowplot)
mis_con <- c("AGE","INCOME","LORES","HMVAL","PHONE","POS","POSAMT","INVBAL",
             "CCBAL","ACCTAGE","CRSCORE")
dat<- data %>% 
  mutate(ACCTAGE.f = ifelse(is.na(data$ACCTAGE), 1, 0)) %>%
  mutate(PHONE.f = ifelse(is.na(data$PHONE), 1, 0)) %>%
  mutate(POS.f = ifelse(is.na(data$POS), 1, 0)) %>%
  mutate(POSAMT.f = ifelse(is.na(data$POSAMT), 1, 0)) %>%
  mutate(INVBAL.f = ifelse(is.na(data$INVBAL), 1, 0)) %>%
  mutate(CCBAL.f = ifelse(is.na(data$CCBAL), 1, 0)) %>%
  mutate(INCOME.f = ifelse(is.na(data$INCOME), 1, 0)) %>%
  mutate(LORES.f = ifelse(is.na(data$LORES), 1, 0)) %>%
  mutate(HMVAL.f = ifelse(is.na(data$HMVAL), 1, 0)) %>%
  mutate(AGE.f = ifelse(is.na(data$AGE), 1, 0)) %>%
  mutate(CRSCORE.f = ifelse(is.na(data$CRSCORE), 1, 0))
# impute median value
for(i in mis_con) {
  dat[ , i][is.na(dat[ , i])] <- median(dat[ , i], na.rm=TRUE)
}
sapply(dat, function(x) sum(is.na(x)))
str(dat)

# only 1 variable exist quasi-completion problem: MMCRED
# deal with quasi-completion problem
table(data$INS,data$MMCRED)
d <- dat %>% 
  mutate(MMCRED = ifelse(MMCRED >= 3, 3, MMCRED))
table(d$MMCRED, d$INS)


## convert categorical variables into factor
varlist <- colnames(ins_t)
cnt <- NULL
for (i in 1:length(varlist)){
  cnt[i]= length(unique(na.omit(ins_t[[i]])))
}
distinct_cnt <- data.frame(varlist,cnt)
continuous <- distinct_cnt$varlist[distinct_cnt$cnt > 10]
continuous <- continuous[-23]
categorical <- distinct_cnt$varlist[distinct_cnt$cnt < 10]
for(i in 1:ncol(d)){
  if(names(d)[i] %in% continuous ){
    next
  } 
  d[,i] <- as.factor(d[,i])
}
str(d)


# EARTH on all variables
mars_all <- earth(INS ~ ., data = d,glm=list(family=binomial))
summary(mars_all)

# Variable importance metric
evimp(mars_all)

# Predictions
pred = predict(mars_all, type="response")

# ROC curve
library(InformationValue)
plotROC(d$INS, pred) ##0.7995


# Select option in GAM to shrink spline variables by penalizing the EDF
gam <- mgcv::gam(INS ~ s(ACCTAGE) +
                       factor(DDA) + 
                       s(DDABAL) +
                       s(DEP,k=5) +
                       s(DEPAMT) +
                       s(CHECKS) + 
                       factor(DIRDEP) +
                       factor(NSF) +
                       s(NSFAMT) + 
                       s(PHONE,k=5) + 
                       s(TELLER) +
                       factor(SAV) + 
                       s(SAVBAL) + 
                       factor(ATM) +
                       s(ATMAMT) + 
                       s(POS) + 
                       s(POSAMT) +
                       factor(CD) + s(CDBAL) +
                       factor(IRA) + 
                       s(IRABAL) + 
                       factor(INV) +
                       s(INVBAL) +
                       factor(MM) +
                       s(MMBAL) +
                       factor(MMCRED)+ 
                       factor(CC) +
                       s(CCBAL) +
                       factor(CCPURC)+
                       factor(SDB) +
                       s(INCOME)+ 
                       s(LORES)+ 
                       s(HMVAL) + 
                       s(AGE) + 
                       s(CRSCORE) + 
                       factor(INAREA)+factor(BRANCH),
                     method = 'REML', select = TRUE, data = d,family = 'binomial')
summary(gam) #R-sq.(adj) =  0.235

# Remaining variables after selection
gam2 <- mgcv::gam(INS ~ s(ACCTAGE) + 
                        s(DDABAL)+
                        s(DEP,k=5) +
                        s(CHECKS) + 
                        s(TELLER) +
                        s(SAVBAL) +
                        s(ATMAMT) +
                        s(CDBAL) +
                        s(CCBAL)+
                        factor(DDA) + 
                        factor(NSF)+
                        factor(ATM)+
                        factor(CD)+
                        factor(IRA)+
                        factor(INV)+
                        factor(MM)+
                        factor(CC)+factor(BRANCH)
                      , method = 'REML', data = d,family = 'binomial')
summary(gam2) #R-sq.(adj) =  0.236 


# Predictions
pred2 = predict(gam2, type="response")

# ROC curve
library(InformationValue)
plotROC(d$INS, pred2) #0.7973
