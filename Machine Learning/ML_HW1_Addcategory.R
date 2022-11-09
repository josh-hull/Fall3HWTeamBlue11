### Peijia
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

#check variable type
varlist <- colnames(ins_t)
cnt <- NULL
for (i in 1:length(varlist)){
  cnt[i]= length(unique(na.omit(ins_t[[i]])))
}
distinct_cnt <- data.frame(varlist,cnt)
distinct_cnt %>% arrange(desc(cnt))
binary <- distinct_cnt$varlist[distinct_cnt$cnt==2]
continuous <- distinct_cnt$varlist[distinct_cnt$cnt > 10]
continuous <- continuous[-23]
ordinal <- distinct_cnt$varlist[distinct_cnt$cnt < 10 & distinct_cnt$cnt >2]
categorical <- c(binary, ordinal)

#check for missing values
sapply(ins_t, function(x) sum(is.na(x)))
data = ins_t

# Impute missing value 
### categorical variable
data$INV[is.na(data$INV)] <- 'M'
data$CC[is.na(data$CC)] <- 'M'
data$CCPURC[is.na(data$CCPURC)] <- 'M'


### continuous variable

# create flag for continuous variable with missing value
mis_con <- c("AGE","INCOME","LORES","HMVAL","PHONE","POS","POSAMT","INVBAL",
             "CCBAL","ACCTAGE","CRSCORE")
flag <- data %>% 
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
d <- flag %>% 
  mutate_if(is.numeric, funs(replace(., is.na(.), median(., na.rm = TRUE))))


sapply(d, function(x) sum(is.na(x)))
str(d)

# check for quasi-completion or completion
df_cat <- data[categorical] 
col_name <- setNames(data.frame(colnames(df_cat), rep(0,length(colnames(df_cat)))), c("col","sep"))

for (i in 1:ncol(df_cat)){
  tally <- as.data.frame(table(df_cat[,i],df_cat$INS))
  len <- length(unique(df_cat[,i]))
  if (sum(tally$Freq == 0) == 0) {
    col_name[i,2] <- "None"
  } else if (sum(tally$Freq == 0) == len) {
    col_name[i,2] <- "Complete"
  } else {
    col_name[i,2] <- "Quasi"
  }
}
q_c <- col_name %>% filter(sep != 'None')
# only 1 variable exist quasi-completion problem: MMCRED
# deal with quasi-completion problem
table(data$INS,data$MMCRED)
train <- d %>% 
  mutate(MMCRED = ifelse(MMCRED >= 3, 3, MMCRED))
table(train$MMCRED, train$INS)

# convert categorical variables into factor
for(i in 1:ncol(train)){
  if(names(train)[i] %in% continuous ){
    next
  } 
  train[,i] <- as.factor(train[,i])
}
str(train)
train$BRANCH <- as.factor(train$BRANCH)

# EARTH on all variables
mars_all <- earth(INS ~ ., data = train,glm=list(family=binomial))
summary(mars_all)

# Variable importance metric
evimp(mars_all)

# Predictions
pred = predict(mars_all, type="response")

# ROC curve
library(InformationValue)
plotROC(train$INS, pred)


# Select option in GAM to shrink spline variables by penalizing the EDF
sel.gam <- mgcv::gam(INS ~ s(ACCTAGE) +
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
                       factor(INAREA),
                      method = 'REML', select = TRUE, data = train,family = 'binomial')
summary(sel.gam) #R-sq.(adj) =  0.239 

# Remaining variables after selection
sel.gam2 <- mgcv::gam(INS ~ s(ACCTAGE) + 
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
                        factor(CC)
                      , method = 'REML', data = train,family = 'binomial')
summary(sel.gam2)#R-sq.(adj) =   0.24

# Predictions
pred2 = predict(sel.gam2, type="response")

# ROC curve
library(InformationValue)
plotROC(train$INS, pred2)
