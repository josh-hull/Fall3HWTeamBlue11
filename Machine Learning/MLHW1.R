#Mahad Munawar
#11/11/22
#ML HW 1 

library(tidyverse)
#reading in training dataset
training <- read.csv("/Users/mahadmunawar/Downloads/Homework1_ML/insurance_t.csv")

#total number of NA values
sum(is.na(training))

#NA values per column
na_count <-sapply(training, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

#creating missing categories for categorical variables
training$INV <- as.character(training$INV)  %>% replace_na('M')
training$CCPURC <- as.character(training$CCPURC)  %>% replace_na('M')

#creates columns with binary indicators whether there were NA's in corresponding columns
training1 <- training %>% 
  mutate(missing_ACCTAGE = ifelse(is.na(training$ACCTAGE), 1, 0)) %>%
  mutate(missing_PHONE = ifelse(is.na(training$PHONE), 1, 0)) %>%
  mutate(missing_POS = ifelse(is.na(training$POS), 1, 0)) %>%
  mutate(missing_POSAMT = ifelse(is.na(training$POSAMT), 1, 0)) %>%
  mutate(missing_INV = ifelse(is.na(training$INV), 1, 0)) %>%
  mutate(missing_INVBAL = ifelse(is.na(training$INVBAL), 1, 0)) %>%
  mutate(missing_CC = ifelse(is.na(training$CC), 1, 0)) %>%
  mutate(missing_CCBAL = ifelse(is.na(training$CCBAL), 1, 0)) %>%
  mutate(missing_CCPURC = ifelse(is.na(training$CCPURC), 1, 0)) %>%
  mutate(missing_INCOME = ifelse(is.na(training$INCOME), 1, 0)) %>%
  mutate(missing_PHONE = ifelse(is.na(training$PHONE), 1, 0)) %>%
  mutate(missing_LORES = ifelse(is.na(training$LORES), 1, 0)) %>%
  mutate(missing_HMVAL = ifelse(is.na(training$HMVAL), 1, 0)) %>%
  mutate(missing_AGE = ifelse(is.na(training$AGE), 1, 0)) %>%
  mutate(missing_CRSCORE = ifelse(is.na(training$CRSCORE), 1, 0)) 

#missing values for inv and ccpurc

#funs is a deprecated function. use list() with ~ for initial function call instead
training2 <- training %>% 
  mutate_if(is.numeric, funs(replace(., is.na(.), median(., na.rm = TRUE))))

#creating the GAM with splines. splines on continious variables.
sel.gam <- mgcv::gam(INS ~ s(ACCTAGE) +
                       factor(DDA) + 
                       s(DDABAL) +
                       factor(DEP) +
                       s(DEPAMT) +
                       s(CHECKS) + 
                       factor(DIRDEP) +
                       factor(NSF) +
                       s(NSFAMT) + 
                       s(PHONE) + #
                       s(TELLER) +
                       factor(SAV) + 
                       s(SAVBAL) + 
                       factor(ATM) +
                       s(ATMAMT) + 
                       factor(POS) + #
                       s(POSAMT) +
                       factor(CD) + s(CDBAL) +
                       factor(IRA) + 
                       s(IRABAL) + 
                       factor(INV) +
                       s(INVBAL) +
                       factor(MM) +
                       s(MMBAL) +
                       factor(MMCRED)+ #
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
                       family = "binomial", method = "REML", select = TRUE, data = training2)


summary(sel.gam)

# Predictions
pred = predict(sel.gam, type="response")

# ROC curve
library(InformationValue)
plotROC(training2$INS, pred)
