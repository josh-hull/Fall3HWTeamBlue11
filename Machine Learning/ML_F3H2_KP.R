# Libraries
library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)
library(Ckmeans.1d.dp)
library(pdp)
library(InformationValue)
library(pROC)

# Read in the data
ins.t = read.csv("/Users/kelsypeil/Desktop/AA502/Machine Learning/Homework2_ML/insurance_t.csv")
ins.v = read.csv("/Users/kelsypeil/Desktop/AA502/Machine Learning/Homework2_ML/insurance_v.csv")

# Look for variables with missing values
summary(ins.t)
sapply(ins.t, function(x) sum(is.na(x)))


####################### Continuous Variables ###################################
# All continuous variables
cont = c("ACCTAGE", "DDABAL", "DEPAMT", "CHECKS", "NSFAMT", "PHONE", "TELLER", 
         "SAVBAL", "ATMAMT", "POS", "POSAMT", "CDBAL", "IRABAL", "INVBAL", "MMBAL", 
         "MMCRED", "CCBAL", "INCOME", "LORES", "HMVAL", "AGE", "CRSCORE")

# Continuous variables with missing values
missing_cont_vars = c("ACCTAGE", "POSAMT", "INVBAL", "CCBAL", "PHONE", "POS",
                      "INCOME", "LORES", "HMVAL", "AGE", "CRSCORE")
ins.t.missing.cont = ins.t[missing_cont_vars]

# Create flags for continuous variables
library(misty)
flag.cont = na.indicator(ins.t.missing.cont, as.na = NULL, check = TRUE)
#Returns a matrix or data frame with r=1r=1 if a value is observed, and r=0r=0 if a value is missing.
colnames(flag.cont) <- c("ACCTAGE.f","POSAMT.f","INVBAL.f","CCBAL.f","PHONE.f","POS.f",
                         "INCOME.f","LORES.f","HMVAL.f","AGE.f","CRSCORE.f")

# Impute continuous variables with the median
med = NULL
for (i in 1:length(missing_cont_vars)){
  med[i] = median(ins.t.missing.cont[[i]], na.rm=TRUE)
  ins.t.missing.cont[[i]][is.na(ins.t.missing.cont[[i]])] = med[i]
}

# Cbind flag variables with imputed continuous variables 
ins.t.missing.cont = cbind(ins.t.missing.cont, flag.cont)

####################### Categorical Variables ##################################
# Categorical variables with missing values
missing_cat_vars = c("INV", "CC", "CCPURC")
ins.t.missing.cat = ins.t[missing_cat_vars]

# Create flags for continuous variables
flag.cat = na.indicator(ins.t.missing.cat, as.na = NULL, check = TRUE)
#Returns a matrix or data frame with r=1r=1 if a value is observed, and r=0r=0 if a value is missing.
colnames(flag.cat) <- c("INV.f","CC.f","CCPURC.f")

### Method 1: Impute missing values of categorical variables with mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mod = NULL
for (i in 1:length(missing_cat_vars)){
  mod[i] = getmode(ins.t.missing.cat[[i]])
  ins.t.missing.cat[[i]][is.na(ins.t.missing.cat[[i]])] = mod[i]
}

# Cbind flag variables with imputed categorical variables 
ins.t.missing.cat = cbind(ins.t.missing.cat, flag.cat)

# Isolate other variables with no missing values
all_vars = as.character(colnames(ins.t))
nomiss_cont_vars = setdiff(all_vars, missing_cont_vars)
nomiss_vars = setdiff(nomiss_cont_vars, missing_cat_vars)
ins.t.nomiss = ins.t[nomiss_vars]

# Cbind all back together (imputed, flags, non-missing)
ins.t.imputed = cbind(ins.t.nomiss, ins.t.missing.cat)
ins.t.imputed = cbind(ins.t.imputed, ins.t.missing.cont)


#################### Change structure of variables #############################

vars = as.character(colnames(ins.t.imputed))
cat_vars = setdiff(vars, cont)

#make categorical variables factor
ins.t.imputed[cat_vars] <- lapply(ins.t.imputed[cat_vars], factor)

# check
str(ins.t.imputed)


##################### Random Forest ############################################

# Random forest 1
set.seed(12345)
rf.ins = randomForest(factor(INS) ~ ., data = ins.t.imputed, ntree = 500, 
                      importance = TRUE)

# Plot the change in error across different number of trees
plot(rf.ins, main = "Number of Trees Compared to MSE")

mse = as.data.frame(rf.ins$err.rate)
write.csv(mse, "/Users/kelsypeil/Desktop/AA502/Machine Learning/Homework2_ML/rfmse.csv")

#Look at variable importance
varImpPlot(rf.ins,
           sort = TRUE,
           n.var = 15,
           main = "Top 20 - Variable Importance")
importance(rf.ins)

# Tune random forest mtry value now with ntree = 150
set.seed(12345)
tuneRF(x = ins.t.imputed[,-23], y = factor(ins.t.imputed[,23]), 
       plot = TRUE, ntreeTry = 150, stepFactor = 0.5)

# Re-run random forest now with ntree=150 and mtry=7
set.seed(12345)
rf.ins2 <- randomForest(factor(INS) ~ ., data = ins.t.imputed, 
                        ntree = 150, mtry = 7, importance = TRUE)

varImpPlot(rf.ins2,
           sort = TRUE,
           n.var = 15,
           main = "Order of Variables")
importance(rf.ins2)


# Include a random variable to determine variable selection
ins.t.imputed$random <- rnorm(8495)

set.seed(12345)
rf.ins3 <- randomForest(factor(INS) ~ ., data = ins.t.imputed, 
                        ntree = 150, mtry = 7, importance = TRUE)

varImpPlot(rf.ins3,
           sort = TRUE,
           n.var = 15,
           main = "Look for Variables Below Random Variable")
sort(round(importance(rf.ins3, type=2), 2))
var.imp = as.data.frame(importance(rf.ins3, type=2))
write.csv(var.imp, "/Users/kelsypeil/Desktop/AA502/Machine Learning/Homework2_ML/varimp.csv")

# final model
rf.ins4 = randomForest(factor(INS) ~ SAVBAL + BRANCH + DDABAL, 
                       data = ins.t.imputed, ntree = 150, importance = TRUE)

# Interpret some of the variables using partial dependence plots
partialPlot(rf.ins4, ins.t.imputed, SAVBAL)
partialPlot(rf.ins4, ins.t.imputed, DDABAL)

# ROC curve
ins.t.imputed$p_hat = predict(rf.ins4, type = "response")
plotROC(ins.t.imputed$INS, ins.t.imputed$p_hat)

require(pROC)
rf.roc<-roc(factor(ins.t.imputed$INS),rf.ins4$votes[,2])
plot(rf.roc)
auc(rf.roc)

rocplot = as.data.frame(cbind(rf.roc$sensitivities, rf.roc$specificities))
colnames(rocplot) = c("Sensitivity", "1-Specificity")
write.csv(rocplot, "/Users/kelsypeil/Desktop/AA502/Machine Learning/Homework2_ML/rocplot.csv")



