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
ins.t.imputed1 <- ins.t.imputed
vars = as.character(colnames(ins.t.imputed1))
cat_vars = setdiff(vars, cont)

#make categorical variables factor
ins.t.imputed1[cat_vars] <- lapply(ins.t.imputed1[cat_vars], factor)

# check
str(ins.t.imputed1)


##################### Random Forest ############################################

# Random forest 1
set.seed(12345)
rf.ins = randomForest(factor(INS) ~ ., data = ins.t.imputed1, ntree = 500, 
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
tuneRF(x = ins.t.imputed1[,-23], y = factor(ins.t.imputed1[,23]), 
       plot = TRUE, ntreeTry = 150, stepFactor = 0.5)

# Re-run random forest now with ntree=150 and mtry=7
set.seed(12345)
rf.ins2 <- randomForest(factor(INS) ~ ., data = ins.t.imputed1, 
                        ntree = 150, mtry = 7, importance = TRUE)

varImpPlot(rf.ins2,
           sort = TRUE,
           n.var = 15,
           main = "Order of Variables")
importance(rf.ins2)


# Include a random variable to determine variable selection
ins.t.imputed$random <- rnorm(8495)

set.seed(12345)
rf.ins3 <- randomForest(factor(INS) ~ ., data = ins.t.imputed1, 
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
                       data = ins.t.imputed1, ntree = 150, importance = TRUE)

# Interpret some of the variables using partial dependence plots
partialPlot(rf.ins4, ins.t.imputed1, SAVBAL)
partialPlot(rf.ins4, ins.t.imputed1, DDABAL)


require(pROC)
rf.roc<-roc(factor(ins.t.imputed1$INS),rf.ins4$votes[,2])
plot(rf.roc)
auc(rf.roc)

rocplot = as.data.frame(cbind(rf.roc$sensitivities, rf.roc$specificities))
colnames(rocplot) = c("Sensitivity", "1-Specificity")
write.csv(rocplot, "/Users/kelsypeil/Desktop/AA502/Machine Learning/Homework2_ML/rocplot.csv")

################################ XGBoost ######################################
# Prepare data for XGBoost
train_x <- model.matrix(INS ~ ., data = ins.t.imputed)[, -1]
train_y <- (ins.t.imputed$INS)

# Build XGBoost model
set.seed(12345)
xgb.ins <- xgboost(data = train_x, label = train_y, subsample = 0.5, nrounds = 100, objective = "binary:logistic")

# xgb.phat1 <- predict(xgb.ins, newdata = train_x, type = 'prob')
# 
# pred <- prediction(list(xgb.phat1), ins.t.imputed$INS)
# perf <- performance(pred, measure = 'tpr', x.measure = 'fpr')
# plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE, color_palette = rev(gray.colors(256)))
# abline(a = 0, b = 1, lty = 3)
# AUROC(ins.t.imputed$INS, xgb.phat1)

# Tuning an XGBoost nrounds parameter - 24 was lowest!
xgbcv.ins <- xgb.cv(data = train_x, label = train_y, subsample = 0.5, nrounds = 100, nfold = 10, objective = "binary:logistic", metrics = "auc")

elog <- as.data.frame(xgbcv.ins $evaluation_log)
print(which.max(elog$test_auc_mean))

#pred <- predict(xgb.ins, train_x)
#prediction <- as.numeric(pred > 0.5)
#err <- mean(as.numeric(pred > 0.5) != ins.t.imputed$label)

# set.seed(12345)
# 
# Tuning through caret
# tune_grid <- expand.grid(
#   nrounds = 16,
#   eta = c(.15, 0.18, 0.20, 0.22, 0.25),
#   max_depth = (1:10),
#   gamma = c(0),
#   colsample_bytree = 1,
#   min_child_weight = 1,
#   subsample = c(0.5, 0.65, 0.75, .85, 0.95, 1)
# )
# 
# set.seed(12345)
# 
# xgb.ins.caret <- train(x = train_x, y = factor(train_y),
#                        method = "xgbTree",
#                        #eval_metric = "auc",
#                        tuneGrid = tune_grid,
#                        trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
#                                                 number = 10))
# 
# plot(xgb.ins.caret)
# xgb.ins.caret$bestTune
# 
#nrounds=16, eta=.2, max_depth=5, sub_sample=.75

# Tuning through caret
set.seed(12345)
tune_grid <- expand.grid(
  nrounds = 16,
  eta = 0.20,
  max_depth = 5,
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 0.75
)

set.seed(12345)

xgb.ins.caret <- train(x = train_x, y = factor(train_y),
                       method = "xgbTree",
                       #eval_metric = "auc",
                       tuneGrid = tune_grid,
                       trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                number = 10))


set.seed(12345)

# Variable importance
xgb.ins <- xgboost(data = train_x, label = train_y, subsample = .75, nrounds = 16, eta = 0.2, max_depth = 5)

# Include a random variable to determine variable selection
ins.t.random <- ins.t.imputed
ins.t.random$random <- rnorm(8495)

train_x <- model.matrix(INS ~ ., data = ins.t.random)[, -1]
train_y <- ins.t.random$INS

set.seed(12345)
xgb.ins1 <- xgboost(data = train_x, label = train_y, subsample = .75, nrounds = 16, eta = 0.2, max_depth = 5, objective = "binary:logistic")

xgb.importance(feature_names = colnames(train_x), model = xgb.ins1)

xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb.ins1))


set.seed(12345)

# Interpret some of the variables using partial dependence plots

partial(xgb.ins1, pred.var = "SAVBAL", 
        plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "lattice", train = train_x, pdp.color = "red")

partial(xgb.ins1, pred.var = "DDABAL", plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "lattice", train = train_x)

partial(xgb.ins1, pred.var = "CDBAL", plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "lattice", train = train_x)


# Creating final model
train_x <- model.matrix(INS ~ SAVBAL + DDABAL + CDBAL + DDA + MMBAL + ACCTAGE + MM + CHECKS + CCBAL + DEPAMT + ATMAMT, data = ins.t.imputed)[, -1]
train_y <- (ins.t.imputed$INS)

xgb.ins.final <- xgboost(data = train_x, label = train_y, subsample = .75, nrounds = 16, eta = 0.2, max_depth = 5)

set.seed(12345)
##P-values
phat_xgb_final <- predict(xgb.ins.final, newdata = train_x, type = 'prob')

##Final ROC Curve
#Final Model AUC: 
pred <- prediction(list(phat_xgb_final), ins.t.imputed$INS)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE, colorize.palette = rev(gray.colors(256)))
abline(a = 0, b = 1, lty = 3)
AUROC(ins.t.imputed$INS, phat_xgb_final)

require(pROC)
xgb.roc<-roc(factor(ins.t.imputed$INS),phat_xgb_final)
plot(xgb.roc)
auc(xgb.roc)

rocplot = as.data.frame(cbind(xgb.roc$sensitivities, xgb.roc$specificities))
colnames(rocplot) = c("Sensitivity", "1-Specificity")
write.csv(rocplot, "//Users/josiahhull/Desktop/502/Machine Learning/Homework2_ML/XGB_ROC.csv")