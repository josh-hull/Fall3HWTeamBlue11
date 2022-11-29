########## ML - HW3 

# import library
library(dplyr)
library(tidyverse)
library(caret)
library(leaps)
library(glmnet)
library(ggplot2)
library(earth)
library(mgcv)
library(caret)
library(randomForest)
library(InformationValue)
library(imputeMissings)
library(nnet)
library(NeuralNetTools)
library(xgboost)
library(Ckmeans.1d.dp)
library(pdp)
# load data 
ins_t <- read.csv("D:/$$Course$$/NCSU/Fall/AA502/ML/Homework3_ML/insurance_t.csv")
ins_v <- read.csv("D:/$$Course$$/NCSU/Fall/AA502/ML/Homework3_ML/insurance_v.csv")
str(ins_t)

# Define variables 
binary <- c("DDA","DIRDEP","NSF", "SAV","ATM","CD","IRA","INV","MM","CC","SDB","INAREA")
ordinal <- c("MMCRED" ,"CCPURC")
normial <- c("BRANCH")
continuous <- c("ACCTAGE","DDABAL","DEP","DEPAMT", 
                "CHECKS","NSFAMT" , "PHONE","TELLER", 
                "SAVBAL","ATMAMT","POS","POSAMT", 
                "CDBAL","IRABAL","INVBAL","MMBAL",  
                "CCBAL","INCOME","LORES","HMVAL",  
                "AGE","CRSCORE")

ins_t[binary]<- lapply(ins_t[binary], as.factor)
ins_t[ordinal]<- lapply(ins_t[ordinal], as.factor)
ins_t[normial]<- lapply(ins_t[normial], as.factor)

# Number of missing value per variable #
sapply(ins_t, function(x) sum(is.na(x)))

# Impute Missing values 
ins_t <- impute(ins_t, method="median/mode", flag=TRUE)
sapply(ins_t, function(x) sum(is.na(x)))


##########################
###again for validation###
##########################
ins_v[binary]<- lapply(ins_v[binary], as.factor)
ins_v[ordinal]<- lapply(ins_v[ordinal], as.factor)
ins_v[normial]<- lapply(ins_v[normial], as.factor)

# Number of missing value per variable #
sapply(ins_v, function(x) sum(is.na(x)))

# Impute Missing values 
ins_v <- imputeMissings::impute(ins_v, method="median/mode", flag=TRUE)
sapply(ins_v, function(x) sum(is.na(x)))

##################### MARS/EARTH Algorithm #####################################
library(earth)

# for median and mode imputation
mars1 = earth(INS ~ ., data = ins_t, glm=list(family=binomial))
summary(mars1)

# Variable importance
evimp(mars1)

# Predictions
pred = predict(mars1, type = "response")

# ROC curve
library(InformationValue)
plotROC(ins_t$INS, pred)

############################## Random Forest ############################
# Random Forest on all variables
set.seed(12345)
rf.ins <- randomForest::randomForest(factor(INS)~., data=ins_t,ntree=500,importance=TRUE)

# Plot the change in error across different number of trees
plot(rf.ins, main = "Number of Trees Compared to MSE")
# ntree=300

#Look at variable importance
varImpPlot(rf.ins,
           sort = TRUE,
           n.var = 15,
           main = "Top 20 - Variable Importance")
importance(rf.ins)

# Tune random forest mtry value now with ntree = 150
set.seed(12345)
tuneRF(x = base::subset(ins_t,select=-c(INS)), y = factor(ins_t$INS), 
       plot = TRUE, ntreeTry = 300, stepFactor = 0.75)

# Re-run random forest now with ntree=150 and mtry=7/10
set.seed(12345)
rf.ins2 <- randomForest(factor(INS) ~ ., data = ins_t, 
                        ntree = 300, mtry = 10, importance = TRUE)

varImpPlot(rf.ins2,
           sort = TRUE,
           n.var = 15,
           main = "Order of Variables")
importance(rf.ins2)

# Plot ROC curve
plotROC(ins_t$INS, predict(rf.ins2,type="prob")[,2])

########################## XG Boost #####################################
# Prepare data for XGBoost function - similar to what we did for glmnet
train_x <- model.matrix(INS ~ ., data = ins_t)[, -1]
train_y <- ins_t$INS
# Build XGBoost model
set.seed(12345)
xgb.ins <- xgboost(data = train_x, label = train_y, subsample = 0.5, nrounds = 50,eval.metric="auc",objective="binary:logistic")

# Tuning an XGBoost nrounds parameter - 24 was lowest!
xgbcv.ins <- xgb.cv(data = train_x, label = train_y, subsample = 0.5, nrounds = 50, nfold = 10,eval.metric="auc",objective="binary:logistic")

# Tuning through caret
tune_grid <- expand.grid(
  nrounds = 8,
  eta = c(0.1, 0.15, 0.2, 0.25, 0.3),
  max_depth = c(1:10),
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = c(0.25, 0.5, 0.75, 1)
)

set.seed(12345)
xgb.ins.caret <- train(x = train_x, y = factor(train_y),
                       method = "xgbTree",
                       tuneGrid = tune_grid,
                       trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                number = 10))

plot(xgb.ins.caret)
# subsample=0.5, eta=0.3, max_depth=4

# Variable importance
xgb.ins <- xgboost(data = train_x, label = train_y, subsample = 0.75, nrounds = 8, eta = 0.25, max_depth = 5,eval.metric="auc",objective="binary:logistic")

xgb.importance(feature_names = colnames(train_x), model = xgb.ins)

xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb.ins))

# Plot ROC curve
plotROC(ins_t$INS, predict(xgb.ins,newdata = train_x))


############################## GAM #####################################

# Fitting with GAM's
sel.gam <- mgcv::gam(INS ~ s(ACCTAGE) +
                       s(DDABAL) +
                       s(DEP) +
                       s(DEPAMT) +
                       s(CHECKS) + 
                       s(NSFAMT) + 
                       s(PHONE) + 
                       s(TELLER) +
                       s(SAVBAL) + 
                       s(ATMAMT) + 
                       s(POS) + 
                       s(POSAMT) +
                       s(CDBAL) +
                       s(IRABAL) + 
                       s(INVBAL) +
                       s(MMBAL) +
                       s(CCBAL) +
                       s(INCOME)+ 
                       s(LORES)+ 
                       s(HMVAL) + 
                       s(AGE) + 
                       s(CRSCORE) + 
                       DDA+DIRDEP+NSF+SAV+ATM+CD+IRA+MM+SDB+INAREA+INV+CC+
                       MMCRED+CCPURC+BRANCH,
                     method = 'REML', data = ins_t,family = binomial(link = 'logit'),select=TRUE)
summary(sel.gam) 

# Remaining variables after selection
sel.gam2 <- mgcv::gam(INS ~ s(ACCTAGE) +
                        s(DDABAL)+
                        s(DEPAMT) +
                        s(CHECKS) +
                        s(TELLER) +
                        s(SAVBAL) +
                        s(ATMAMT) + 
                        s(CDBAL) +
                        s(INVBAL) +
                        s(MMBAL) +
                        s(CCBAL) +
                        DDA+NSF+ATM+CD+IRA+MM+INV+CC+BRANCH,
                      method = 'REML', data = ins_t,family = binomial(link = 'logit'),select=FALSE)
summary(sel.gam2)

plot(sel.gam2)
plotROC(ins_t$INS, sel.gam2$fitted.values)

#####################################################################
### creating a neural net on all variables ... w/ standardization ###
#####################################################################

# Standardizing Continuous Variables
ins_t_scaled <- ins_t %>%
  mutate(ACCTAGE = scale(ACCTAGE),
         DDABAL = scale(DDABAL),
         DEP = scale(DEP),
         DEPAMT = scale(DEPAMT),
         CHECKS = scale(CHECKS),
         NSFAMT = scale(NSFAMT),
         PHONE = scale(PHONE),
         TELLER = scale(TELLER),
         SAVBAL = scale(SAVBAL),
         ATMAMT = scale(ATMAMT),
         POS = scale(POS),
         POSAMT = scale(POSAMT),
         CDBAL = scale(CDBAL),
         IRABAL = scale(IRABAL),
         INVBAL = scale(INVBAL),
         MMBAL = scale(MMBAL),
         CCBAL = scale(CCBAL),
         INCOME = scale(INCOME),
         LORES = scale(LORES),
         HMVAL = scale(HMVAL),
         AGE = scale(AGE),
         CRSCORE = scale(CRSCORE)
  )

## Convert all flag variables into factor
which(colnames(ins_t)=='ACCTAGE_flag')
ins_t[,39:52] <- lapply(ins_t[,39:52] , factor)

set.seed(12345)
nn.ins <- nnet(INS ~. , data = ins_t_scaled, size = 5, linout = TRUE)

# Optimize Number of Hidden Nodes and Regularization (decay option)
tune_grid <- expand.grid(
  .size = c(3, 4, 5, 6, 7, 8, 9, 10),
  .decay = c(0, 0.5, 1)
)


set.seed(12345)
nn.ins.caret <- train(INS ~ .
                      , data = ins_t_scaled,
                      method = "nnet", # Neural network using the nnet package
                      tuneGrid = tune_grid,
                      trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                               number = 10),
                      trace = FALSE, linout = TRUE)

nn.ins.caret$bestTune

#run again but with best tune
set.seed(12345)
nn.ins <- nnet(INS ~. , data = ins_t, size = 4, decay = 1, linout = TRUE)


#prediction for training data
p1 <- predict(nn.ins, ins_t_scaled)

(tab1 <- table(p1, ins_t_scaled$INS))

#missclassification for training
(1 - sum(diag(tab1)) / sum(tab1))

#prediction for validation data
valid_pred <- predict (nn.ins, ins_v)

(valid_pred_tab <- table(valid_pred, ins_v$INS))

#missclassification rate for validation
(1 - sum(diag(valid_pred_tab)) / sum(valid_pred_tab))

#roc curve on training
train_roc = roc(ins_t_scaled$INS ~ p1, plot = TRUE, print.auc = TRUE)
as.numeric(train_roc$auc)

#####################################################################
###                       naive bayes                             ###
#####################################################################
library(e1071)

# Naive Bayes model
set.seed(12345)
nb.ins <- naiveBayes(as.factor(INS) ~ ., data = ins_t, laplace = 0, usekernel = TRUE)

summary(nb.ins)


#predictions
train_pred <- predict(nb.ins, ins_t)
(train_pred_tab <- table(train_pred, ins_t$INS))

(1 - sum(diag(train_pred_tab)) / sum(train_pred_tab))

#roc cruve on training
train_roc = roc(ins_t$INS ~ as.numeric(train_pred), plot = TRUE, print.auc = TRUE)
as.numeric(train_roc$auc)



