# Libraries
library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)
library(Ckmeans.1d.dp)
library(pdp)
library(InformationValue)

set.seed(12345)

# Read in the data
ins.t = read.csv("https://raw.githubusercontent.com/josh-hull/Fall3HWTeamBlue11/main/Machine%20Learning/insurance_t.csv")
ins.v = read.csv("https://raw.githubusercontent.com/josh-hull/Fall3HWTeamBlue11/main/Machine%20Learning/insurance_v.csv")

# Look for variables with missing values
summary(ins.t)
sapply(ins.t, function(x) sum(is.na(x)))


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


# Prepare data for XGBoost function - similar to what we did for glmnet
train_x <- model.matrix(INS ~ ., data = ins.t.imputed)[, -1]
train_y <- (ins.t.imputed$INS)


# look <- model.matrix(INS ~ ., data = ins.t.imputed)


########## XGBoost #############

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

set.seed(12345)

# Tuning through caret
tune_grid <- expand.grid(
  nrounds = 18,
  eta = c(.18, 0.20),
  max_depth = (3:7),
  gamma = c(0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = c(0.75, .85)
)

set.seed(12345)

xgb.ins.caret <- train(x = train_x, y = factor(train_y),
                       method = "xgbTree",
                       #eval_metric = "auc",
                       tuneGrid = tune_grid,
                       trControl = trainControl(method = 'cv', # Using 10-fold cross-validation
                                                number = 10))

plot(xgb.ins.caret)
xgb.ins.caret$bestTune


set.seed(12345)

# Variable importance
xgb.ins <- xgboost(data = train_x, label = train_y, subsample = .85, nrounds = 24, eta = 0.2, max_depth = 6)

# Include a random variable to determine variable selection
ins.t.random <- ins.t.imputed
ins.t.random$random <- rnorm(8495)

train_x <- model.matrix(INS ~ ., data = ins.t.random)[, -1]
train_y <- ins.t.random$INS

set.seed(12345)
xgb.ins1 <- xgboost(data = train_x, label = train_y, subsample = .75, nrounds = 24, eta = 0.18, max_depth = 6, objective = "binary:logistic")

xgb.importance(feature_names = colnames(train_x), model = xgb.ins1)

xgb.ggplot.importance(xgb.importance(feature_names = colnames(train_x), model = xgb.ins1))


set.seed(12345)

# Interpret some of the variables using partial dependence plots
xgb.ins2 <- xgboost(data = train_x, label = train_y, subsample = .85, nrounds = 24, eta = 0.18, max_depth = 6, objective = "binary:logistic")

partial(xgb.ins1, pred.var = "SAVBAL", 
        plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "lattice", train = train_x, pdp.color = "red")

partial(xgb.ins1, pred.var = "DDABAL", plot = TRUE, rug = TRUE, alpha = 0.1, plot.engine = "lattice", train = train_x)

train_x <- model.matrix(INS ~ ., data = ins.t.imputed)[, -1]
train_y <- (ins.t.imputed$INS)

xgb.ins.final <- xgboost(data = train_x, label = train_y, subsample = .85, nrounds = 24, eta = 0.2, max_depth = 6)

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

