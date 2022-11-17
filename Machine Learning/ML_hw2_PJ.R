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
library(imputeMissings)
# load data 
ins_t <- read.csv("D:/$$Course$$/NCSU/Fall/AA502/ML/Homework2_ML/insurance_t.csv")
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


## Modeling -- Random Forest 
# Random Forest model
set.seed(12345)
rf.ins <- randomForest(factor(INS) ~ ., data = ins_t, ntree = 500, importance = TRUE)

# Plot the change in error across different number of trees
plot(rf.ins, main = "Number of Trees Compared to MSE") 
#100-250

#Look at variable importance
varImpPlot(rf.ins,
           sort = TRUE,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf.ins)

# Tune an random forest mtry value
set.seed(12345)
i=which(colnames(ins_t)=="INS") # 
x = ins_t[,-37]
y = factor(ins_t[,37])
tuneRF(x = x, y = y, plot = TRUE, ntreeTry = 500, stepFactor = 0.5)
# mtry = 7 

# build model 
set.seed(12345)
rf.ins1 <- randomForest(factor(INS) ~ ., data = ins_t, ntree = 200, mtry = 7, importance = TRUE)
varImpPlot(rf.ins1,
           sort = TRUE,
           n.var = 14,
           main = "Order of Variables")
importance(rf.ins1)
p = predict(rf.ins1,type = "prob")

library(pROC)
r = multiclass.roc(ins_t$INS,p)
auc(r) #0.7895
rs <- r$rocs
plot.roc(rs$`0/1`[[1]],col="blue")
lines.roc(rs$`0/1`[[2]],col="red")

g0 <- ggroc(rs$`0/1`, legacy.axes = TRUE, colour ="#FF9999",linetype = 1, size = 1)
g0 + geom_line()+
  xlab("1-Specificity(FPR)") + ylab("Sensitivity(TPR)") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="grey", linetype="dashed")+
  ggtitle("ROC Curve for RandomForest-ALL Variables") + annotate("text", x=0.75, y=0.5, label="AUROC=0.7895") 

# Interpret some of the variables using partial dependence plots
partialPlot(rf.ins1, ins_t, SAVBAL)
partialPlot(rf.ins1, ins_t, DDABAL)

# Include a random variable to determine variable selection
ins_t$random <- rnorm(8495)

set.seed(12345)
rf.ins2 <- randomForest(factor(INS) ~ ., data = ins_t, ntree = 200, mtry = 7, importance = TRUE)
varImpPlot(rf.ins2,
           sort = TRUE,
           n.var = 20,
           main = "Look for Variables Below Random Variable")
importance(rf.ins2)

# final model 
set.seed(12345)
rf.ins3 <- randomForest(factor(INS) ~ SAVBAL+BRANCH+DDABAL, data = ins_t, ntree = 200, importance = TRUE)
pred = predict(rf.ins3,type = "prob")

library(pROC)
roc.mul = multiclass.roc(ins_t$INS,pred)
auc(roc.mul) # 0.7331
rs.f = roc.mul$rocs
plot.roc(rs.f$`0/1`[[1]],col="blue")
lines.roc(rs.f$`0/1`[[2]],col="red")

g.f <- ggroc(rs.f$`0/1`, legacy.axes = TRUE, colour ="#FF9999",linetype = 1, size = 1)
g.f + geom_line()+
  xlab("1-Specificity(FPR)") + ylab("Sensitivity(TPR)") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="grey", linetype="dashed")+
  ggtitle("ROC Curve for RandomForest-3variables") + annotate("text", x=0.75, y=0.5, label= "AUROC=0.7331") 
