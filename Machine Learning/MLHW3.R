#Mahad Munawar
#Due: 11/30/22
#ML HW 3
#Blue Team 11

#Contains code from HW 1 and 2 with validation AUCs along with HW 3 requirements


library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)
library(Ckmeans.1d.dp)
library(pdp)
library(InformationValue)
library(caret)
library(nnet)
library(NeuralNetTools)
library(pROC)

# Read in the data
ins_t = read.csv("/Users/mahadmunawar/Downloads/Homework2_ML/insurance_t.csv")
ins_v = read.csv("/Users/mahadmunawar/Downloads/Homework2_ML/insurance_v.csv")


#####################################################################
###           Dr. LaBarr'smethod of imputation                    ###
#####################################################################

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
ins_t <- imputeMissings::impute(ins_t, method="median/mode", flag=TRUE)
sapply(ins_t, function(x) sum(is.na(x)))
##########################
###again for validation###
##########################
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

# for median imputation and missing value category
#mars2 = earth(INS ~., data = ins.t.imputed2, glm=list(family=binomial))
#summary(mars2)

# Variable importance
evimp(mars1)


# Predictions
pred = predict(mars1, type = "response")
pred_val = predict(mars1, ins_v, type = "response")

# ROC curve
library(InformationValue)
plotROC(ins_t$INS, pred)
plotROC(ins_v$INS, pred_val)


############################## GAM #####################################

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
                       factor(INAREA) +
                       factor(BRANCH),
                     method = 'REML', select = TRUE, data = ins_t,family = 'binomial')
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
                        factor(CC)+ 
                        factor(BRANCH)
                      , method = 'REML', data = ins_t,family = 'binomial')
summary(sel.gam2)#R-sq.(adj) =   0.24

# Predictions
pred2 = predict(sel.gam2, type="response")
pred2_val = predict(sel.gam2,ins_v, type = "response")

# ROC curve
library(InformationValue)
plotROC(ins_t$INS, pred2)
plotROC(ins_v$INS, pred2_val)

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
ins_v_scaled <- ins_v %>% 
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
nn.ins <- nnet(INS ~. , data = ins_t_scaled, size = 4, decay = 1, linout = TRUE)


#prediction for training data
p1 <- predict(nn.ins, ins_t_scaled)

(tab1 <- table(p1, ins_t_scaled$INS))

#missclassification for training
(1 - sum(diag(tab1)) / sum(tab1))

#prediction for validation data
valid_pred <- predict (nn.ins, ins_v_scaled)

(valid_pred_tab <- table(valid_pred, ins_v$INS))

#missclassification rate for validation
(1 - sum(diag(valid_pred_tab)) / sum(valid_pred_tab))

#roc curve on training
train_roc = roc(ins_t_scaled$INS ~ p1, plot = TRUE, print.auc = TRUE)
as.numeric(train_roc$auc)

#roc curve on validation
val_roc = roc(ins_v_scaled$INS ~ valid_pred, plot = TRUE, print.auc = TRUE)
as.numeric(val_roc$auc)




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


#validation
valid_pred <- predict (nb.ins, ins_v)

(valid_pred_tab <- table(valid_pred, ins_v$INS))

#missclasification
(1 - sum(diag(valid_pred_tab)) / sum(valid_pred_tab))



#roc curve on validation
val_roc = roc(ins_v$INS ~ as.numeric(valid_pred), plot = TRUE, print.auc = TRUE)
as.numeric(val_roc$auc)


plotROC(ins_v$INS, as.numeric(valid_pred))

###########Peijia's code for Random Forest ##############

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
pred = predict(rf.ins3, type = "prob")

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

# final model - validation 
set.seed(12345)
rf.ins3 <- randomForest(factor(INS) ~ SAVBAL+BRANCH+DDABAL, data = ins_t, ntree = 200, importance = TRUE)
pred = predict(rf.ins3, newdata=ins_v, type = "prob")

library(pROC)
roc.mul = multiclass.roc(ins_v$INS,pred)
auc(roc.mul) # 0.7377
rs.f = roc.mul$rocs
plot.roc(rs.f$`0/1`[[1]],col="blue")
lines.roc(rs.f$`0/1`[[2]],col="red")

g.f <- ggroc(rs.f$`0/1`, legacy.axes = TRUE, colour ="#FF9999",linetype = 1, size = 1)
g.f + geom_line()+
  xlab("1-Specificity(FPR)") + ylab("Sensitivity(TPR)") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="grey", linetype="dashed")+
  ggtitle("ROC Curve for RandomForest-3variables") + annotate("text", x=0.75, y=0.5, label= "AUROC=0.7377") 



#apparently there is a glitch in predict for random forest. this is a trick to get around it. 
ins_v <- rbind(ins_t[1, ] , ins_v)
ins_v <- ins_t[-1,]

#predicting new values on validation 
set.seed(12345)
pred = predict(rf.ins3, newdata=ins_v, type = "prob")


library(pROC)
roc.mul = multiclass.roc(ins_v$INS,pred)
auc(roc.mul) # 0.885
rs.f = roc.mul$rocs
plot.roc(rs.f$`0/1`[[1]],col="blue")
lines.roc(rs.f$`0/1`[[2]],col="red")

g.f <- ggroc(rs.f$`0/1`, legacy.axes = TRUE, colour ="#FF9999",linetype = 1, size = 1)
g.f + geom_line()+
  xlab("1-Specificity(FPR)") + ylab("Sensitivity(TPR)") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="grey", linetype="dashed")+
  ggtitle("ROC Curve for RandomForest-3variables") + annotate("text", x=0.75, y=0.5, label= "AUROC=0.885") 

#using rf.ins1 since rf.ins3 doesn't have ACCTAGE
partialPlot(rf.ins1, ins_t, ACCTAGE)


