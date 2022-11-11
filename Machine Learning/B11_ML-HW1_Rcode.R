### Blue11 ML HW1 

# Read in the data
ins.t = read.csv("D:/$$Course$$/NCSU/Fall/AA502/ML/Homework1_ML/insurance_t.csv")
ins.v = read.csv("D:/$$Course$$/NCSU/Fall/AA502/ML/Homework1_ML/insurance_v.csv")

# Look for variables with missing values
summary(ins.t)
length(sapply(ins.t, function(x) sum(is.na(x)))==TRUE)


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


##################### MARS/EARTH Algorithm #####################################
library(earth)

# for median and mode imputation
mars = earth(INS ~ ., data = ins.t.imputed, glm=list(family=binomial))
summary(mars)

# Variable importance
evimp(mars)

# Predictions
pred = predict(mars, type = "response")

# ROC curve
library(InformationValue)
plotROC(ins.t.imputed$INS, pred)
AUROC(ins.t.imputed$INS, pred) #0.7994854
### use ggplot 
library(ROCR)
pred1 <- prediction(fitted(mars), factor(ins.t.imputed$INS))
perf <- performance(pred, measure = "tpr", 
                    x.measure = "fpr")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE, colorize.palette = rev(gray.colors(256)))
abline(a = 0, b = 1, lty = 3)

library(pROC)
rocobj <- roc(ins.t.imputed$INS, pred)
g <- ggroc(rocobj, alpha = 0.5, colour = "red", linetype = 1, size = 1)

gl <- ggroc(rocobj, legacy.axes = TRUE, colour ="red")
gl
gl + xlab("FPR") + ylab("TPR") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="grey", linetype="dashed")+
  ggtitle("ROC Curve for MARS Algorithm") + annotate("text", x=0.75, y=0.5, label= "AUROC=0.7995") 



# no select 
gam1 <- mgcv::gam(INS ~ s(ACCTAGE) +
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
                    factor(INAREA) + factor(BRANCH),
                  method = 'REML', data = ins.t.imputed,family = 'binomial')
summary(gam1) #R-sq.(adj) =  0.245
pred1 = predict(gam1, type="response")

# ROC curve
library(InformationValue)
plotROC(d$INS, pred1)#0.8029


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
                   factor(INAREA) + factor(BRANCH),
                 method = 'REML', select = TRUE, data = ins.t.imputed,family = 'binomial')
summary(gam) #R-sq.(adj) =  0.24


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
                  , method = 'REML', data = ins.t.imputed,family = 'binomial')
summary(gam2) #R-sq.(adj) =  0.244 


# Predictions
pred2 = predict(gam2, type="response")


# ROC curve
library(InformationValue)
plotROC(ins.t.imputed$INS, pred2)
AUROC(ins.t.imputed$INS, pred2)#0.8014101

### use ggplot 
library(pROC)
rocgam <- roc(ins.t.imputed$INS,ins.t.imputed$pred2,levels = c(0, 1), direction = "<")
ggroc(rocobj, alpha = 0.5, colour = "red", linetype = 1, size = 1)

g <- ggroc(rocgam, legacy.axes = TRUE, colour ="red")
g
g + xlab("FPR") + ylab("TPR") + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="grey", linetype="dashed")+
  ggtitle("ROC Curve for GAM Model") + annotate("text", x=0.75, y=0.5, label= "AUROC=0.8014") 

