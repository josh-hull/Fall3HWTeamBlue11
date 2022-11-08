# Read in the data
ins.t = read.csv("/Users/kelsypeil/Desktop/AA502/Machine Learning/Homework1_ML/insurance_t.csv")
ins.v = read.csv("/Users/kelsypeil/Desktop/AA502/Machine Learning/Homework1_ML/insurance_v.csv")

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


### Method 2: Try missing value category instead of mode imputation
ins.t.missing.cat = ins.t[missing_cat_vars]
ins.t.missing.cat[is.na(ins.t.missing.cat)] <- "M"

# Cbind all back together (imputed, flags, non-missing)
ins.t.imputed2 = cbind(ins.t.nomiss, ins.t.missing.cat)
ins.t.imputed2 = cbind(ins.t.imputed2, ins.t.missing.cont)


##################### MARS/EARTH Algorithm #####################################
library(earth)

# for median and mode imputation
mars1 = earth(INS ~ ., data = ins.t.imputed, glm=list(family=binomial))
summary(mars1)

# for median imputation and missing value category
mars2 = earth(INS ~., data = ins.t.imputed2, glm=list(family=binomial))
summary(mars2)

# Variable importance
evimp(mars1)
evimp(mars2)

# Predictions
pred = predict(mars1, type = "response")
pred2 = predict(mars2, type = "response")

# ROC curve
library(InformationValue)
plotROC(ins.t.imputed$INS, pred)
plotROC(ins.t.imputed2$INS, pred2)

