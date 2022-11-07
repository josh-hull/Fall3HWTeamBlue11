# Read in the data
ins.t = read.csv("/Users/kelsypeil/Desktop/AA502/Machine Learning/Homework1_ML/insurance_t.csv")
ins.v = read.csv("/Users/kelsypeil/Desktop/AA502/Machine Learning/Homework1_ML/insurance_v.csv")

# All continuous variables
cont = c("ACCTAGE", "DDABAL", "DEPAMT", "CHECKS", "NSFAMT", "PHONE", "TELLER", 
         "SAVBAL", "ATMAMT", "POSAMT", "CDBAL", "IRABAL", "INVBAL", "MMBAL", "MMCRED", 
         "CCBAL", "CCPURC", "INCOME", "LORES", "HMVAL", "AGE", "CRSCORE")

# Look for variables with missing values
summary(ins.t)
sapply(ins.t, function(x) sum(is.na(x)))

# Impute missing values of continuous variables with median
missing_cont_vars = c("ACCTAGE", "POSAMT", "INVBAL", "CCBAL", "CCPURC", "PHONE",
                      "INCOME", "LORES", "HMVAL", "AGE", "CRSCORE")
ins.t.missing.cont = ins.t[missing_cont_vars]

med = NULL
for (i in 1:length(missing_cont_vars)){
  med[i] = median(ins.t.missing.cont[[i]], na.rm=TRUE)
  ins.t.missing.cont[[i]][is.na(ins.t.missing.cont[[i]])] = med[i]
}

# Create mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Impute missing values of categorical variables with mode
missing_cat_vars = c("POS", "INV", "CC")
ins.t.missing.cat = ins.t[missing_cat_vars]

mod = NULL
for (i in 1:length(missing_cat_vars)){
  mod[i] = getmode(ins.t.missing.cat[[i]])
  ins.t.missing.cat[[i]][is.na(ins.t.missing.cat[[i]])] = mod[i]
}

# Other variables (non-missing)
all_vars = as.character(colnames(ins.t))
nomiss_cont_vars = setdiff(all_vars, missing_cont_vars)
nomiss_vars = setdiff(nomiss_cont_vars, missing_cat_vars)
ins.t.nomiss = ins.t[nomiss_vars]

# cbind back together
ins.t.imputed = cbind(ins.t.nomiss, ins.t.missing.cat)
ins.t.imputed = cbind(ins.t.imputed, ins.t.missing.cont)


# MARS/EARTH algorithm
library(earth)
mars1 = earth(INS ~ ., data = ins.t.imputed, glm=list(family=binomial))
summary(mars1)

# Variable importance
evimp(mars1)

# Predictions
pred = predict(mars1, type="response")

# ROC curve
library(InformationValue)
plotROC(ins.t.imputed$INS, pred)







