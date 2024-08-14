#
# gbm.r
#
# Latest version of caret:
# devtools::install_github('topepo/caret/pkg/caret')
#
library(mlbench)
library(parallel)
library(doParallel)
library(foreach)
library(haven)
library(MASS)
library(ggplot2)
library(caret)
library(gbm)
library(pROC)
library(party)
library(dplyr)
library(ggraph)
library(igraph)
#
#
cl <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cl)
#
setwd("~/Stone_Website/notes/ICPSR_Machine_Learning/code")
load("ANES2016.rda") #bit complicated. Load is kinda like reading in the data? I think because it is a .Rda file. 
anes2016 <- ANES2016
#
# Decide which of these variables to include:
#
#[1197] "genderresent1"
#[1198] "genderresent2"               "genderresent3"               "genderresent4"
#[1201] "presvote"                    "female"                      "partyid"
#[1204] "libcon"                      "spendserv"                   "defensespend"
#[1207] "healthinsurance"             "guarjobs"                    "mexicowall"
#[1210] "aidblacks"                   "environmentjobs"             "affirmativeaction"
#[1213] "troopsisis"                  "syrianrefugees"              "climatechangeaction"
#[1216] "transgenderbathrooms"        "abortion"                    "presvote2012"
#[1219] "education"                   "college"                     "race"
#[1222] "white"                       "black"                       "hispanic"
#[1225] "thermometerclinton"          "thermometertrump"            "thermometerjohnson"
#[1228] "thermometerstein"            "thermometerkaine"            "thermometerpence"
#[1231] "thermometerroberts"          "thermometerpopefrancis"      "thermometerchristianfund"
#[1234] "thermometegbmeminists"        "thermometerliberals"         "thermometerunions"
#[1237] "thermometerpoor"             "thermometerbigbusiness"      "thermometerconservatives"
#[1240] "thermometersupremecourt"     "thermometergaylesbians"      "thermometercongress"
#[1243] "thermometerrich"             "thermometermuslims"          "thermometerchristians"
#[1246] "thermometerjews"             "thermometerteaparty"         "thermometerpolice"
#[1249] "thermometertransgender"      "thermometerscientists"       "thermometerblacklivesmatter"
#
dat <- anes2016[,colnames(anes2016) %in% c("presvote","genderresent1","thermometerpolice","thermometerbigbusiness",
  "mexicowall","climatechangeaction","college","white","spendserv","guarjobs","syrianrefugees","thermometerunions",
  "thermometerjohnson","thermometerstein","thermometerroberts")] # creating a subset of variables. 
# colnames(anes2016): Retrieves the column names of the dataframe anes2016.
# %in% c(...): Checks which of these column names are present in the specified vector.
# c("..."): Specifies a vector of column names to be included in the subset.
# In summary, the code extracts a subset of columns from anes2016 and assigns it to the dataframe dat, containing a specific set of political and social variables from the larger dataset.
#
dat$presvote <- factor(dat$presvote, labels=c("Clinton","Trump")) 
# pres vote is coded as 0 and 1. We are changing it to strings. 
#
# Pre-process data
#
set.seed(1985)
# This code confused me...I wasn't sure what it was doing. I used chat GPT to help. 
# this code uses the caret package. 
# impute is basically cleaning and replacing missing values. 
impute <- preProcess(dat[,which(!colnames(dat)=="presvote")], method=c("bagImpute")) # method is bagging (bootstrap aggregate)
predictors <- predict(impute, dat[,which(!colnames(dat)=="presvote")])
# predict(impute, dat[, which(!colnames(dat) == "presvote")]): Uses the predict function to impute missing values in the predictors (all columns except "presvote") using the information stored in impute. This step fills in any missing values in the predictors based on the bagged imputation model.
dat1 <- data.frame(presvote=dat$presvote, predictors)
# I created a new object. Compare the difference in the data between dat1 and dat. You will see what the output of all that code is, and subsequently the purpose of it. 
#
# Delete observations with missing values on y (presvote)
#
# now this code will overwrite dat1 and delete the NA values. The original code just stayed with the same 'dat' object. I created dat1 just to show the difference. Thus the subsequent code uses dat1 instead of dat. 
dat1 <- na.omit(dat1)
#
# Split data
#
set.seed(1985)
trainIndex <- createDataPartition(dat1$presvote, p=0.7, list=FALSE)
#
train <- dat1[trainIndex,]
test <- dat1[-trainIndex,]
#
# Set control parameters for model training
#
fitCtrl <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 2,
                        summaryFunction=twoClassSummary,
                        ## Estimate class probabilities
                        classProbs = TRUE,
                        ## Search "grid" or "random"
                        search = "random",
                        ## Down-sampling
                        sampling = "down",
                        ## Use cluster
                        allowParallel = TRUE)
#
# Set testing grid for gbm
#
gbmGrid <-  expand.grid(n.trees = c(1:20)*100,
                        interaction.depth=c(2:4),
                        shrinkage = c(0.01, 0.05),
                        n.minobsinnode = 5)
#
# 
gbm.res <- train(presvote ~ .,
    data=train,
    method="gbm",
    trControl=fitCtrl,
    tuneGrid=gbmGrid,
    #tuneLength=10,
    bag.fraction=0.5,
    metric="ROC",
    verbose=FALSE)
#
gbm.res
plot(gbm.res)
#
#
# Extract predictions
#
confusionMatrix(predict(gbm.res, train, type="raw"), train$presvote)
confusionMatrix(predict(gbm.res, test, type="raw"), test$presvote)
#
pred.train <- predict(gbm.res, train, type="prob")[,"Trump"]
roc(train$presvote ~ pred.train)
#
pred.test <- predict(gbm.res, test, type="prob")[,"Trump"]
roc(test$presvote ~ pred.test)
#
plot.roc(train$presvote, pred.train)
plot.roc(test$presvote, pred.test, add=TRUE, col="green")
#
# Variable importance
#
gbmImp <- varImp(gbm.res)
plot(gbmImp)
#
#
stopCluster(cl)
registerDoSEQ()
#
