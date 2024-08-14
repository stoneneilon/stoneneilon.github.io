#
#  regularization.r
#
library(caret)
library(MASS)
library(glmnet)
library(titanic)
library(vcd)
library(pROC)
#
# Titanic survivor data
#
dat <- titanic_train[,colnames(titanic_train) %in% c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]
dat$Survived <- factor(dat$Survived, labels=c("Died", "Survived"))
dat$Sex <- factor(dat$Sex, levels=c("male", "female"))
dat$Embarked <- factor(dat$Embarked, levels=c("S","C","Q"))
#
dat <- na.omit(dat)
#
# Mosaic plot
#
mosaic(~Sex + Pclass + Survived, shade=TRUE, data=dat)
#
# Dummy encoding of factor variables
#
createDummies <- dummyVars(~., dat[,-1], fullRank = TRUE)
new.predictors <- predict(createDummies, dat[,-1])
dat <- data.frame(Survived = dat$Survived, new.predictors)
#
# Adding some random (noise) variables
#
set.seed(1234)
nobs <- nrow(dat)
dat <- data.frame(dat, junk1=rnorm(nobs), junk2=rnorm(nobs), junk3=rnorm(nobs),
  junk4=rnorm(nobs), junk5=rnorm(nobs), junk6=rnorm(nobs),
  junk7=rnorm(nobs), junk8=rnorm(nobs), junk9=rnorm(nobs),
  junk10=rnorm(nobs), junk11=rnorm(nobs), junk12=rnorm(nobs),
  junk13=rnorm(nobs), junk14=rnorm(nobs), junk15=rnorm(nobs),
  junk16=rnorm(nobs), junk17=rnorm(nobs), junk18=rnorm(nobs),
  junk19=rnorm(nobs), junk20=rnorm(nobs), junk21=rnorm(nobs))
#
# Standardize predictors
#
preProcValues <- preProcess(dat, method=c("center","scale"))
dat <- predict(preProcValues, dat)
#
# Split data
#
set.seed(1234)
trainIndex <- createDataPartition(dat$Survived, p=0.7, list=FALSE)
#
train <- dat[trainIndex,]
test <- dat[-trainIndex,]
#
# Set control parameters for model training
#
fitCtrl <- trainControl(method = "repeatedcv",
                        number = 3,
                        repeats = 2,
                        ## This allows you to use the metric "ROC"
                        summaryFunction=twoClassSummary,
                        ## Estimate class probabilities
                        classProbs = TRUE,
                        ## Search "grid" or "random"
                        search = "random")
#
# Set testing grid
#
glmnetGrid <- expand.grid(alpha=seq(0,1,by=0.1), lambda=seq(0,0.2,by=0.01))
#
# Train model
#
set.seed(1234)
glmnet.res <- train(Survived ~ .,
    data=train,
    method="glmnet",
    trControl=fitCtrl,
    tuneGrid=glmnetGrid,
    #tuneLength=40,
    metric="ROC")
#
glmnet.res
plot(glmnet.res)
#
# Extract predictions and assess model performance
#
predclass.train <- predict(glmnet.res, train)
confusionMatrix(predclass.train, train$Survived[complete.cases(train)])
#
predclass.test <- predict(glmnet.res, test)
confusionMatrix(predclass.test, test$Survived[complete.cases(test)])
#
predprob.train <- predict(glmnet.res, train, type="prob")[,"Survived"]
predprob.test <- predict(glmnet.res, test, type="prob")[,"Survived"]
hist(predprob.test, col="skyblue", breaks=20)
#
roc(train$Survived[complete.cases(train)] ~ predprob.train)
roc(test$Survived[complete.cases(test)] ~ predprob.test)
#
#
#
#
# Regularization coefficients
#
fit.elasticnet <- glmnet(as.matrix(train[,-1]), as.numeric(train[,1]),
    family="binomial", alpha=0.9, lambda=0.03)
fit.elasticnet$beta
#
fit.elasticnet2 <- glmnet(as.matrix(train[,-1]), as.numeric(train[,1]),
                         family="binomial", alpha=0.9)
plot(fit.elasticnet2, xvar="lambda")
#
# Other regularization options
#
fit.lasso <- glmnet(train[,-1], train[,1], family="binomial", alpha=1)
fit.elnet <- glmnet(train[,-1], train[,1], family="binomial", alpha=.5)
fit.ridge <- glmnet(train[,-1], train[,1], family="binomial", alpha=0)
#
par(mfrow=c(1,3))
plot(fit.lasso, xvar="lambda")
plot(fit.elnet, xvar="lambda")
plot(fit.ridge, xvar="lambda")
#
