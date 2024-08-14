#
# mars.r
#
# Latest version of caret:
# devtools::install_github('topepo/caret/pkg/caret')
#
library(mlbench)
library(recipes)
library(dplyr)
library(ggplot2)
library(caret)
library(earth)
library(pdp)
library(plotly)
library(viridis)
#
set.seed(2024)
#
n <- 500
U <- mlbench.friedman1(n, sd=1)
dat <- data.frame(y=U$y, U$x)
#
# Standardize variables
#
preProcValues <- preProcess(dat, method=c("center","scale"))
dat <- predict(preProcValues, dat)
#
# Split data
#
trainIndex <- createDataPartition(dat$y, p=.8, list=FALSE)
#
train <- dat[trainIndex,]
test <- dat[-trainIndex,]
#
# Set control parameters for model training
#
fitCtrl <- trainControl(method = "repeatedcv",
                        number = 3,
                        repeats = 2,
                        ## Search "grid" or "random"
                        search = "grid")
#
# Set testing grid
#
marsGrid <- expand.grid(degree=1:3,
                       nprune=seq(2,20,by=2))
#
# Train model
#
mars.res <- train(y ~ .,
    data=train,
    method="lm",
    trControl=fitCtrl,
    tuneGrid=marsGrid,
    #tuneLength=20,
    metric="RMSE")
#
mars.res
plot(mars.res)
#
# Examine results
# (note slopes for hinge functions attached to x4 and x5)
#
summary(mars.res)
#
# Extract predictions
#
yhat.train <- predict(mars.res, train, type="raw")
yhat.test <- predict(mars.res, test, type="raw")
#
cor(yhat.train, train$y)^2
cor(yhat.test, test$y)^2
#
# Plots
#
base4 <- pdp::partial(mars.res, pred.var = "X4", grid.resolution = 20) %>% autoplot()
base5 <- pdp::partial(mars.res, pred.var = "X5", grid.resolution = 20) %>% autoplot()
gridExtra::grid.arrange(base4, base5, ncol = 2)
#
base1 <- pdp::partial(mars.res, pred.var = "X1", grid.resolution = 20) %>% autoplot()
base2 <- pdp::partial(mars.res, pred.var = "X2", grid.resolution = 20) %>% autoplot()
base3 <- pdp::partial(mars.res, pred.var = "X3", grid.resolution = 20) %>% autoplot()
gridExtra::grid.arrange(base1, base2, base3, ncol = 3)
#
# 3D plot
#
pdp::partial(mars.res, pred.var = c("X1", "X2"), grid.resolution = 10) %>%
  plotPartial(levelplot = FALSE, zlab = "Prediction", drape = TRUE, colorkey = TRUE, screen = list(z = -20, x = -60))
#
# Interactive 3D plot: thanks to Sam Fuller for this code.
#
interaction.preds <- pdp::partial(mars.res, pred.var = c("X1", "X2"), grid.resolution = 50)
#
values.x <- unique(interaction.preds$X1)
values.y <- unique(interaction.preds$X2)
values.z <- matrix(interaction.preds$yhat, nrow=50, byrow=TRUE)
#
mars.3d <- plot_ly(x=values.x, y=values.y, z=values.z, colors = viridis_pal(option = "plasma")(10)) %>%
  add_surface() %>%
  layout(
    title = "MARS model",
    scene = list(
      xaxis = list(title = "X1"),
      yaxis = list(title = "X2"),
      zaxis = list(title = "Prediction")
    ))
#
htmlwidgets::saveWidget(as_widget(mars.3d), "mars.html")
#
# Plot a sine wave (optional)
#plot(seq(-3,3,by=0.1), sin(seq(-3,3,by=0.1)))
#
#
# Note: if you want to estimate prediction uncertainty intervals, I've found it's easier
#  to go back and use the "earth" function directly, plugging in the optimal tuning parameters
#
earth.mod <- earth(y ~ ., data=train, nprune=16, degree=2, nfold=10, ncross=30, varmod.method="const")
#
summary(earth.mod)
#
yhat.train.uncertainty <- predict(earth.mod, train, interval="pint", level=.95)
yhat.test.uncertainty <- predict(earth.mod, test, interval="pint", level=.95)
#



h(1.29144-X4)                       -0.56850922
h(X4-1.29144)                        0.57098625


> x <- -2
> max((1.29 - x),0) * -0.57
[1] -1.8753
> max((x - 1.29),0) * 0.57
[1] 0
> 
  > x <- 0
> max((1.29 - x),0) * -0.57
[1] -0.7353
> max((x - 1.29),0) * 0.57
[1] 0
> 
  > x <- 2
> max((1.29 - x),0) * -0.57
[1] 0
> max((x - 1.29),0) * 0.57
[1] 0.4047

> -1.8753 - -0.7353
[1] -1.14
> -0.7353 - 0.4047
[1] -1.14
