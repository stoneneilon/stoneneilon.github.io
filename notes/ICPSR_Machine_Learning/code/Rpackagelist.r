##################################
### R PACKAGE LIST AND INSTALL ###
##################################

# Thanks to Sam Fuller and Jack Rametta for improving this code!

install.packages(c("grf","bartMachine","ggdist","tidymodels","ggridges",
                    "GenAlgo","ClassDiscovery","rFSA","AmesHousing","mlbench","ISLR",
                    "leaps","sjPlot","MASS","ggplot2","caret","mandelbrot","recipes","sparsereg","corrplot",
                    "glmnet","klaR","titanic","vcd","pROC","poLCA","psych","smacof","iml","future","future.callr",
                    "MCMCpack","quadprog","earth","dplyr","nnet","devtools","reshape","ggthemes",
                    "lattice","rgenoud","plotly","ranger","gbm","haven","dplyr","ggraph","igraph","rpart",
                    "party","rpart.plot","doParallel","foreach","kernlab","recipes","RColorBrewer",
                    "pdp","NeuralNetTools","permimp","randomForest","minerva","funModeling","prodlim","fpc",
                    "vip","factoextra","fastshap","ggbeeswarm","data.table","ggfortify","cluster","flashlight",
                    "randomForestExplainer","xgboost","DiagrammeR","ggrepel","EIX","boot","tidyverse","SuperLearner",
                    "caretEnsemble","caTools"),
                 Ncpus =  parallel::detectCores()-1)
#
# A package to install quickly from github, bioconductor, or cran
if("pak" %in% installed.packages()[,1]){
  print("Pak already installed")
  } else {
  install.packages("pak")
}
#
# If you have issues with pak, you can also use remotes::install_github(. . .) OR
# devtools::install_github(. . .) for github packages. For Bioconductor, if pak fails
# you can use BiocManager::install(. . .)
#
pak::pak("partykit")
#
pak::pak("ModelOriented/EIX")
#
pak::pak("nredell/shapFlex")
#
# May need to install from CRAN
#install.packages("funModeling")
#pak::pak("pablo14/funModeling")
#
pak::pak("ryantibs/conformal/conformalInference")
#
pak::pak("lihualei71/cfcausal")
#
