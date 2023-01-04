############################################################################################
# R script belonging to the tutorial 'Machine Learning for DSM-Part 2'
# 03 03 2021
# Author: Dr. R. Taghizadeh, Tuebingen University
############################################################################################
# empty memory and workspace
gc(); rm(list=ls())

# check directory
getwd()
dir()

#To install the required R packages
ls <- c("sp", "raster", "rgdal", "caret", "randomForest", "rpart", "rpart.plot", 
        "ggplot2", "corrplot", "plotKML", 
        "colorspace", "Cubist", "viridis","leaflet","mapview","corrplot")
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load library YOU MUST INSTALL
library(sp);library(raster);library(rgdal);library(caret);library(randomForest)
library(rpart);library(rpart.plot);library(ggplot2);library(corrplot);library(mapview)


################################################################Preparing geodatabase
################################################################Preparing geodatabase
# import raster layers of covarites (RS data)
covariates_RS <- stack(list.files("./cov/", pattern="\\.tif$", full.names = TRUE))
names(covariates_RS) 

# import raster layers of covarites (DEM)
covariates_DEM <- stack(list.files("./SAGA/", pattern="\\.sdat$", full.names = TRUE))
names(covariates_DEM)

# make a stack of covarites (RS data + DEM)
covariates <- stack(covariates_RS,resample(covariates_DEM,covariates_RS))
names(covariates)

# import the point soil data from desktop
soil <- read.csv ("soil.csv", header = TRUE)

# convert soil point data to spatial point data  
coordinates(soil) <- ~ x + y

# set projection
proj4string(soil) <- CRS("+init=epsg:32639")

# plot the point on the raster
plot(covariates$Nir, main = "Landsat Image + Sample Points");plot(soil, add =T,pch = 19)

# extract values
cov = raster::extract(covariates,soil,method='simple')

# combing the covariates + soil data
cov_soil = cbind(data.frame(cov),EC= soil$EC)

# inspect the covariates + soil data 
str(cov_soil)

################################################################Mahchine learning
################################################################Mahchine learning
# remove na values
cov_soil <- cov_soil[complete.cases(cov_soil),]
nrow(cov_soil)

# check the column names
names(cov_soil)

# check the corrolation between EC and ovariates
corrplot.mixed(cor(cov_soil), lower.col = "black", number.cex = .7)

# split the data to training (70%) and testing (30%) sets
trainIndex <- createDataPartition(cov_soil$EC,p = 0.7, list = FALSE, times = 1)

# subset the dataset
cov_soil_Train <- cov_soil[ trainIndex,]
cov_soil_Test  <- cov_soil[-trainIndex,]

# inspect the two datasets
str(cov_soil_Train)
str(cov_soil_Test)

# fit models on training set: random forest model
rf_fit <- randomForest(EC ~ ., data=cov_soil_Train, ntree=1000)

# covariate importance
varImpPlot(rf_fit) 

# apply the random forest model on testing data
EC_rf_Pred <- predict(rf_fit, cov_soil_Test)  # predict EC

# check the plot actual and predicted EC values
plot(cov_soil_Test$EC, EC_rf_Pred, main="Random Forest model", 
     col="blue",xlab="Actual EC", ylab="Predicted EC", xlim=c(0,250),ylim=c(0,250))
abline(coef = c(0,1),  col="red" )

# calculate corrolation
cor_rf <- cor(cov_soil_Test$EC, EC_rf_Pred);cor_rf

# calculate RMSE
RMSE_rf <- sqrt(mean((cov_soil_Test$EC - EC_rf_Pred)^2));RMSE_rf

################################################################Preparing soil maps
################################################################Preparing soil maps
map_rf <- raster::predict( covariates,rf_fit)

# plot the final maps
dev.off()
plot(map_rf, main="EC map based on RF model")

# intractive maps
mapview(map_rf, layer.name ="EC")

################################################################Mahchine learning_caret
################################################################Mahchine learning_caret
# import data
ec_class <- read.csv("EC_Class_Cov.csv")

#inspect data
str(ec_class)

# set as a factor
ec_class$EC.class <- as.factor(ec_class$EC.class)

#inspect data
str(ec_class)
head(ec_class, n=10)

# summarize the dataset
summary(ec_class)

# split the data to training (70%) and testing (30%) sets
trainIndex2 <- createDataPartition(ec_class$EC.class,p = 0.7, list = FALSE, times = 1)

# subset the dataset
cov_soil_Train2 <- ec_class[ trainIndex2,]
cov_soil_Test2  <- ec_class[-trainIndex2,]

# inspect the two datasets
str(cov_soil_Train2)
str(cov_soil_Test2)

# fit models on training set: random forest model
rf_fit2 <- randomForest(EC.class ~ ., data=cov_soil_Train2, ntree=1000)

# covariate importance
varImpPlot(rf_fit2) 

# apply the random forest model on testing data
EC_rf_Pred2 <- predict(rf_fit2, cov_soil_Test2)  # predict EC

# check the actual and predicted EC classes
confusionMatrix(EC_rf_Pred2, cov_soil_Test2$EC.class,
                dnn = c("Prediction", "Reference"))

#Preparing soil EC class map
map_rf2 <- raster::predict(covariates,rf_fit2)
# plot the final maps
dev.off()
my_col = c('beige','red','green','blue')
plot(map_rf2, legend = FALSE, col = my_col, main="EC Class Map")
legend(x='bottomleft', legend = c("S1","S2", "S3","S4"), fill = my_col)

# plot two soil salinity maps
dev.off()
par(mfrow=c(1,2))
plot(map_rf2, legend = FALSE, col = my_col, main="EC Class Map")
plot(map_rf, legend = FALSE, main="EC Value Map")

################################################################Mahchine learning_caret
################################################################Mahchine learning_caret
# Step by Step
############## 1) understand the data
# import data
ec_class <- read.csv("EC_Class_Cov.csv")

#inspect data
str(ec_class)

# set as a factor
ec_class$EC.class <- as.factor(ec_class$EC.class)

#inspect data
str(ec_class)
head(ec_class, n=10)

# summarize the dataset
summary(ec_class)


# create histograms for each covariate
dev.off()
par(mfrow=c(2,3))
for(i in 1:6) {hist(ec_class[,i], main=names(ec_class)[i])}

# density and box plots for each covarite by class value
x <- ec_class[,1:6]
y <- ec_class[,7]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="box", scales=scales)

############## 2) preprocessing the data
# scale the covariates into the range of [0, 1]!
preprocessParams <- preProcess(ec_class[,1:6], method=c("range"))
scaled_covariates <- predict(preprocessParams, ec_class[,1:6])
scaled_covariates$EC.class <- ec_class$EC.class
str(scaled_covariates)

############## 3) preprocessing the data _ k-fold Cross-Validation
# define training control
trainControl <- trainControl(method="cv", number=10)

# RF model
fit.rf <- train(EC.class~., data=scaled_covariates, trControl=trainControl, method="rf", metric="Accuracy")
# display the RF results
print(fit.rf)

# SVM model
fit.svm <- train(EC.class~., data=scaled_covariates, trControl=trainControl, method="svmRadial", metric="Accuracy")
# display the SVM results
print(fit.svm)

# KNN model
fit.knn <- train(EC.class~., data=scaled_covariates, trControl=trainControl, method="knn", metric="Accuracy")
# display the SVM results
print(fit.knn)

# collect resamples
results <- resamples(list(RF=fit.rf, SVM=fit.svm, KNN=fit.knn))

# box and whisker plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)


################################################################Tune Mahchine learning using Caret package
################################################################Tune Mahchine learning using Caret package
# Grid Search
trainControl2 <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(2:6))
rfRandom <- train(EC.class~., data=scaled_covariates, method="rf", 
                  metric="Accuracy", tuneGrid=tunegrid,trControl=trainControl2)
print(rfRandom)
plot(rfRandom)


################################################################Neural Networks using Caret package
################################################################Neural Networks using Caret package
library(neuralnet)
library(NeuralNetTools)

# inspect data
str(scaled_covariates)
head(scaled_covariates)

# simple ANN with only a single hidden neuron
ANN_model = neuralnet(EC.class~., data = scaled_covariates, hidden=4)

# plot
dev.off()
plot(ANN_model)
plotnet(ANN_model)

 



