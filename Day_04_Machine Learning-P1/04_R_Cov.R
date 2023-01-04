############################################################################################
# R script belonging to the tutorial 'Machine Learning for DSM-Part 1'
# 21 02 2021
# Author: Dr. R. Taghizadeh, Tuebingen University
############################################################################################
# empty memory and workspace
gc()
rm(list=ls())

#set the directory
setwd("...../Day_04_Machine Learning")#YOU MUST CHANGE THE ADDRESS

# check directory
getwd()
dir()

#To install the required R packages
ls <- c("sp", "raster", "rgdal", "caret", "randomForest", "rpart", "rpart.plot", 
        "ggplot2", "corrplot", "PerformanceAnalytics", "plotKML", 
        "colorspace", "Cubist", "viridis","leaflet","mapview","corrplot")
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load library YOU MUST INSTALL
#install.packages("name of packages")
library(sp)
library(raster)
library(rgdal)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(corrplot)
library(PerformanceAnalytics)
library(mapview)
################################################################Preparing geodatabase
################################################################Preparing geodatabase
# import raster layers of covarites (RS data + DEM)
covariates <- stack(list.files("./cov/", pattern="\\.tif$", full.names = TRUE))
names(covariates) 

# viwe RGB
#viewRGB(covariates, r = 5,g = 3,b = 1)

# import the point soil data from desktop
soil <- read.csv ("soil.csv", header = TRUE)

# convert soil point data to spatial point data  
coordinates(soil) <- ~ x + y

# set projection
proj4string(soil) <- CRS("+init=epsg:32639")

# plot the point on the raster
plot(covariates$Nir, main = "Landsat Image + Sample Points")
plot(soil, add =T,pch = 19)

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
chart.Correlation(cov_soil, histogram=TRUE, pch=19)

# split the data to training (70%) and testing (30%) sets
trainIndex <- createDataPartition(cov_soil$EC, 
                                  p = 0.7, list = FALSE, times = 1)

# inspect
head(trainIndex)

# subset the dataset
cov_soil_Train <- cov_soil[ trainIndex,]
cov_soil_Test  <- cov_soil[-trainIndex,]

# inspect the two datasets
str(cov_soil_Train)
str(cov_soil_Test)

# fit models on training set: regression model
linear_fit <- lm(EC ~ Blue+DEM+Green+Nir+Red+SWIR1+SWIR2, 
                 data=cov_soil_Train)

# look at the summary of linear model
print(linear_fit)
summary(linear_fit)
#plot(linear_fit)

# apply the linear model on testing data
EC_linear_Pred <- predict(linear_fit, cov_soil_Test)  # predict EC


# check the plot actual and predicted EC values
plot(cov_soil_Test$EC, EC_linear_Pred, main="Linear model", 
     col="blue",xlab="Actual EC", ylab="Predicted EC", 
     xlim=c(0,250),ylim=c(0,250))

abline(coef = c(0,1),  col="red" )

# calculate corrolation
cor_linear <- cor(cov_soil_Test$EC, EC_linear_Pred)
cor_linear

# calculate RMSE
RMSE_linear <- sqrt(mean((cov_soil_Test$EC - EC_linear_Pred)^2))
RMSE_linear

# fit models on training set: decision tree model
tree_fit <- rpart(EC ~ Blue+DEM+Green+Nir+Red+SWIR1+SWIR2,
                  method="anova", data=cov_soil_Train,
                  control = rpart.control(minsplit = 20, cp = 0.0001))

# display the results
printcp(tree_fit) 

# visualize cross-validation results
plotcp(tree_fit) 

# detailed summary of splits
summary(tree_fit) 

# visualize the tree
rpart.plot(tree_fit) 

# apply the tree model on testing data
EC_tree_Pred <- predict(tree_fit, cov_soil_Test)  # predict EC

# check the plot actual and predicted EC values
plot(cov_soil_Test$EC, EC_tree_Pred, main="Tree model", 
     col="blue",xlab="Actual EC", ylab="Predicted EC", xlim=c(0,250),ylim=c(0,250))

abline(coef = c(0,1),  col="red" )

# calculate corrolation
cor_tree <- cor(cov_soil_Test$EC, EC_tree_Pred)
cor_tree

# calculate RMSE
RMSE_tree <- sqrt(mean((cov_soil_Test$EC - EC_tree_Pred)^2))
RMSE_tree

# fit models on training set: random forest model
rf_fit <- randomForest(EC ~ Blue+DEM+Green+Nir+Red+SWIR1+SWIR2, 
                       data=cov_soil_Train,ntree=1000, do.trace = 25)

# detailed summary of splits
summary(rf_fit) 

# visualize the tree
varImpPlot(rf_fit) 

# apply the random forest model on testing data
EC_rf_Pred <- predict(rf_fit, cov_soil_Test)  # predict EC

# check the plot actual and predicted EC values
plot(cov_soil_Test$EC, EC_rf_Pred, main="Tree model", 
     col="blue",xlab="Actual EC", ylab="Predicted EC", xlim=c(0,250),ylim=c(0,250))
abline(coef = c(0,1),  col="red" )

# calculate corrolation
cor_rf <- cor(cov_soil_Test$EC, EC_rf_Pred)
cor_rf

# calculate RMSE
RMSE_rf <- sqrt(mean((cov_soil_Test$EC - EC_rf_Pred)^2))
RMSE_rf

# check the accuary of three models
RMSE_models <- c(Linear=RMSE_linear,Tree=RMSE_tree,RF=RMSE_rf)
cor_models <- c(Linear=cor_linear,Tree=cor_tree,RF=cor_rf)

# plot the final results
dev.off()
par(mfrow=c(1,2))
barplot(RMSE_models, main="RMSE",col=c("red","blue","green"))
barplot(cor_models, main="Corrolation",col=c("red","blue","green"))


################################################################Preparing soil maps
################################################################Preparing soil maps
map_linear <- raster::predict( covariates,linear_fit)
map_tree <- raster::predict( covariates,tree_fit)
map_rf <- raster::predict( covariates,rf_fit)

# plot the final maps
dev.off()
spplot(map_linear, main="EC map based on Linear model")
spplot(map_tree, main="EC map based on Tree model")
spplot(map_rf, main="EC map based on RF model")

# intractive maps
#mapview(map_rf)
#mapshot(mapview(map_rf),url = paste0("./map.html"))
