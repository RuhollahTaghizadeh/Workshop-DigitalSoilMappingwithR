############################################################################################
# R script belonging to the tutorial 'DSM From 0 to 100'
# 04 07 2021
# Author: Dr. R. Taghizadeh, Tuebingen University
############################################################################################
# load library
library(sp)
library(raster)
library(rgdal)
library(caret)
library(randomForest)
library(plotly)
library(mapview)
library(Metrics)
library(randomForestExplainer)

####################### 1) Import and Inspect Soil Data
# import the point data from soil folder
point <- read.csv ("./soil/point.csv", header = TRUE)

# inspect soil data
str(point)

# plot a histogram of EC
hist(point$EC, breaks=20, main="Histogram",col="blue",xlab="EC (dS/m)")

# plot a histogram of EC (interactive)
plotly::plot_ly(x = point$EC, type = "histogram",color=I("red"))

# summary statistics of EC
summary(point$EC)

# convert data.frame to spatial data
point_SP = point
coordinates(point_SP) <- ~ x + y
proj4string(point_SP) <- CRS("+init=epsg:32639")

# plot spatial data
spplot(point_SP)

# create interactive spatial plot
mapview::mapview(point_SP)

####################### 2) Prepare Covariates
# import raster layers: RS data from covariates_RS folder
blue = raster("./covariates_RS/Blue.tif")
green = raster("./covariates_RS/Green.tif")
red = raster("./covariates_RS/Red.tif")
nir = raster("./covariates_RS/Nir.tif")
swir1 = raster("./covariates_RS/swir1.tif")
swir2 = raster("./covariates_RS/swir2.tif")
ndvi = raster("./covariates_RS/ndvi.tif")

# make a stack file from RS rasters
covariate_RS = stack(blue,green,red,nir,swir1,swir2,ndvi)

# set the names to the rasters in the stack file
names(covariate_RS) <- c("blue","green","red","nir","swir1","swir2","ndvi")

# plot all covariate_RS
plot(covariate_RS)

# import raster layers: terrain attributes from covariates_DEM folder
Elev = raster("./covariates_DEM/Elevation.sdat")
CHNL = raster("./covariates_DEM/CHNL_BASE.sdat")
LSF = raster("./covariates_DEM/LSFACTOR.sdat")
MRV = raster("./covariates_DEM/MRVBF.sdat")
RSP = raster("./covariates_DEM/RSP.sdat")
TWI = raster("./covariates_DEM/TWI.sdat")
VAL = raster("./covariates_DEM/VALLEY_DEPTH.sdat")

# make a stack file from terrain attributes
covariate_DEM = stack(Elev,CHNL,LSF,MRV,RSP,TWI,VAL)

# set the names to the rasters in the stack file
names(covariate_DEM) <- c("Elev","CHNL","LSF","MRV","RSP","TWI","VAL")

# plot all covariate_DEM
plot(covariate_DEM)

# re-sampling covariate_RS to covariate_DEM 
RS_resampled = resample(covariate_RS,covariate_DEM, method="bilinear")

# make a final stack file from covariate_RS + covariate_DEM
covariate_stack = stack(covariate_DEM, RS_resampled)

# check the name of covariate_stack
names(covariate_stack)

# check the dim of covariate_stack
dim(covariate_stack)

# plot all covariate_stack
plot(covariate_stack)

####################### 3) Intersecting the Covariates with the Soil Data
# extract values
cov_extract = raster::extract(covariate_stack,point_SP,method='simple')

# convert to data frame
cov_extract = data.frame(cov_extract)

# inspect
str(cov_extract)

# combing the covariates + soil data
cov_soil = cbind(cov_extract,EC= point$EC)

# inspect
str(cov_soil)

# remove na values
cov_soil <- cov_soil[complete.cases(cov_soil),]

####################### 4) Split Data: Train and Test
# split the data to training (70%) and testing (30%) sets
trainIndex <- createDataPartition(cov_soil$EC,p = 0.7,list = FALSE)

# subset the data set
cov_soil_Train <- cov_soil[ trainIndex,]
cov_soil_Test  <- cov_soil[-trainIndex,]

# inspect the two data sets
str(cov_soil_Train)
str(cov_soil_Test)

# plot a histogram of EC for the two data sets
par(mfrow=c(1,2))
hist(cov_soil_Train$EC, breaks=20, main="Training Data",col="blue",xlab="EC (dS/m)")
hist(cov_soil_Test$EC, breaks=20, main="Testing Data",col="red",xlab="EC (dS/m)")

# summary statistics of EC for the two data sets
summary(cov_soil_Train$EC)
summary(cov_soil_Test$EC)

####################### 5) Train a Random Forest
# train the RF with hyper parameters
rf_tune = list()
for (i in 1:4) {
  NTREE <- c(100, 500, 1000, 2000)   
  rf_tune[[i]] <- tuneRF(cov_soil_Train[,-15], cov_soil_Train[,10], 
                        mtryStart = 2, ntreeTry=NTREE[i],
                        stepFactor=1.5, trace=F)}

# change the names
names(rf_tune) <- c("Tr_100", "Tr_500", "Tr_1000", "Tr_2000")

# look at the minimum OOE; choose mtry and ntree
min(rf_tune$Tr_100)
min(rf_tune$Tr_500)
min(rf_tune$Tr_1000)
min(rf_tune$Tr_2000)

# run RF using the best mtry and ntree
rf_fit <- randomForest(EC ~ ., data=cov_soil_Train, mtry = 10 , ntree=500)

# inspect the fitted random forest
rf_fit

####################### 6) Test a Random Forest
# apply the random forest model on testing data
EC_rf_Pred <- predict(rf_fit, cov_soil_Test)  # predict EC

# make a data.frame consist of actual EC and predicted EC
act_pred <- data.frame(measured = cov_soil_Test$EC, predicted = EC_rf_Pred)

# inspect data.frame act_pred
str(act_pred)

# plot actual and predicted EC values
plot(act_pred$measured, act_pred$predicted, main="RF model", 
     col="blue",xlab="Actual EC", ylab="Predicted EC", xlim=c(0,250),ylim=c(0,250))

# interactive plot actual and predicted EC values
plot_ly(data = act_pred, x = ~measured, y = ~predicted)

# calculate the error indices
rmse(act_pred$measured, act_pred$predicted)#RMSE
mae(act_pred$measured, act_pred$predicted)#mean absolute error
cor(act_pred$measured, act_pred$predicted) # correlation

####################### 7) Covariate Importance
# visualize the importance 1
varImpPlot(rf_fit) 

# visualize the importance 2
min_depth <- min_depth_distribution(rf_fit)
plot_min_depth_distribution(min_depth, mean_sample = "relevant_trees", k = 14)

# check the correlation covariates and EC
corrplot::corrplot.mixed(cor(cov_soil), lower.col = "black", number.cex = .7)

####################### 8) Predict Soil Map
# apply RF model on covariate 
map_rf <- raster::predict( covariate_stack,rf_fit)

# plot the final map
spplot(map_rf, main="EC map based on RF model")

# interactive map
mapview(map_rf)

####################### 9) Uncertainty Analysis
# Fit RF models for each bootstrap
boot = 5
fit_RF_boot = list()
for (i in 1:boot) { 
  index <- sample.int(nrow(cov_soil_Train), 1.0 * nrow(cov_soil_Train),replace = TRUE)
  fit_RF_boot[[i]] <- randomForest(EC~., data = cov_soil_Train[index,])}

# spatial predictions
r1 = list()
for (i in 1:boot) {r1[[i]] = predict(covariate_stack, fit_RF_boot[[i]])}

# Raster stack
r1 <- stack(r1)
spplot(r1)

# calculate statistical parameters
meanR <- calc(r1, fun = mean) # mean of the predicted maps
sdR <- calc(r1, fun = sd) # standard deviation of the predicted maps
sdER <- sdR/sqrt(boot) # standard error of the predicted maps

# upper and lower prediction limit
bootMap_upper <- meanR + sdER
bootMap_lower <- meanR - sdER

# prediction interval range
PI <- bootMap_upper - bootMap_lower

# plot
r3=stack(bootMap_lower,meanR,bootMap_upper, PI)
names(r3) <- c("Lower", "Mean", "Upper", "PI")
spplot(r3)
