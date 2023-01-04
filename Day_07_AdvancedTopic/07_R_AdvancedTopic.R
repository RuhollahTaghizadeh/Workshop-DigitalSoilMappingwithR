############################################################################################
# R script belonging to the tutorial 'AdvancedTopic in DSM'
# 26 06 2021
# Author: Dr. R. Taghizadeh, Tuebingen University
############################################################################################
# empty  workspace
rm(list=ls())

# check directory
getwd()
dir()
############# ############# ############# ############# ############# ############# 
############# 3D Soil Mapping: soil depth function + Machine learning
############# ############# ############# ############# ############# #############
############# 1) Soil Depth Functions
# load library
# if you dont have a package: install.packages("NAME")
require(plotKML)
require(GSIF)
require(ithir)
require(aqp)


# Input data
ar_df<-read.csv("3D_SoilDepth.csv") # Data
str(ar_df)


# plot soil profile
plot_soilProfile(data = ar_df, vals = ar_df$EC, depths = ar_df[,2:3], label= "EC")


# Soil Depth Functions
# exponential
expo = lm (EC ~ log(Depth), data = ar_df)

# Equal-Smoothing Splines****
depths(ar_df) <- ID ~ Upper + Lower
plot(ar_df)

# spline model
ar_fit<-ea_spline(ar_df, var.name ="EC", d = t(c(0, 5, 15, 30, 60, 100, 200)), lam = 0.1, vlow = 0)
str(ar_fit)

# plot spline
plot_ea_spline(splineOuts = ar_fit, d = t(c(0, 5, 15, 30, 60,100, 200)), 
                 maxd = 200, type = 1, plot.which = 1,
                 label = "EC") 

plot(ar_fit$var.1cm)

############# 2) Machine learning
# load library
# if you dont have a package: install.packages("NAME")
require(caret)
require(randomForest)
require(raster)
require(sp)
require(rgdal)

# import raster layers of covarites (RS data)
covariates_RS <- stack(list.files("./cov/", pattern="\\.tif$", full.names = TRUE))
names(covariates_RS) 


# import raster layers of covarites (DEM)
covariates_DEM <- stack(list.files("./SAGA/", pattern="\\.sdat$", full.names = TRUE))
names(covariates_DEM)


# make a stack of covarites (RS data + DEM)
covariates <- stack(covariates_RS,resample(covariates_DEM,covariates_RS))
names(covariates)

# aggreagte rasters
covariates = aggregate(covariates, 9, fun=mean)


# import the point soil data from desktop
soil <- read.csv ("3D_ML.csv", header = TRUE)

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
cov_soil = cbind(data.frame(cov),soil[2:6])

# inspect the covariates + soil data 
cov_soil = cov_soil [,-c(14:16)]
str(cov_soil)

# remove na values
cov_soil <- cov_soil[complete.cases(cov_soil),]
nrow(cov_soil)


# Define the formula
Formula  = list()
for (i in 1:5) { 
  Formula [[i]] = as.formula (paste(names(cov_soil)[i+8]," ~ ", paste(names(cov_soil)[1:8],collapse="+")))}


# run random forest using caret package
# Define traincontrol
library(caret)
TrainControl <- trainControl(method="cv", 10, allowParallel = TRUE, savePredictions=TRUE)
# training random forest
Fit_RF  = list()
for (i in 1:5) { 
  Fit_RF [[i]] <- train(Formula[[i]], data=cov_soil, method="rf", metric="RMSE", trControl=TrainControl)} #RF


# Look at the primary results of RF
ModelList <- resamples(list(D01=Fit_RF[[1]], D02=Fit_RF[[2]],D03=Fit_RF[[3]], D04=Fit_RF[[4]], D05=Fit_RF[[5]]))
bwplot(ModelList, scales=list(x=list(relation="free"), y=list(relation="free")))
summary(ModelList)


# predict soil salinity map for five standard depths
EC_rf_Map = list()
for (i in 1:5) {
  EC_rf_Map[[i]] <- predict(covariates,Fit_RF[[i]])}  # predict EC 


# convert to stack
EC_rf_Map = stack(EC_rf_Map)
names(EC_rf_Map) = c("Depth01", "Depth02","Depth03","Depth04","Depth05")


# plot the predicted soil salinity maps for five standard depths
spplot(EC_rf_Map)


############# ############# ############# ############# ############# ############# 
############# Spatial sampling: cLHS - Conditioned Latin Hypercube Sampling (Dr. Dave White)
############# ############# ############# ############# ############# ############# 
# load library
# if you dont have a package: install.packages("NAME")
library(rgdal)
library(raster)
library(clhs)

# load raster data
cov <- stack(list.files("./cov/", pattern="\\.tif$", full.names = TRUE))
names(cov)
res(cov)


# aggreagate covariates with a lower resolution
cov_ag = aggregate(cov, 9, fun=mean)
names(cov_ag)
res(cov_ag)

# convert raster to SpatialPointsDataFrame
cov_ag_sp <- rasterToPoints(cov_ag, spatial=TRUE)
cov_ag_sp

# run cLHS
clhs_res <- clhs(cov_ag_sp, size = 100, progress = T, iter = 1000, simple = FALSE)

# plot clhs result
plot(clhs_res, mode=c("obj", "box"))


# the index of selected points
point_idx <- clhs_res$index_samples


# check visually
par(mar = c(1,1,1,1))
plot(cov[[3]], axes=FALSE)
points(cov_ag_sp[point_idx, ], bg = 'red', pch=21)


# save cLHS points to shp
writeOGR(cov_ag_sp[point_idx, ], dsn = './GIS', layer='clhs', driver = 'ESRI Shapefile')



############# ############# ############# ############# ############# ############# 
############# Uncertainty quantification
############# ############# ############# ############# ############# #############
# load library
# if you dont have a package: install.packages("NAME")
require(caret)
require(randomForest)
require(raster)
require(sp)
require(rgdal)

# make a geodatbase: covariates + soil data
covariates_RS <- stack(list.files("./cov/", pattern="\\.tif$", full.names = TRUE))
covariates_DEM <- stack(list.files("./SAGA/", pattern="\\.sdat$", full.names = TRUE))
covariates <- stack(covariates_RS,resample(covariates_DEM,covariates_RS))
covariates = aggregate(covariates, 9, fun=mean)
soil <- read.csv ("Uncertainty.csv", header = TRUE)
coordinates(soil) <- ~ x + y
proj4string(soil) <- CRS("+init=epsg:32639")
cov = raster::extract(covariates,soil,method='simple')
cov_soil = cbind(data.frame(cov),EC=soil$EC)
cov_soil <- cov_soil[complete.cases(cov_soil),]
names(cov_soil)


############# 1) k-fold cross validation (Prof. Budiman Minasny)
nval=nrow(cov_soil) 
kfold=10  
# first randomise the order of data
j <- sample.int(nval)
soilshuff <-cov_soil[j,] 
folds <- cut(seq(1,nval),breaks=kfold,labels=FALSE) 
pred=list() 

for (i in 1:kfold) {
  idx <- which(folds==i) 
  sfit <- randomForest(EC ~ ., data=cov_soil[-idx,])
  pred[[i]] <- predict (covariates,sfit)} 

# convert to stack
EC_rf_Map = stack(pred)
names(EC_rf_Map) = c("kfold01", "kfold02","kfold03","kfold04","kfold05",
                     "kfold06", "kfold07","kfold08","kfold09","kfold10")
  
# plot the predicted soil salinity maps for 10 fold cross validations
spplot(EC_rf_Map)


# calculate a mean and sd raster
meanR <- calc(EC_rf_Map, fun = mean)
sdR <- calc(EC_rf_Map, fun = sd)


# calculate upper and lower boundries (calculated the 90% confidence interval of prediction)
EC_upper = meanR + 1.64 * sdR
EC_lower = meanR - 1.64 * sdR


# plots
EC_rf_Map_final = stack(EC_lower, meanR, EC_upper)
names(EC_rf_Map_final) = c("Lower", "Mean","Upper")
spplot(EC_rf_Map_final)



############# 2) Quantile random forest (Lukas Schiesser)
library(quantregForest)
# make a geodatbase: covariates + soil data
covariates_RS <- stack(list.files("./cov/", pattern="\\.tif$", full.names = TRUE))
covariates_DEM <- stack(list.files("./SAGA/", pattern="\\.sdat$", full.names = TRUE))
covariates <- stack(covariates_RS,resample(covariates_DEM,covariates_RS))
covariates = aggregate(covariates, 9, fun=mean)
soil <- read.csv ("Uncertainty.csv", header = TRUE)
coordinates(soil) <- ~ x + y
proj4string(soil) <- CRS("+init=epsg:32639")
cov = raster::extract(covariates,soil,method='simple')
cov_soil = cbind(data.frame(cov),EC=soil$EC)
cov_soil <- cov_soil[complete.cases(cov_soil),]
names(cov_soil)

# define x and y
xData = cov_soil[,c(1:8)]
yData = cov_soil[,-c(1:8)]

xTrain = xData [c(1:140),]
yTrain = yData [c(1:140)]

xTest = xData [c(141:180),]
yTest = yData [c(141:180)]

# run the Quantile random forest
QRF = quantregForest(xTrain , yTrain )
print(QRF)

#  predict 
predictQRF = predict(QRF, newdata=xTest, quantiles=c(0.1,0.5,0.9))
predictQRF


# plot 90 prediction intervals
quantiles <- c(0.05,0.5,0.95)
quant <- predict(QRF,quantiles=quantiles,newdata=xTest)
z <- quant[,3]-quant[,1]
or <- order(z)
ynew <- yTest
n <- length(ynew)
med <- quant[or,2]
upp <- quant[or,3]-quant[or,2]
low <- quant[or,1]-quant[or,2]
ytrain <- ynew[or]-quant[or,2]


plot(1:n,ynew[or]-quant[or,2],pch=20,xlab="ordered samples",
      ylab="observed response and prediction
      intervals(centred)",type="n",main="90% prediction intervals")
dist <- 0.5
for (i in 1:n){
   polygon( c(i-dist,i+dist,i+dist,i-dist),
             c(upp[i],upp[i],low[i],low[i]) ,col=rgb(0.8,0.8,0.8) ,border=NA) }
for (i in 1:n){
  lines(c(i-dist,i+dist) , c(upp[i],upp[i]) )
  lines(c(i-dist,i+dist) , c(low[i],low[i]) )}
inpred <- (ytrain<= upp) & (ytrain>=low)
for (i in 1:n) points(i,ynew[or[i]]-quant[or[i],2],col=as.numeric(inpred)[i]+2,pch=20,cex=3)



############# 3) bootstraps (B. Malone)
library(remote)
library(raster)
library(sp)
library(randomForest)

# make a geodatbase: covariates + soil data
covariates_RS <- stack(list.files("./cov/", pattern="\\.tif$", full.names = TRUE))
covariates_DEM <- stack(list.files("./SAGA/", pattern="\\.sdat$", full.names = TRUE))
covariates <- stack(covariates_RS,resample(covariates_DEM,covariates_RS))
covariates = aggregate(covariates, 9, fun=mean)
soil <- read.csv ("Uncertainty.csv", header = TRUE)
coordinates(soil) <- ~ x + y
proj4string(soil) <- CRS("+init=epsg:32639")
cov = raster::extract(covariates,soil,method='simple')
cov_soil = cbind(data.frame(cov),EC=soil$EC)
cov_soil <- cov_soil[complete.cases(cov_soil),]
names(cov_soil)

# define train and test
cDat = cov_soil [c(1:140),]
vDat = cov_soil [c(141:180),]


# Fit RF models for each bootstrap
fit_RF = list()
for (i in 1:10) { # number of bootstarping
  trainingREP <- sample.int(nrow(cDat), 1.0 * nrow(cDat),replace = TRUE)
  fit_RF[[i]] <- randomForest(EC~., data = cDat[trainingREP,])}


# calibration data: accuracy assessment
rfMat <- matrix(NA, nrow = 10, ncol = 5)
for (i in 1:10) {
  rfMat[i, ] <- as.matrix(goof(observed = cDat$EC,predicted = predict(fit_RF[[i]], newdata = cDat)))}

rfDat <- as.data.frame(rfMat)
names(rfDat) <- c("R2", "concordance", "MSE", "RMSE", "bias")
colMeans(rfDat)


# Validation data:accuracy assessment
rfPred.V <- matrix(NA, ncol = 10, nrow = nrow(vDat))
rfMat <- matrix(NA, nrow = 10, ncol = 5)
for (i in 1:10) {
  rfMat[i, ] <- as.matrix(goof(observed = vDat$EC,predicted = predict(fit_RF[[i]], newdata = vDat)))}

rfDat <- as.data.frame(rfMat)
names(rfDat) <- c("R2", "concordance", "MSE", "RMSE", "bias")
colMeans(rfDat)


# spatial predictions
r1 = list()
for (i in 1:10) {r1[[i]] = predict(covariates, fit_RF[[i]])}

# Raster stack
r1 <- stack(r1)
spplot(r1)


# calculate a mean, sd, median, variance, cv, standard error
meanR <- calc(r1, fun = mean)
sdR <- calc(r1, fun = sd)
medianR <- calc(r1, median)
varR <- calcVar(r1)
cvR <- cv(r1)
sdER <- sdR/sqrt(10)

# upper prediction limit
bootMap.upper <- meanR + sdER
bootMap.lower <- meanR - sdER

# prediction interval range
PI <- bootMap.upper - bootMap.lower

# plot
r3=stack(bootMap.lower,meanR,bootMap.upper, PI)
names(r3) <- c("Lower", "Mean", "Upper", "PI")
spplot(r3)


############# ############# ############# ############# ############# ############# 
############# Feature selection
############# ############# ############# ############# ############# #############
# load data
soil <- read.csv ("FeatureSelction.csv", header = TRUE)
str(soil)
soil = soil[complete.cases(soil),]

############# 1) Boruta algorithm
# load library
library(Boruta)

BorutaTrain <- Boruta(Clay~., data = soil)
BorutaBank <- TentativeRoughFix(BorutaTrain)
plot(BorutaBank, xlab = "", xaxt = "n", main=paste0("Clay"))
lz <- lapply(1:ncol(BorutaBank$ImpHistory),
               function(i)BorutaBank$ImpHistory[is.finite(BorutaBank$ImpHistory[,i]),i])
names(lz) <- c(names(soil)[1:64],c("sh_Max","sh_Mean","sh_Min"))
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),at = 1:ncol(BorutaBank$ImpHistory),cex.axis = 1)


############# 2) Highly corrolated features
# load library
library(caret)

# load data
soil <- read.csv ("FeatureSelction.csv", header = TRUE)
str(soil)
soil = soil[complete.cases(soil),]
cov = soil[,-ncol(soil)]
names(cov)

# find highly corrolated covariates
high_cor=findCorrelation(cor(cov), cutoff = .80, exact = TRUE)

# remove highly corrolated covariates
soil_high=soil[,-high_cor]

# check how many coloums are removed
ncol(soil)
ncol(soil_high)


############# 3) Recursive Feature Elimination 
# load library
library(caret)

# load data
soil <- read.csv ("FeatureSelction.csv", header = TRUE)
str(soil)
soil = soil[complete.cases(soil),]

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

# run the RFE algorithm
results <- rfe(soil[,-ncol(soil)], soil[,ncol(soil)], sizes=c(1:8), rfeControl=control)

# summarize the results
print(results)



############# 4) Variable Importance Random Forest
#load library
library(randomForestExplainer)
library(randomForest)

# make a geodatbase: covariates + soil data
covariates_RS <- stack(list.files("./cov/", pattern="\\.tif$", full.names = TRUE))
covariates_DEM <- stack(list.files("./SAGA/", pattern="\\.sdat$", full.names = TRUE))
covariates <- stack(covariates_RS,resample(covariates_DEM,covariates_RS))
covariates = aggregate(covariates, 9, fun=mean)
soil <- read.csv ("Uncertainty.csv", header = TRUE)
coordinates(soil) <- ~ x + y
proj4string(soil) <- CRS("+init=epsg:32639")
cov = raster::extract(covariates,soil,method='simple')
cov_soil = cbind(data.frame(cov),EC=soil$EC)
cov_soil <- cov_soil[complete.cases(cov_soil),]
names(cov_soil)

# run random forest model
impo_rf_EC <- randomForest(EC ~ ., data=cov_soil,ntree=100, importance=T)
min_depth_frame <- min_depth_distribution(impo_rf_EC)
importance_frame <- measure_importance(impo_rf_EC)

# plot the results
plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15)


