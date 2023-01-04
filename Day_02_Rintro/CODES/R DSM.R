#To install all required R packages
ls <- c("reshape", "Hmisc", "rgdal", "raster", "sf", "GSIF", "plotKML", 
        "plyr", "ROCR", "randomForest", "quantregForest", 
        "psych", "mda", "h2o", "h2oEnsemble", "dismo", "grDevices", 
        "snowfall",?"hexbin", "lattice", "ranger", 
        "soiltexture", "aqp", "colorspace", "Cubist",
        "randomForestSRC", "ggRandomForests", "scales",
        "xgboost", "parallel", "doParallel", "caret", 
        "gam", "glmnet", "matrixStats", "SuperLearner",
   ?    "intamap", "fasterize", "viridis","leaflet")
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#To install the most up-to-date version of plotKML/GSIF
if(!require(GSIF)){
  install.?ackages("GSIF", repos=c("http://R-Forge.R-project.org"), 
                   type = "source", dependencies = TRUE)}

#To test if these packages work properly, create soil maps and visualize them in Google Earth
library(GSIF)
library(sp)
library(boot)
libra?y(aqp)
library(plyr)
library(rpart)
library(splines)
library(gstat)
library(quantregForest)
library(plotKML)

demo(meuse, echo=FALSE)
omm <- fit.gstatModel(meuse, om~dist+ffreq, meuse.grid, method="quantregForest")
om.rk <- predict(omm, meuse.grid)
om.rk
p?otKML(om.rk)

#Connecting R and SAGA GIS
if(.Platform$OS.type=="unix"){
  saga_cmd = "saga_cmd"
}
if(.Platform$OS.type=="windows"){
  saga_cmd = "C:/ProgramData/SAGA-GIS/saga_cmd.exe"
}
saga_cmd

#To test if SAGA GIS work properly
library(plotKML)
library(?gdal)
library(raster)

data("eberg_grid")
gridded(eberg_grid) <- ~x+y
proj4string(eberg_grid) <- CRS("+init=epsg:31467")
writeGDAL(eberg_grid["DEMSRT6"], "./DEMSRT6.sdat", "SAGA")
system(paste(saga_cmd, 'ta_lighting 0 -ELEVATION "./DEMSRT6.sgrd" 
         ?   -SHADE "./hillshade.sgrd" -EXAGGERATION 2'))

plot(readGDAL("./hillshade.sdat"))


# Load the libraries
library(leaflet)

#leaflet example
popup = c("WORK", "HOME", "SWRI")
leaflet() %>%
  addProviderTiles("Stamen.Watercolor") %>%
  addProviderTiles("St?men.TonerHybrid") %>%
  addMarkers(lng = c(9, 54, 50),
             lat = c(48, 31, 35), 
             popup= popup)

plot(x, sin(x), type ="l", col="red", main="Sin Curve") 
