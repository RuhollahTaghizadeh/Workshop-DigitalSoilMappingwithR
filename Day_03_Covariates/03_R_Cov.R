############################################################################################
# R script belonging to the tutorial 'Preparing Covariates for DSM'
# 14 02 2021
# Author: Dr. R. Taghizadeh, Tuebingen University
############################################################################################
# empty memory and workspace
gc()
rm(list=ls())

# check directory
getwd()

# load library
library(sp)
library(raster)
library(rgdal)
library(plotKML)
library(mapview)
library(corrplot)


# import raster layers of covarites (RS data + DEM)
blue = raster("./cov/Blue.tif")
green = raster("./cov/Green.tif")
red = raster("./cov/Red.tif")
nir = raster("./cov/Nir.tif")
swir1 = raster("./cov/swir1.tif")
swir2 = raster("./cov/swir2.tif")
dem = raster("./cov/DEM.tif")

# plot raster layers of covarites (RS data + DEM)
plot(blue, main = "blue band of landsat", xlab = "Easting (m)", ylab = "Northing (m)")
plot(dem, main = "digital elevation model", xlab = "Easting (m)", ylab = "Northing (m)")

# calculate some RS indices such as NDVI and salinity index
ndvi = (nir - red) / (nir + red)
salin_ind = (red - nir) / (green + nir)

# plot ndvi on google earth
plotKML(ndvi)

# make a stack file from RS rasters
landsat = stack(blue,green,red,nir,swir1,swir2,ndvi,salin_ind)

# set the names to the rasters in the stack file
names(landsat) <- c("blue","green","red","nir","swir1","swir2","ndvi","salin_ind")


# calclulate some tearrain attributes from DEM
# SAGA 
if(.Platform$OS.type=="unix"){
  saga_cmd = "saga_cmd"
}
if(.Platform$OS.type=="windows"){
  saga_cmd = "C:/ProgramData/SAGA-GIS/saga_cmd.exe"# YOU MUST CHANGE THIS LINE
}
saga_cmd


# convert DEM to SpatialGridDataFrame
DEM.Grid <- as(dem, 'SpatialGridDataFrame')

# export SpatialGridDataFrame to SAGA format in the SAGA folder
writeGDAL(DEM.Grid["DEM"], "./SAGA/DEM.sdat", "SAGA")  

# calculate different terrain attributes using the following codes 
#(see:(http://www.saga-gis.org/saga_tool_doc/2.3.0/a2z.html))

# valley depth
system(paste0(saga_cmd, 
              ' ta_channels 7 -ELEVATION=\"SAGA/DEM.sgrd\" -VALLEY_DEPTH=\"SAGA/VALLEY_DEPTH.sgrd\"')) 
# topographic position index
system(paste0(saga_cmd, 
              ' ta_hydrology 15 -DEM=\"SAGA/DEM.sgrd\" -TWI=\"SAGA/TWI.sgrd\" '))  
# MrVBF
system(paste0(saga_cmd, 
              ' ta_morphometry 8 -DEM=\"SAGA/DEM.sgrd\" -MRVBF=\"SAGA/MRVBF.sgrd\" -T_SLOPE=10 -P_SLOPE=3'))
# Basic Terrain Analysis
system(paste0(saga_cmd, 
              ' ta_compound 0 -ELEVATION=\"SAGA/DEM.sgrd\" -LSFACTOR=\"SAGA/LSFACTOR.sgrd\" -FLOW=\"SAGA/FLOW.sgrd\" -CHNL_BASE=\"SAGA/CHNL_BASE.sgrd\" -RSP=\"SAGA/RSP.sgrd\" '))  


# import the terrain aatibutes using list.files command
dem_lst <- list.files("./SAGA/", pattern="\\.sdat$", full.names = TRUE)
dem_lst

# make a stack file from terrain attributes 
terran = stack(dem_lst)

#plot to see SAGA is working well
plot(terran)


# resampling RS data to terrain attributes, then make a stack file 
RS_terrain_resample = resample(landsat,terran, method="bilinear")

# make a stack file from RS data and terrain attributes
covariate_stack = stack(terran, RS_terrain_resample)
names(covariate_stack)

# export covariate_stack to the computer
writeRaster(covariate_stack, filename="./cov/covariate_stack.tif", options="INTERLEAVE=BAND", format="GTiff")

# SAVE ALL INFORMATION
save.image(file = "covariates.RData")


# extract values from covariate_stack
# import the point soil data from desktop
soil_data <- read.csv ("soil.csv", header = TRUE)

# inspect soil point data
str(soil_data)

# convert soil point data to spatial point data  
soil_gis = soil_data
coordinates(soil_gis) <- ~ x + y
ZoneUTM <- c("+init=epsg:32639")
proj4string(soil_gis) <- CRS(ZoneUTM)

# inspect spatial point data
str(soil_gis)

# plot spatial point data
spplot(soil_gis)
plotKML(soil_gis)
mapview(soil_gis)


# plot the point on the raster
plot(covariate_stack$ndvi, main = "NDVI", xlab = "Easting (m)", ylab = "Northing (m)")
plot(soil_gis, add =T,pch = 19)


# extract values (without buffer)
cov = raster::extract(covariate_stack,soil_gis,method='simple')

# convert to data frame
cov1 = data.frame(cov)
str(cov1)
names(cov1)

# combing the covariates + soil data
cov_soil = cbind(cov1,EC= soil_data$EC)
str(cov_soil)
names(cov_soil)

# export cov_soil file 
write.csv(cov_soil, "cov_soil.csv")

# check the corrolation covariates and EC
corrplot.mixed(cor(cov_soil), lower.col = "black", number.cex = .7)

