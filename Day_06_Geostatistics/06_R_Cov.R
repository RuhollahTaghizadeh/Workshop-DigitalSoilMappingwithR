############################################################################################
# R script belonging to the tutorial 'Geostatistics'
# 03 03 2021
# Author: Dr. R. Taghizadeh, Tuebingen University
############################################################################################
# empty  workspace
rm(list=ls())

# check directory
getwd()
dir()

# load library
# if you dont have a package: install.packages("NAME")
library(sp)
library(rgdal)
library(maptools)
library(gstat)
library(rgeos)
library(MASS)
library(randomForest)
library(plotKML)
library(gridExtra)
library(ggcorrplot)
library(raster)

# import the point soil data 
soil.df <- read.csv ("soil.csv", header = TRUE)

# assign to new object
soil = soil.df

# inspect data
str(soil)
hist(soil$EC, main = "Topsoil Soil Salinity (dS/m)", col="red")


# convert soil point data to spatial point data  
coordinates(soil) <- ~ x + y


# set projection
proj4string(soil) <- CRS("+init=epsg:32639")


# plot data
spplot(soil)


# import boundary study area 
study_area <- readOGR("./GIS/poly.shp")


# export boundry as a ascii file
study_area_grid <- vect2rast(study_area, cell.size=90)


# plot observations
spplot(soil, zcol = "EC", cex = 1.5, main = "EC (dS/m)",
       sp.layout = list(list("sp.polygons", study_area)), 
       scales=list(draw=T), col.regions = bpy.colors(5))


################################################### geostatistics-kriging
# compute experimental semivariogram
gstat_EC <- gstat(formula = EC~1, data = soil)
vg_EC <- variogram(gstat_EC)
plot(vg_EC, plot.nu=FALSE)


# define initial parameters of semivariogram model
vg_parameters_EC <- vgm(nugget = 1000, psill = 4000, range = 9000, model = "Sph")
plot(vg_EC, vg_parameters_EC)


# fit semivariogram model
vg_model_EC <- fit.variogram(vg_EC, vg_parameters_EC)
plot(vg_EC, vg_model_EC)
str(vg_model_EC)
vg_model_EC
attr(vg_model_EC, "SSErr")


# ordinary point kriging
EC_krig <- krige(formula = EC~1, locations = soil, newdata = study_area_grid,
                   model = vg_model_EC)

names(EC_krig)

# plot the predicted EC by kriging
spplot(EC_krig, zcol = "var1.pred", main="EC predictions (dS/m)")

# calculate standard devuation of EC and plot
EC_krig$var1.sd <- sqrt(EC_krig$var1.var)
spplot(EC_krig, zcol = "var1.sd", sp.layout=list(soil, pch=1, cex=2),
       main = "EC SD (dS/m)")


# plot two predicted maps
grid.arrange(spplot(EC_krig, "var1.sd", xlab = "x", ylab = "y", main = "SD of EC (dS/m)") , 
             spplot(EC_krig, "var1.pred", xlab = "x", ylab = "y", main = "EC Kriged (dS/m)" ))


# plot two images
par(mfrow =c(1,2))
hist(EC_krig$var1.pred, main="Pred EC Kriged (dS/m)",col="red", xlab= "EC predicted")
hist(EC_krig$var1.sd, main="SD EC Kriged (dS/m)", col="blue", xlab= "EC SD")
dev.off()


# IDW 
EC_idw <- krige(formula = EC~1, locations = soil, newdata = study_area_grid)
names(EC_idw)
spplot(EC_idw, zcol = "var1.pred", main="EC predictions (dS/m)")
summary(EC_idw)


# plot two images
par(mfrow =c(1,2))
hist(EC_idw$var1.pred, main="Pred EC IDW (dS/m)",col="red", xlab= "EC predicted")
hist(EC_krig$var1.pred, main="Pred EC Kriged (dS/m)",col="blue", xlab= "EC predicted")
dev.off()



# plot two predicted maps
grid.arrange(spplot(EC_idw, "var1.pred", xlab = "x", ylab = "y", main = "EC IDW (dS/m)") , 
             spplot(EC_krig, "var1.pred", xlab = "x", ylab = "y", main = "EC Kriged (dS/m)" ))



################################################### geostatistics-regression kriging
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
cov_soil = cbind(data.frame(cov),EC= soil.df$EC)

# inspect the covariates + soil data 
str(cov_soil)

# check the corrolation between EC and ovariates
ggcorrplot(cor(cov_soil), hc.order = TRUE, type = "lower",lab = TRUE)


# fit random forest model
rf_fit <- randomForest(EC ~ ., data=cov_soil, ntree=1000)


# append residuals to geul dataset
cov_soil$residuals <- rf_fit$predicted - cov_soil$EC
names(cov_soil)
summary(cov_soil)

# histogram of the residuals 
hist(cov_soil$residuals, col="lightblue")


# convert cov_soil to spatial data
cov_soil$x <- soil.df$x
cov_soil$y <- soil.df$y
coordinates(cov_soil) <- ~ x + y
proj4string(cov_soil) <- CRS("+init=epsg:32639")
spplot(cov_soil)  
  

#  compute experimental semivariogram
gstat_res <- gstat(formula = residuals~1, data = cov_soil)
vg_res <- variogram(gstat_res)
plot(vg_res, plot.nu=FALSE)


# define initial semivariogram model
vg_parameters_res <- vgm(nugget = 2100, psill = 500, range = 3000, model = "Gau")
plot(vg_res, vg_parameters_res)


# fit semivariogram model
vg_model_res <- fit.variogram(vg_res, vg_parameters_res)
plot(vg_res, vg_model_res)
vg_model_res


# ordinary point kriging
res_krig <- krige(formula = residuals~1, locations = cov_soil, newdata = study_area_grid,
                 model = vg_model_res)


# plot the residuals map
spplot(res_krig, zcol = "var1.pred", main="residuals predictions")


# random forest prediction part 
map_rf <- raster::predict( covariates,rf_fit)


# plot the RF map
spplot(map_rf, main="EC map based on RF model")


# obtain regression kriging prediction
res_krig_raster <- resample(raster(res_krig) , map_rf)

RK_map <- res_krig_raster + map_rf

# plot the RK map
spplot(RK_map, main="EC map based on RK model")


# viwe multiple maps
grid.arrange(spplot(EC_idw, "var1.pred", main = "EC IDW (dS/m)") , 
             spplot(EC_krig, "var1.pred", main = "EC Kriged (dS/m)" ),
             spplot(res_krig_raster, main = "residuals Kriged (dS/m)" ),
             spplot(map_rf, main = "EC RF (dS/m)" ),
             spplot(map_rf, main = "EC RK (dS/m)" ))


################################################### geostatistics-spatial stochastic simulations
EC_sim <- krige(EC~1, soil, newdata = study_area_grid, vg_model_EC, nsim = 9, nmax = 24)

names(EC_sim)

spplot(EC_sim, zcol = "sim1",main = "sim1")
spplot(EC_sim, zcol = "sim9",main = "sim2")

# make animations
library(raster)
R<-stack(EC_sim)
animate(R, n=1)

# make a gif
library(gganimate)
library(magick)
library(dplyr)
library(ggplot2)

df.sim = as.data.frame(EC_sim,xy=T)

df.sim.plot <- tidyr::gather(df.sim,"simulate","value",1:9)
names(df.sim.plot)

simlist = c("sim1" ,"sim2" ,"sim3","sim4","sim5","sim6","sim7","sim8","sim9")
for (id in simlist) {
  
  p <-
    df.sim.plot %>%
    dplyr ::filter(simulate == id) %>%
    ggplot(aes(x,y,fill=value)) +
    geom_raster() +
    scale_fill_gradientn(colours=c("#ffffd9","#edf8b1","#c7e9b4","#7fcdbb",
                                   "#41b6c4","#1d91c0","#225ea8","#253494","#081d58"))+
    labs(x = "long", y = "lat", title = id)+
    theme_bw () + 
    coord_equal(ratio = 1) 
    
  fp <- file.path(".", paste0(id, ".png"))
  
  ggsave(plot = p, 
         filename = fp, 
         device = "png")}

## list file names and read in
imgs <- list.files(".", pattern="\\.png$", full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 1)
image_write(image = img_animated,path = "sim_gif.gif")

