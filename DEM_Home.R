#Load necessary packages
library(ithir)
library(sp)
library(raster)
library(rgdal)
library(leaflet)
library(magrittr)
library(RSAGA)
library(envirem)
library(lidR)
library(htmltools)


##############################################################################################################################################
#Set Working Directory
setwd("G:/Soil Sci 585/HW2/Soil-585/Homework 2")

## 1. Input C_Spatialpoints.csv and convert it to SpatialPointsDataFrame, define the CRS using X and Y with CRS("+init=epsg:32618") 
Spatialpoints <- read.csv(file = "C_Spatialpoints.csv")
Spatialpoints
coordinates(Spatialpoints) <- ~X + Y
str(Spatialpoints)
proj4string(Spatialpoints) <- CRS("+init=epsg:32618")

## Plot "CLAY" using spplot
## You need to input the R package "sp"
spplot(Spatialpoints, "CLAY", scales = list(draw = T), cuts = 5,
       col.regions = bpy.colors (cutoff.tails = 0.1, alpha = 1), cex = 1)

##############################################################################################################################################
#Problem 2 (Done)
## 2. Input grid_5m.csv and convert the Elevation data to a raster layer using rasterFromXYZ
grid_5m <-read.csv("grid_5m.csv")
grid_5m
DEM <- rasterFromXYZ(grid_5m)
DEM

#Plotting DEM
spplot(DEM)
#Plotting DEM with Spatialpoints
plot(DEM)
points(Spatialpoints, pch= 20)

## Define the CRS using x and y using epsg number: CRS("+init=epsg:32618") and plot it using plot and spplot
proj4string(DEM) <- CRS("+init=epsg:32618")
## Save the raster layer to a Geotiff file
writeRaster(DEM, filename = "DEM.tif", format = "GTiff", overwrite= TRUE)

## Hint: You need to input the R package "raster"
## Hint2: rasterFromXYZ requires an input of a dataframe of only three columns (first two are coordinates and the third one is the data)

#################################################################################################################################################
#Problem 3 (Done)
## 3. Input the saved tiff file of the elevation data and assign it to a variable named "elevation" 
elevation <- raster("USGS_one_meter_x69y340_TX_Eastern_B1_2018.tif")
plot(elevation)
str(elevation)
names(elevation)

## Calculate the following terrain parameters using "terrain" function: ('Terrain' requires RasterLayer)
## slope, aspect, tpi, tri, roughness, flowdir 
slope <- terrain(elevation, opt=c("slope"), unit="degrees")    
aspect <- terrain(elevation, opt=c("aspect"), unit="degrees")
tpi <- terrain(elevation, opt=c("tpi"))  ## Topographic Position Index
tri <- terrain(elevation, opt=c("tri"))  ## Terrain Ruggedness Index
roughness <- terrain(elevation, opt=c("roughness"))
flowdir <- terrain(elevation, opt=c("flowdir"))

###################################################################################################################################################
#Problem 4 (Done)
## 4. Stack all the terrain parameters including elevation data into a new variable called "terrain"
terrain <- stack(elevation, slope, aspect, tpi, tri, roughness, flowdir)

## Extract "terrain" values to the spatial points created from Step 1 using the default method ("simple")
terrain1 <- raster::extract(terrain, Spatialpoints, method = 'simple')
names(terrain1)
summary(terrain1)

## Combine the original point data with the extracted raster values into a dataframe
Spatialpoints1 <- data.frame(Spatialpoints)
C_Spatialpoints <- cbind(terrain1, Spatialpoints1)
## Save it as a csv file.
write.csv(x = C_Spatialpoints, file = "C_Spatialpoints.csv")

##########################################################################################################################
#Problem 5 (Done)
## 5. Input Lidar data "las2_2.laz" using "readLAS"
las2 <- readLAS("las2_2.laz")
las2

## Plotting the elevation data of the lidar data
plot(las2)

## Plotting the color of the lidar profile as an RGB image
plot(las2, color ="RGB")

## Creates a digital surface model (raster file) from the lidar data with "res = 0.01, dsmtin()" and plot it 
## Hint: You need to input R package "lidR" and "htmltools"
las2_1 <- grid_canopy(las2, res = 0.01, dsmtin())
col <- height.colors(100)
col
plot(las2_1, col = col)




