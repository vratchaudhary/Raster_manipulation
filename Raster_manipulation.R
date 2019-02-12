
##This script is for using landscape raster data and shpfile
##to get by pixl information about the data characteristics-like year of deforestation etc
#install.packages("raster") #for reading, writing, analyzing, and modeling spatial data
#install.packages("rgdal") #read/write spatial objects, project data
#install.packages("sp") #define data classes, some methods for visualizing, selecting, subsetting
#install.packages("rgeos") #creates new geometries 
#install.packages("dplyr") #data wrangling
#install.packages("rasterVis") #visualization 
library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(dplyr)
library(rasterVis)
# Importing transmission line using readOGR from rgdal

ts_line <- readOGR(dsn = ".", layer= "NE_line1")
##look at the line
class(ts_line)
##lets look at its summary
summary(ts_line)
##letsplot it
plot(ts_line)
##look at its projection
crs(ts_line)
#########we would like to bring this projection to UTM 235
line <- spTransform(ts_line, CRS("+init=epsg:29183")) 
###########Now we will create the buffer of 20 km around the line
buff<- 2000
linebuff<- gBuffer(line, width=buff, byid=F)
###############
plot(lineBuff)
############
#################lets import the raster 
# This raster shows year of deforestation for each pixel (1-14 = 2001-2014, 0 = still forest)
Han <- raster("./NE_HanAm.tif")
##lets look at it
Han
##lets plot it
plot(Han)
######lets bring raster and buffer to same co-ord system and clip raster to buffer
lineBuff <- spTransform(lineBuff, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" )
crs(lineBuff)
####################
Han_clip <- crop(Han, extent(lineBuff), snap= "out")
plot(Han_clip, main = "Result after applying crop")
##########
Hansen_buff <- mask(x=Han_clip, mask=buf_raster)
# creates a new raster object that has the same values as X
writeRaster(Hansen_buff, "hansen_buffer", "GTiff", overwrite= TRUE)
# save the raster as a GeoTiff file
#plot with the ts line
plot(Hansen_buff, main = "20 km Buffer around Transmission Line")
lines(ts_line, col = "red")
# Save the deforestation year and pixel ID as a dataframe
def_year_df <- as.data.frame(values(Hansen_buff))
head(def_year_df)

lat <- as.data.frame(init(Hansen_buff, 'x'))
long <- as.data.frame(init(Hansen_buff, 'y'))
# init creates a new raster layer with values reflecting a specified cell property, ie lat/long
# Find the distance of the center of each pixel to the transmission line
# Convert raster to points in order to use gDisatance to find distance values
Han_points <-  as(Hansen_clip,"SpatialPoints")
# save it as a spatialPoints object
str(Han_points)
crs(Han_points)

Han_points <- spTransform(Han_points, CRS("+init=epsg:29183"))

crs(line)
crs(Han_points)

class(Han_points)

#takes ~30min to run
dist <-  gDistance(Han_points, line, byid=TRUE)

class(dist)
#finds the distance from every point to all 28 lines. Inputs are the two files 
# and buid= true- finds distance to each line

dim(dist)
#[1]       28 17440572
#there are 28 possible lines and 17440572 pixels - this found distance to EACH line

#find the minimum distance between the a pixel and the nearest line- reduce it to one value
dmin <- apply(dist, 2, min)
#returns an array of values by applying the designated function to a matrix


Han_buff[] <- dmin
#check to see if the values went into the raster
values(Hansen_buff)


plot(Han_buff, col= heat.colors(14), main = "Raster of Pixel Distance from Transmission Line")
lines(ts_line, col = "black")
#dev.print(pdf, "Buff_distance.pdf")

distance_df <- as.data.frame(values(Han_buff))
#save the values from the distance raster in dataframe format

################################################################
#cleaning the data

#bind two dataframes together
final_df <- cbind(distance_df, def_year_df, lat, long)
#cbind binds dataframes together

##name columns
colnames(final_df) = c("distance(m)", "year_def", "Lat", "Long")
head(final_df) #check

#remove NA values-
final_df <- na.omit(final_df)
#remove 0s, also still forested. na.omit removes rows with NA values
final_df <- filter(final_df, final_df[,2] != 0)

#save CSV file
write.csv(final_df, "Results.csv")

##how to make spatial data from a csv file
#turn a csv into a spatial object
coordinates(final_df) <- ~Long+Lat #columns that correspond to the lat/long in the csv
proj4string(final_df) = CRS("+proj=longlat +datum=WGS84") #geogrpahic cooridnate system

class(final_df)

plot(final_df)
