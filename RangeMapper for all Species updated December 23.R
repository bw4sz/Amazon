all_files="C:/Users/Ben/Documents/Stony Brook/1st Year/R"
 require(rangeMapper)
 dbcon = rangeMap.start(file = "all_birds", dir = tempdir() , overwrite = TRUE)
# Download range vector files
setwd("C:/Users/Ben/Documents/Stony Brook/1st Year/R")
# Combine all ranges in one "SpatialPolygonsDataFrame"
   R = list() # a container for all ranges
  lst = selectShpFiles("Trochilidae")
  for(i in 1:nrow(lst)) {
                  ri = readOGR(lst$dsn[i], lst$layer[i], verbose = FALSE)
                  ri = spChFIDs(ri,  paste(i, 1:length(ri), sep = "." ) )
                  print(i)
                 R[[i]] = ri
  }
 R = do.call(rbind, R)
 proj4string(R) = CRS("+proj=longlat +datum=NAD83")
 setwd(wd)
 plot(R, border = "grey", axes = TRUE)
global.bbox.save(con = dbcon, bbox = R)
 gridSize.save(dbcon)
 canvas.save(dbcon)
 processRanges(spdf = R, con =  dbcon, ID = "SCI_NAME")
 rangeMap.save(dbcon)
 sr = rangeMap.fetch(dbcon)
 plot(sr)
richness<-raster(sr)

#Nighttime Lights
library(RCurl)
library(R.utils)
library(rgdal)
library(raster)
library(ggplot2)

hiResLights = raster("C:/Users/Ben/Documents/GIS/NightLight Calibrated/F16_20051128_20061224.cloud2.light1.marginal0.fg_15_35_55.full_swath.line_screened.shiftx2y2.op_blended.avg_vis.tif" )

#reproject lights raster

lights.crop<-crop(hiResLights,richness)
lights.resample<-resample(lights.crop,richness)

r = crop(hiResLights,e)
p = rasterToPoints(r)
df.lights = data.frame(p)
colnames(df.lights) = c("lon", "lat", "dn")
plot(richness)
h = rasterToPoints(richness)
df.richness = data.frame(h)
colnames(df.richness) = c("lon", "lat", "dn")

#Create datamatrix for richness
#Clip and get means for polygons based on each raster

#test for ecuador, with low quality data...
#Get Ecuador Political Boundaries
Ecuador<-getData('GADM', country='ECU', level=0)
#Get Peru Boundaries
Peru<-getData('GADM', country='PER', level=0)

#Crop the Night Lights By the political boundaries
r = crop(hiResLights,extent(Ecuador))
plot(r)
plot(Ecuador,add=TRUE)
#Get the Precip Variables
biovar<-getData('worldclim', var='bio', res=10)
#Ecuador
#Plot Ecuador and the biovar==12 annual precip


#Bazil
Brazil<-getData('GADM', country='BRA', level=0)
plot(precip[[12]])
plot(Brazil,add=TRUE)


#Write a function are two polygons different based on raster data
#Step 1, what are the inputs, x= polygon 1, y= polygon 2, z= raster to compare

PolyRasDiff<-function(w,x,y,z){ 
polys <- list(w,x,y)
v <- lapply(polys, function(a) extract(z,a, fun= mean, na.rm=TRUE))
o<-matrix(ncol=nlayers(z),nrow=3, data=unlist(v),byrow=TRUE)
colnames(o)<-colnames(v[[1]])
print(o)}

#Test of the function, richness rasters
richness<-PolyRasDiff(Ecuador,Brazil,Peru, richness)

#It works, now for all the bioclimatic variables
bio<-PolyRasDiff(Ecuador,Brazil,Peru, biovar)

#Compute TRI, topographic roughness index
Elev(raster())
TRI <- focal(Elev, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8)