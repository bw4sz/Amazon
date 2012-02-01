#Amazon Borders, Do the amazon borderlands harbor increased cultural and biodiversity?
#In Colloboration with David Salisbury 12/26/11
# Submitted Human Ecology and Land Use Policy


#Create avian richness rasters for the Western Hemisphere, built from Natureserve range maps. Utilizing the package, rangeMapper.
all_files="C:/Users/Ben/Documents/NatureServe"
setwd("C:/Users/Ben/Documents/NatureServe")
dirs<-list.dirs(all_files,full.names=TRUE)
dirs<-dirs[-c(57,81)]
dirs
#skip 74,"C:/Users/Ben/Documents/NatureServe/Pycnonotidae",101, 104, 119
dirs<-dirs[120]

require(rangeMapper)
require(raster)
ranges<-lapply(dirs,function(x){
 dbcon = rangeMap.start(file = "all_birds", dir = getwd() , overwrite = TRUE)
# Download range vector files
# Combine all ranges in one "SpatialPolygonsDataFrame"
   R = list() # a container for all ranges
  lst = selectShpFiles(x)
  for(i in 1:nrow(lst)) {
                  ri = readOGR(lst$dsn[i], lst$layer[i], verbose = FALSE)
                  ri = spChFIDs(ri,  paste(i, 1:length(ri), sep = "." ) )
                  print(i)
                 R[[i]] = ri
  }
 R = do.call(rbind, R)

proj4string(R) = CRS("+proj=longlat +datum=NAD83")
#Create Bounding Box
global.bbox.save(con = dbcon, bbox = Amazon)
print("cleared")
 gridSize.save(dbcon, gridSize=1)
 canvas.save(dbcon)
 processRanges(spdf = R, con =  dbcon, ID = "SCI_NAME")
 rangeMap.save(dbcon)
sr<-rangeMap.fetch(dbcon)
writeRaster(raster(sr),paste(x,"richn",sep=""),overwrite=TRUE)
print(raster(sr))
})
alltif<-do.call(stack,ranges)
finalmap<-sum(alltif)
writeRaster(finalmap,"finalmap2",overwrite=TRUE)
allgrd<-list.files(pattern=".grd")
grdstack<-do.call(stack,list(allgrd))
allrich<-overlay(grdstack,fun=function(x) sum(x,na.rm=TRUE))

setwd("C:\\Users\\Ben\\Documents\\Natureserve\\Correctedgrids")
correcgrd<-list.files(pattern=".grd")
corr.list<-as.list(correcgrd)
corr.list.e<-lapply(corr.list,function(x) resample(raster(x),grdstack))
corrstack<-do.call(stack,corr.list.e)
corrich<-overlay(corrstack,fun=function(x) sum(x,na.rm=TRUE))
plot(sum(corrich,allrich))
finalrich<-sum(corrich,allrich)
writeRaster(finalrich,"avianAmazon")

#Are borderlands areas more remote? Does remoteness correlate with language and avian diversity?
# Using the night time calibrated lights from NOAA, calibrated for most sensitive, lowest emmitance data

library(rgdal)
require(raster)
library(ggplot2)
require(stats)
hiResLights = raster("C:/Users/Ben/Documents/GIS/NightLight Calibrated/F16_20051128_20061224.cloud2.light1.marginal0.fg_15_35_55.full_swath.line_screened.shiftx2y2.op_blended.avg_vis.tif" )

#reproject lights raster
#Resample lights to make it analogous to the species richness rasters

lights.mask<-crop(hiResLights,Lowland)
lights.agg<-aggregate(lights.mask,100)
lights.resample<-resample(lights.mask,finalrich)


#Correlate avian richness and nighttime lights
p = rasterToPoints(lights.resample)
df.lights = data.frame(p)
colnames(df.lights) = c("lon", "lat", "dn")
h = rasterToPoints(crop(finalrich,Lowland))
df.richness = data.frame(h)
colnames(df.richness) = c("lon", "lat", "dn")
plot(df.lights[,3]~df.richness[,3], main="Remoteness promotes biodiversity",xlab="Hummingbird Richness",ylab="Night Lights")
y<-cor(x=df.lights[,3],y=df.richness[,3], method="spearman")
summary(y)

#Create datamatrix for richness
#Clip and get means for polygons based on each raster

#test for ecuador, with low quality data...
#Get Political Boundaries
setwd("C:/Users/Ben/Documents/Stony Brook/1st Year/R")
km50<-readShapePoly("AmazonBorderlands1M_50km_ClipLowlandRF_GCS.shp", proj4string=CRS("+proj=lonlat"))
km150<-readShapePoly("AmazonBorderlands1M_150km_ClipLowlandRF_GCS.shp",proj4string=CRS("+proj=lonlat"))
Lowland<-readShapePoly("LowlandForestAmazoniaGCS.shp",proj4string=CRS("+proj=lonlat"))
Amazon<-readShapePoly("AmazonianCountriesGCS.shp",proj4string=CRS("+proj=lonlat"))

#Plot Boundaries
plot(richness, ext=studybox)
plot(Amazon,type='l',add=TRUE)
plot(km150,type='l',add=TRUE)
plot(km50, type='l', add=TRUE)


plot(lights.mask, ext=Lowland, breaks= seq(1, 10, by=1),col=colorRampPalette(c("blue", "red"))(255), xaxt="n",yaxt="n")
plot(Amazon,add=TRUE)
plot(Lowland,add=TRUE)
plot(km150,lty="dotted",add=TRUE)
plot(km50,lty="dashed",add=TRUE)

#Crop the Night Lights By the Amazon political boundaries
studybox<-bbox(Amazon)
r = crop(hiResLights,studybox)
#Get the Precip Variables
biovar<-getData('worldclim', var='bio', res=2.5)

#Write a function are two polygons different based on raster data
#Step 1, what are the inputs, x= polygon 1, y= polygon 2, z= raster to compare

require(reshape)

#Create Boxplots
PolyRasBox<-function(w,x,y,z){ 
poly <- list(w,x,y)
names(poly)<-c("km50","km150","Lowland")
envplot<-lapply(poly, function(b) extract(z,b,na.rm=TRUE))
melt.box<-melt.list(envplot)
}

bioclim<-PolyRasBox(km50,km150,Lowland, biovar)



polymean<-function(y){
g<-subset(bioclim,bioclim$X2==y)
tapply(g$value,factor(g$L1),mean)}

bioclim.mean<-sapply(unique(bioclim$X2),polymean)

#Get means
precip<-subset(bioclim,bioclim$X2=="bio12")
tapply(precip$value,factor(precip$L1),mean)

#Single Boxplot Richness
boxplot(value~factor(L1),data=richness.box)

boxplotPoly<-function(y){ 
u<-unique(y$L1)
sapply(u,function(x) boxplot(value~factor(L1),data=y,subset=y$L1=="x"))}

boxplotPoly(richness.box)

richnessstat<-TukeyHSD(aov(value ~ factor(L1), data=richness.box))

#Ttests for bioclim data
biotest<-TukeyHSD(aov(value ~ factor(L1), data=subset(bioclim,bioclim$X2=="bio1")))
boxplot(value~factor(L1),data=bioclim)

bioclim.tukey<-sapply(unique(bioclim$L1),function(x) boxplot(value~factor(L1),data=bioclim,subset=bioclim$L1=="x"))}


#Lights Stat updated
light.box<-PolyRasBox(km50,km150,Lowland,lights.mask)
light.mean<-tapply(light.box$value,factor(light.box$L1),function(x) mean(x,na.rm=TRUE))
a<-aov(value ~ factor(L1), data=light.box)
light.stat<-TukeyHSD(a)
boxplot(value~factor(L1),data=light.box)



#Avian Statistics
richness.box<-PolyRasBox(km50,km150,Lowland,finalrich)
rich.mean<-tapply(richness.box$value,factor(richness.box$L1),function(x) mean(x,na.rm=TRUE))
rich.stat<-TukeyHSD(aov(value ~ factor(L1), data=richness.box))



#It works, now for all the bioclimatic variables
bio.box<-PolyRasBox(km50,km150,Lowland, biovar)
TRI.mean<-tapply(bio.box$value,factor(bio.box$L1),mean)
TRIstat<-TukeyHSD(aov(value ~ factor(L1), data=bio.box))



write.csv(bio, "EnvClip.csv")

#Elevation
#elev<-raster("alt.bil")
elev<-raster("alt/alt/w001001.adf")
elev.crop<-crop(elev,studybox)


elev.box<-PolyRasBox(km50,km150,Lowland, elev.crop)
elev.mean<-tapply(elev.box$value,factor(elev.box$L1),count)
Elevstat<-TukeyHSD(aov(value ~ factor(L1), data=elev.box))


#Compute TRI, topographic roughness index
TRI <- focal(elev.crop, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8)
TRI.box<-PolyRasBox(km50,km150,Lowland, TRI)
TRI.mean<-tapply(TRI.box$value,factor(TRI.box$L1),mean)
TRIstat<-TukeyHSD(aov(value ~ factor(L1), data=TRI.box))
 

#Is avian richness correlated with remoteness?
lapply(list(km150,km50,Lowland,Amazon),function(x){
lights.crop<-crop(hiResLights,x)
richness.crop<-crop(finalrich,x)
lights.resample<-resample(lights.crop,richness.crop)
lights.masked<-mask(lights.resample,richness.crop)
g<-mantel(lights.masked,richness.crop,method="spearman")})


#What is the language diversity in each clip
#Create point in raster heat map for languages
require(sp)
require(rgdal)
require(maps)
 
# read in language data, and turn it into a SpatialPointsDataFrame
language.range<-readShapePoly("langaAmazonCountriesGCS_WGS84.shp", proj4=CRS("+proj=lonlat"))
require(rangeMapper)
require(raster)
require(maptools)
dbcon = rangeMap.start(file = "all_language", dir = getwd() , overwrite = TRUE)
# Download range vector files


#Create Bounding Box
global.bbox.save(con = dbcon, bbox = Amazon)
print("cleared")
 gridSize.save(dbcon, gridSize=1)
 canvas.save(dbcon)
 processRanges(spdf = language.range, con =  dbcon, ID = "NAME_PROP")
 rangeMap.save(dbcon)
sr<-rangeMap.fetch(dbcon)
language.rich<-raster(sr)


#Stats the language rasters
lang.box<-PolyRasBox(km50,km150,Lowland, language.rich)
lang.mean<-tapply(lang.box$value,factor(lang.box$L1),function(x) mean(x,na.rm=TRUE))
langstat<-TukeyHSD(aov(value ~ factor(L1), data=lang.box))
boxplot(value~factor(L1),data=lang.box)

#Ecosystem Data
ecosystem<-raster("C:\\Users\\Ben\\Documents\\Stony Brook\\1st Year\\R\\sa_ecosystems\\sa_ecosys_450m_dd84.img")
eco.crop<-crop(ecosystem,Amazon)
eco.mask<-mask(eco.crop,Amazon)
eco.box<-PolyRasBox(km50,km150,Lowland, eco.crop)

lapply(eco.box,barplot)
sapply(eco.box,table)





