#I downloaded the supplementary data from La Sorte, F. & Jetz, W. (2010). Projected range contractions of montane biodiversity under global warming.
#This study looked at potential elevation range shifts of species tracking their current thermal niche. 
#Change the filepath to where ever you downloaded it. Tip, you can just drag and drop into the R window, and then change the function.
d<-read.csv("C:/Users/Ben/Documents/R/Jetz.csv", sep=",", header=TRUE)
d#What does the entire file look like?
d
#That's annoying, i just want to see the structure of it
head(d)
#How many birds did Jetz look at?
nrow(d)
#How many variables for each species?
length(d)
#Back to R - what kind of structure are we looking at?
class(d)
#Okay its a dataframe, that means it has specific rows and columns
#What are the column names?
names(d)
#I just want a unique species list. To specify a column use the format data.frame$columnname
# d is our data.frame, $, Species is the column name
range<-d$Rangesize
#Get basic statistics, what's the average range size?
mean(range)
#Okay but this has been all pretty uninteresting. Let's look at the mean range size, split by realm?
a<-subset(d,d$Realm=="Neotropics")
mean(a)
mean.all<-tapply(range,factor(d$Realm),mean)
#What if our data is categorical?
table(d$IUCN)
table(d$IUCN,d$Family)
#Wow that was alot, let's break it down
#We have arrived at the wonderful world of functions, preset code that performs functions
#Let's say i told you to use the tapply function, you would have no idea what to do with that piece of jargon.
#Enter the help screen, the single best thing about R
?barplot
?table
#Let's talk about this for a while
#Visualize the data
barplot(tapply(range,factor(d$Realm),mean))
#but thats really ugly command, and hard to look at, remember we can save everything as an object. EVERYTHING should be save as an object.
#now we can plot the output directly
barplot(mean.all)
#But we are learning statistics. Means are only worth so much. Enter the boxplot
boxplot(d$Rangesize~d$Realm)
#is range and realm statistically significant? Quick, Aov!
?aov
range.stat<-aov(Rangesize~Realm, data=d)
#Adjust p values for multiple t tests
?TukeyHSD
Tukey.stat<-TukeyHSD(range.stat)
Tukey.stat
#I just want the p values below .05
pvalue<-sort(as.matrix(Tukey.stat[[1]])[,4])
round(pvalue,2)
barplot(pvalue,cex.names=.35)
abline(h=.05,col=4,lty=2)
#Last thing, what if i wanted to do this entire thing over with IUCN...., wouldn't that suck? typing in all those commands?
#Everyone take 10min and try to ask some small question involving this dataset, this is the time to shine. If you choose the boring route. I made a worksheet.
sample(d$Species,5)

