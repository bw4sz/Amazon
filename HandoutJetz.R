#1.What montane bird Families will be most/least impacted by climate change? (2 point)
a<-aggregate(d$RangeChange,by=list(d$Family),mean)
which.max(a[,2])
a[4,]
#4 Alaudidae 0.55
which.min(a[,2])
a[37,]
#37 Otididae -0.86

2.	Are species with smaller ranges more susceptible to range loss (Hint: correlation)? (2 points)
cor(d$Rangesize,d$RangeChange)
#0.06439793

3. How many species of montane hummingbird are there (Hint: Hummingbird: Trochilidae) (1 point if you look it up, 2 if you can fully code it)
f<-table(d$Family)
f["Trochilidae"]

4. What is the mean rangeChange for each IUCN category? (1 point)
tapply(d$RangeChange, factor(d$IUCN),mean)
   CR         EN         LC         NT         VU 
-0.4160000 -0.4937838 -0.2539713 -0.2691935 -0.2817391 


5. Which IUCN categories have a significantly higher RangeChange? (3 points)
range.stat<-aov(RangeChange~IUCN, data=d)
#Adjust p values for multiple t tests
?TukeyHSD
Tukey.stat<-TukeyHSD(range.stat)
Tukey.stat

#6. Anything else, of interest, scripted fully (4 points)


