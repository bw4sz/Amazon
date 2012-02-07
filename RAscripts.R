#Method Constraint for Phylogenetic Beta Diversity

geotree<-read.nexus("C:\\Users\\Ben\\Documents\\GIS\\Geospiza.nex")
collapsedgeotree <- di2multi(g, 0.03)

plot(geotree)
plot(collapsedgeotree)
collapse.range(a,b,c)<-function(a)
x<-seq(a,b,c)
for (i in 1:length(x))
collapsedgeotree <- di2multi(g, x[i])
return

