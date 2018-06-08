rm(list=ls())
mytab = read.table("latlong.txt", header=T, as.is=T)
mytab[1,]
mytab[,dim(mytab)[2]]
mynewtab = mytab[,-3]
plot(mynewtab$long, sin(mynewtab$lat*pi/180), pch=16)
myindices = which(mytab$continent.region=="Africa")
mylongs = mytab$long[myindices]
mylats = mytab$lat[myindices]
plot(mylongs, sin(mylats*pi/180), pch=16)
plot(mytab$long[which(mytab$continent.region=="Africa" | mytab$continent.region=="Europe")], sin(mytab$lat[which(mytab$continent.region=="Africa" | mytab$continent.region=="Europe")]*pi/180), pch=16)
a = c(1, 1, 0)
length(which(a==0))
rm(a)
length(which(mytab$long>=0 & mytab$long<=100))
weights = c(100, 115, 206, 225, 130, 130, 132, 141, 242, 123, 275, 178, 180, 181, 175)
weights_kg = weights / 2.2
heights = c(60, 61, 72, 70, 60, 65, 65, 65, 67, 70, 74, 68, 73, 73, 73)
sexes = c("F", "F", "M", "M", "F", "F", "F", "M", "M", "F", "M", "F", "M", "M", "M")
heights_m = heights / 39.37
min(heights_m)
bmis = weights_kg / (heights_m^2)
bmis[3]
mean(bmis)
sum(bmis) / length(bmis)
bmis[c(1, 4, 12)]
which(bmis > 18.5 & bmis < 24.9)
which(weights==130)
which(sexes=="M" & (bmis<20 | bmis>30))
#My first comment
ls()
mydf = data.frame(weights, heights, sexes)
mydf$heights[4]
mydf[4,2]
mydf[c(1, 3, 10),]
mydf[7:11,]
surveydata = read.table("lab1.surveydata.txt", header=T, as.is=T, sep="\t")
head(surveydata)
length(which(surveydata$sex=="M"))
2 * 3