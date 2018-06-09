for (i in 1:10) {
  print(paste("i=",i))
  print(paste("i+1=",i+1))
}
getwd()
setwd("Desktop/MiriamProgramming/r/r_practice")
surveydata = read.table("lab1.surveydata.txt", header=T, as.is=T)
for (i in c(5:10, 20)) {
  print(surveydata[i,])
}
length(which(surveydata$mothers_height_inches!="NA"))
for (col in 1:2) {
  print(mean(surveydata[,col][which(surveydata[,col]!="NA")]))
}
myranges = function(dataframe) {
  for (i in 1:dim(dataframe)[2]) {
    print(names(dataframe)[i])
    numbers = which(dataframe[,i]!="NA")
    a = max(dataframe[,i][numbers])
    b = min(dataframe[,i][numbers])
    print(a - b)
  }
  rm(numbers)
  rm(a)
  rm(b)
}
myranges(surveydata)
myaverage = function(v) {
  s = sum(v)
  return(s / length(v))
}
myaverage(surveydata$fathers_height_inches[which(surveydata$fathers_height_inches!="NA")])
myaverage(surveydata$siblings)
avgheight = myaverage(surveydata$height_inches)
colmin = function(dataframe, col) {
  return(min(dataframe[,col]))
}
colmin(surveydata, 1)
hist(surveydata$height_inches, col="red", xlim=c(48, 99), main="heights in inches", xlab="inches")
values = boxplot(surveydata$height_inches[which(surveydata$sex=="F")], surveydata$height_inches[which(surveydata$sex=="M")], names=c("F", "M"), ylab="heights")
medians = values$stats[3,]
par(mfrow=c(1,2))
dev.off()
plot(surveydata$height_inches, surveydata$shoe_size_european, xlab="height in inches", ylab="shoe size", main="height vs shoe size", type="n")
points(surveydata$height_inches[which(surveydata$sex=='M')], surveydata$shoe_size_european[which(surveydata$sex=="M")], pch="m")
points(surveydata$height_inches[which(surveydata$sex=="F")], surveydata$shoe_size_european[which(surveydata$sex=="F")], pch="f")