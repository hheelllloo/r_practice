data = read.table("Lab10_data/Biometry9.5.txt")
rm(list=ls())
head(data)
levels(data[,2])
summary(data)
anova(lm(data[,1]~as.factor(data[,2])))