plot(1:20, dpois(1:20, 2), pch=16, col="red")
points(1:20, dpois(1:20, 5), pch=16, col="blue")
points(1:20, dpois(1:20, 0.5), pch=16, col="green")
points(1:20, dpois(1:20, 0.1), pch=16, col="orange")
points(1:20, dpois(1:20, 20), pch=16, col="purple")
dev.off()
curve(dchisq(x, df=10), from=0, to=30, add=T)
curve(dchisq(x, df=10), from=0, to=30)
ceiling(qnorm(1-0.2005, mean=78, sd=sqrt(53.29)))
1 - pnorm(0.8398366)
qpcr = read.table("Lab9data/qpcr_data.csv", header=T, sep=",")
diff = qpcr$Control - qpcr$Gene.A
wild_diff = diff[1:4]
m_diff = diff[5:8]
t.test(wild_diff, mu=mean(m_diff))
b_diff = qpcr$Control - qpcr$Gene.B
b_wild_diff = b_diff[1:4]
b_m_diff = b_diff[5:8]
t.test(b_wild_diff, mu=mean(b_m_diff))
survey = read.table("lab1.surveydata.txt", header=T, as.is=T)
result = t.test(survey$height_inches[which(survey$sex=="F")], mu=64)
print(result$statistic)
print(result$p.value)
incomes = read.table("Lab9data/paygap.txt", header = T, sep="\t", as.is=T)
summary(lm(FtoMRatio~Prop_Obama, data=incomes))
cor(incomes$Prop_Obama, incomes$FtoMRatio)^2
xbar = mean(incomes$FtoMRatio)
ybar = mean(incomes$Prop_Obama)
xvars = incomes$FtoMRatio - xbar
yvars = incomes$Prop_Obama - ybar
r = sum(xvars * yvars)/sqrt(sum(xvars^2)*sum(yvars^2))
time = read.table("Lab9data/timedata.txt", header=T, sep="\t", as.is=T)
head(time)