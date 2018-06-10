#for question 8 - change portions that begin with ##
#this is reproduced on page 4 of your lab packet

myCI = function(observations, parameter, confidencelevel) {
	if(confidencelevel <= 0 | confidencelevel >= 1) {
print(paste("Cannot calculate a", confidencelevel*100, "% confidence interval."))
		return()
	}
	alpha = 1-confidencelevel
	
	u = mean(observations)
	s = sd(observations)
	
	if(parameter=="mu") {
		tval = qt(1 - alpha/2, length(observations) - 1)
		lowerlimit = u - tval*s/sqrt(length(observations))
		upperlimit = u + tval*s/sqrt(length(observations))
	}
	else if(parameter=="sigma") {
		lowerlimit = (n-1)*s^2/qchisq(1-alpha/2, n-1)
		upperlimit = (n-1)*s^2/qchisq(alpha/2, n-1)
	}
	else {
		print(paste("I cannot make a confidence interval for", parameter, "."))
		return()
	}
	myinterval = c(lowerlimit, upperlimit)
	#print(paste("The", confidencelevel*100, "% confidence interval for", parameter, "is:"))
	return(myinterval)	
}

myCI(flash, "mu", 5)
myCI(flash, "brownbear", 0.95)
myCI(flash, "mu", 0.95)
myCI(flash, "sigma", 0.99)
data = read.table("lab1.surveydata.txt", header=T, sep="\t")
mean(data$height_inches[which(data$sex=="M")])
sd(data$height_inches)
a = myCI(data$height_inches, "mu", 0.99)
b = myCI(data$height_inches[which(data$sex=="M")], "mu", 0.99)
c = myCI(data$height_inches[which(data$sex=="F")], "mu", 0.99)
avgs = c(mean(data$height_inches), mean(data$height_inches[which(data$sex=="M")]), mean(data$height_inches[which(data$sex=="F")]))
bp = barplot(avgs, ylim=c(0, 80), names.arg = c("All", "Males", "Females"))
title(xlab="categories", ylab=expression(bar(x) %+-% s), main="heights")
segments(bp[1,1], a[1], bp[1,1], a[2])
t.test(data$height_inches[which(data$sex=="M")], mu=mean(data$height_inches))



#for question 25 - change lines below beginning with ##
#this question is optional.

################################################################################
# Function : cont_ints                                                         #
# This function generates 100 confidence intervals for a specified alpha from  #
# random samples from either a Poisson, Binomial, or Normal distribution (the #
# user specifies the distribution IN QUOTES). Each confidence interval is 	   #
# plotted as a vertical line.												   #                                                               
# !Caution! If your sample size is very then you may 						   #
#           get an error saying "data are essentially constant" and the 	   #
#           simulation will stop.    										   #
#  We'll use lists in this function, a data type we haven't used before.       #                                   
################################################################################

confinterval_simulation <- function(distrib, alpha){
	maxReps <- 100; #change this if you want more samples taken
	#the random samples generated will be stored in this list, called samples
	samples = list()
	#the array confidence_intervals stores the confidence intervals generated for each sample so they can be plotted at the end of the simulation
	confidence_intervals <- array(0.0,c(2,maxReps));
	#grepl tests for a particular pattern in an input.
	if(grepl("norm", distrib, ignore.case=TRUE)==TRUE) {
		mu = as.numeric(readline("Enter population mean, mu: "));
		sigma = as.numeric(readline("Enter population standard deviation, sigma: "));
		n = as.numeric(readline("Enter sample size, n: "));
		#ymax, ymin, and expected are to help in changing the size of the window based on the generated confidence intervals. 
		ymax = mu
		ymin = mu
		expected = mu
		for(i in 1:maxReps) {
			samples[[i]] = ##generate n random numbers from a normal distribution with mean mu and standard deviation sigma, which the user entered earlier
		}
	}
	else if(grepl("pois", distrib, ignore.case=TRUE)==TRUE){
		l = as.numeric(readline("Enter the expected value, lambda: "));
		n = as.numeric(readline("Enter sample size, n: "));
		ymax = l
		ymin = l
		expected = l
		for(i in 1:maxReps) {
			samples[[i]] = ##generate n random numbers from a poisson distribution with rate lambda, which the user entered earlier as l
		}
	}
	else if(grepl("binom", distrib, ignore.case=TRUE)==TRUE){
		p = as.numeric(readline("Enter the probability of success, p: "));
		if(p>1) {
			print("p must be a probability!")
			return(-9)
		}
		n = as.numeric(readline("Enter sample size, n: "));
		ymax = p
		ymin = p
		expected = n*p
		for(i in 1:maxReps) {
			samples[[i]] = ##generate n BERNOULLI TRIALS (set size=n in rbinom()) with probability of success p, which the user entered earlier
		}
	}
	else {
		print("Unrecognized distribution.")
		return(-9)
	}

		
	for(i in 1:maxReps) {
		n = length(samples[[1]])
		tval = ##calculate the POSITIVE critical value from the t distribution that has alpha/2 area to its RIGHT
		upper = ##the upper limit of the (1-alpha)% confidence interval for the mean using samples[[i]]; your command must include mean(samples[[i]]), length(samples[[i]]), and sd(samples[[i]])
		lower = ##the lower limit of the (1-alpha)% confidence interval for the mean using samples[[i]]; your command must include mean(samples[[i]]), length(samples[[i]]), and sd(samples[[i]])
		
		confidence_intervals[,i] = c(upper, lower)
		#This adjusts the ymax and ymin basedon the largest values of the confidence interval and checks that the sum of all observations is not zero for the discrete random variable simulations.
		if (!is.nan(upper)) {
			if(upper > ymax){
				ymax <- upper
			}
			if(lower < ymin){
				ymin <- lower
			}
		}
	}

	##NO MORE CHANGES ARE NECESSARY BELOW; Confidence intervals that do not cross the true mean (determined by user-specified values) are given in red	
	#This makes an empty plot with the dimensions relative to the difference between the expected value and the maximum/minimum value of the all the confidence intervals.
	plot(x=1,y=1, xlim=c(0,maxReps), ylim=c(expected - (expected-ymin)*1.25, expected + (ymax-expected)*1.25), type="n", xlab="Sample number", ylab="Value")
	abline(h=expected)
	#This plots each confidence interval as a line, skipping the ones which never showed up.
	badInt = 0
	for(i in 1:maxReps){
		if(!is.nan(confidence_intervals[1,i])){
			if(confidence_intervals[1,i] < expected | confidence_intervals[2,i] > expected){
				lines(x=c(i,i), y=confidence_intervals[,i], type="l", col="red")
				print(paste("Confidence interval number", i, "does not capture the population mu"))
				badInt = badInt + 1
			}
			else {
				lines(x=c(i,i), y=confidence_intervals[,i], type="l", col="black")
			}
		}
	}
	print(paste("Total count of bad intervals: ", badInt))
}