###########################################################################################################
###   R Stats recap 1 for Movement Ecology workshop - UNESP Rio Claro 19 March 2018			  #
###   Luca Borger											  #
###   R script 1											  #
###########################################################################################################
### Learning aims:											  #
###													  #
### 	1. Linear regression recap & permutation-based regression					  #
###													  #
### 	2. Relax distributional assumptions - GLMs							  #
###												          #
###	3. Relax linearity assumption - GAMs 								  #                                                                                          
###													  #
###########################################################################################################


# load the IsleRoyaleData into R/ R Studio 

  IsleRoyaleData <- read.csv("isleRoyale.csv")



###########################################################################################################
# 4. Regression models (estimate regression parameters, obtain test statistics)
#
# The aim is to estimate the Intercept and Slope, which define the regression line: y = a + bx
#
# The intercept is the value of the response variable (y) when the explanatory variable (x) is equal to zero
#
# The slope tells you how fast the response variable changes with a change in the explanatory variable; is the slope is positive, the response variable increases with an increase in the explanatory variable and viceversa.
# 
# And what if the slope is equal to zero? --> you get a horizontal line
#
# the key R comand is lm(), which stands for 'linear model'
#
# as usual, we create a new object, e.g. called 'm1' (model 1), where we store the results of the regression analysis, then we use the 'summary()' comand to obtain the parameter estimates and regression statistics
#
# for convenience, we define a new variable, the Moose per Wolf ratio, which we then use as explanatory variable in the regression model
  

# add the kill rate data - note the use of '$' to create a new column in the isle royale dataframe:
  
  IsleRoyaleData$KillRate <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0.614804965,0.818668047,0.760416667,0.599037917,0.645426829,0.56267618,0.298202614,0.506944444,0.387442741,0.329943503,0.217261905,0.869047619,0.393821931,
  	0.440241228,0.456630525,0.67019774,0.549189815,0.864109848,0.809652278,0.856874189,1.028909465,1.428406998,0.877403846,0.792100694,1.391198891,1.274167562,0.637665968,0.802197802,0.631203008,0.474480022,
  	0.876670844,1.1385918,0.727671451,0.834312957,0.724206349,0.506944444,0.695238095,0.732931727,0.63875,0.438,0.667933559)

  
  IsleRoyaleData$MooseWolfRatio <- with(IsleRoyaleData, Moose/Wolf)
  
  m1 <- lm(KillRate ~ MooseWolfRatio, data = IsleRoyaleData)
  
  # note how this comand is essentially equal to the plot() comand
  
  plot(KillRate ~ MooseWolfRatio, data = IsleRoyaleData)

# we can now use the abline() comand of last week to add the estimated regression line to our scatter plot
  
  abline(m1)
  
  # this function plots a regression line and is called 'abline' as it asks for the intercept (a) and slope (b) of the linear equation: y = a + bx
  # let us change the x and y limits, to visualize this better, and annotate the plot properly, and add again the regression line
  
  plot(KillRate ~ MooseWolfRatio, data = IsleRoyaleData, xlim = c(0,150), ylim = c(0,1.5), xlab = "Moose per wolf", ylab= "Kill rate", main = "How much food does each wolf get?")
  abline(m1)
  
# let us inspect now what the regression model has estimated; to do this you use the summary() comand, in the same way you used it before to summarize the data in your dataframes
  
  summary(m1)
  
# inspect the output:
#
# Call: 		--> this tells you the formula you have used for your model
#
# Residuals: 		--> gives you the distribution of residuals (do not worry about these for now)
#
# Coefficients:		--> this is the most important part! This is the table of regression coefficients ('Estimate'), their standard errors, and the t-value and p-value (Pr(>|t|)) 
# 			--> of the t-tests on the regression parameters --> Null hypothesis is that the value is equal to zero
#
# Residual standard error: --> the standard error of the residuals (the unexplained variance); here, note especially the degrees of freedom, as well as the number of missing values 
# 			--> to understand the degrees of freedom: it is the number of records minus the number of estimated parameters, here two (intercept and slope).
#			--> thus check:
  
  nrow(IsleRoyaleData)
  
  # missing data = 12
  # N estimated parameters = 2
  # Degrees of freedome should hence be equal to:
  
  53-12-2
  
  # compare to what summary(m1) tells you
#
#
# Multiple R-squared:  0.5802,    Adjusted R-squared:  0.5694 --> tells you the amount of variance explained by the model (here, simply the effect of the Moose/Wolf ratio) - compare to the cor()^2 value!
# do not worry for now what 'adjusted R sqare' is ...
#
# F-statistic:  additional statistic, do not worry for now

# let us investigate better the structure of what summary(m1) gives us

  str(summary(m1))

# cool - as  you can see this indicates you can access all single components by using the $
# for example, the most important part, the coefficients table
  
  summary(m1)$coefficients
  
# and again, we can investigate the structure of the coefficients table
  
  str(summary(m1)$coefficients)
  
# so, we can directly obtain regression slopes or p-values, etc., by writing:

  summary(m1)$coefficients[[8]]

# what value is this? compare to summary(m1)$coefficients[[1]] & summary(m1)$coefficients[[4]]

# now, you might want to add the estimated regression equation to the plot, or the p-value. How would you do this? See above and last week regarding how to add text to figures.


###########################################################################################################
# 5. Permutation test for regression (involves recap of for-loop from practical 1)

# we first create a subset of our dataframe, excluding all NA values --> we do this using the na.omit() function
  
  subIsleRoyaleData <- na.omit(IsleRoyaleData)
  
  # compare
  
  IsleRoyaleData
  subIsleRoyaleData

# then we use the extremely handy sample() function, to randomly reshuffle the MooseWolfRatio vector - repeat this line several times and compare the values
  
  sample(subIsleRoyaleData$MooseWolfRatio)
  sample(subIsleRoyaleData$MooseWolfRatio)
  sample(subIsleRoyaleData$MooseWolfRatio)
  
  
# now we refit the regression model 1000 times, but instead of using the MooseWolfRatio as explanatory variable, we randomly reshuffle the latter, then count how many times we obtain a significant p-value by chance  
  
  permPvals <- numeric(1000)
  
  for(i in 1:1000) {
  	
  	x <- sample(subIsleRoyaleData$MooseWolfRatio)
  	foo <- lm(subIsleRoyaleData$KillRate ~ x)
  	permPvals[i] <- summary(foo)$coefficients[[8]]
  }
  
  
# compare the distribution of permutated p-values to the one we had obtained:

  hist(permPvals)
  abline(v=summary(m1)$coefficients[[8]],col=2,lwd=4)

# in fact no p-value equal to or less than the one we obtained has been obtained by chance

  length(which(permPvals <= summary(m1)$coefficients[[8]]))/1000

# let us check:

  min(permPvals)
  summary(m1)$coefficients[[8]]
  # in fact, way lower
  
# it is also interesting to calculate the proportion of p-values below the 'traditional' significance level
  
  length(which(permPvals<0.05))/1000
  # [1] 0.052
  # which suggest that for this relationship a p-value < 0.05 is adequate
  


# such a permuation test can be very useful also for the regression estimates itself, such as the slope. Think about how yu could easily modify the above loop function to get a permutated sloep estimate.
# it is a very powerful way to test the significance of the parameter estimates and does not depend on distributional assumptions.


#############################################################################################################################################################
# Will the Turtle Dove be extinct within the next 10 years in the UK?

  
# create dataframe from count data  (actually, proportional change from baseline year)
  
  BBSindexTD <- (c(85.188950, 94.781975, 86.061043, 74.505808, 77.340111, 69.055226, 61.860457, 57.063944, 51.831385, 48.343012, 46.598826, 46.816849, 34.171498,
    			28.938938, 23.706379, 25.014519, 19.127890, 12.805214, 10.406958,  9.534865, 6.482538))/91.511625*100

  
  DoveData <- data.frame(Year = 1994:2014, BBSindex = BBSindexTD)
  
  dim(DoveData); str(DoveData); summary(DoveData)

  plot(BBSindex ~ Year, data = DoveData, ylim=c(0,110), xlim=c(1994,2025))
  abline(h=0)

  with(DoveData, cor(BBSindex, Year, method = "spearman"))
  with(DoveData, cor.test(BBSindex, Year, method = "spearman"))
  with(DoveData, cor(BBSindex, Year, method = "spearman"))^2
  
  Dm1 <- lm(BBSindex ~ Year, data = DoveData) #note, you can copy simply the comand from the plot function a few lines above!
  
  summary(Dm1)
  
# take the estimated slope and use it to predict the mean poopulation index for 2014 and 2015  
# remember the equation: y = a + bx
# 
  
  9818.8795-4.8745*2014
  
  9818.8795-4.8745*2015 # ouch ... imminent trouble ahead? --> take the uncertainty of the slope estimate into account (standard error, s.e.)
  
  9818.8795-(4.8745-0.1538)*2015:2025
  
# let us get some permutated intercept and slope estimates
  
  
   permIntvals <- numeric(1000)
   permSlpvals <- numeric(1000)
     
    for(i in 1:1000) {
    	
    	x <- sample(DoveData$Year)
    	foo <- lm(DoveData$BBSindex ~ x)
    	permIntvals[i] <- summary(foo)$coefficients[[1]]
    	permSlpvals[i] <- summary(foo)$coefficients[[2]]
    	
    }

# now let us compare the permutated and the estimated slope estimates

  hist(permSlpvals, xlim = c(-5,5))
  abline(v = summary(Dm1)$coefficients[[2]], col = 2, lwd = 2)
  
# what can you conclude?


  
  
#############################################################################################################################################################


# for today, load all these datasets:

  IsleRoyaleData <- read.csv("isleRoyale.csv")
  
  StorkData <- read.csv("StorkData.csv")
  
  tanninData <- read.table("tannin.txt", header = TRUE)	# Note the use of 'read.table' --> it is in fact the basic R function to read in tab-separated text files
  
  ozone.pollution <- read.table("ozone.data.txt", header=TRUE)

  GannetDats <- read.csv("GannetConditionData.csv")

  
# and these we directly generate within R
  
  BBSindexTD <- (c(85.188950, 94.781975, 86.061043, 74.505808, 77.340111, 69.055226, 61.860457, 57.063944, 51.831385, 48.343012, 46.598826, 46.816849, 34.171498,
      			28.938938, 23.706379, 25.014519, 19.127890, 12.805214, 10.406958,  9.534865, 6.482538))/91.511625*100
  
    
  DoveData <- data.frame(Year = 1994:2014, BBSindex = BBSindexTD)
  

# 8 datasets! to check if you have loaded all of them into your workspace use the ls() command ('list' --> it lists all objects in your R workspace)

  ls()


#####################################################################################################################################################################
# 1. Hypothesis: Wolf kill rate should be greatest during years when the ratio of moose per wolf in the population is greatest.


# add the kill rate data - note the use of '$' to create a new column in the isle royale dataframe:
  
  IsleRoyaleData$KillRate <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,0.614804965,0.818668047,0.760416667,0.599037917,0.645426829,0.56267618,0.298202614,0.506944444,0.387442741,0.329943503,0.217261905,0.869047619,0.393821931,
  	0.440241228,0.456630525,0.67019774,0.549189815,0.864109848,0.809652278,0.856874189,1.028909465,1.428406998,0.877403846,0.792100694,1.391198891,1.274167562,0.637665968,0.802197802,0.631203008,0.474480022,
  	0.876670844,1.1385918,0.727671451,0.834312957,0.724206349,0.506944444,0.695238095,0.732931727,0.63875,0.438,0.667933559)


# We start, as usual, by plotting our data.  
# for convenience, we generate and add the MooseWolf ratio column
  
  IsleRoyaleData$MooseWolfRatio <- with(IsleRoyaleData, Moose/Wolf)
  
  plot(KillRate ~ MooseWolfRatio, data = IsleRoyaleData, xlim = c(0,150), ylim = c(0,1.5), xlab = "moose per wolf", ylab= "Kill rate", main = "How much food does each wolf get?")
  

# tot test our hypothesis we fit a regression model

  m1 <- lm(KillRate ~ MooseWolfRatio, data = IsleRoyaleData) 	    # note how this comand is essentially equal to the plot() comand
 

# and with the summary() comand we obtain the estimated model parameters and test statistics
   
   summary(m1)
 
 
# inspect the output:
#
# Call: 		--> this tells you the formula you have used for your model
#
# Residuals: 		--> gives you the distribution of residuals (do not worry about these for now)
#
# Coefficients:		--> this is the most important part! This is the table of regression coefficients ('Estimate'), their standard errors, and the t-value and p-value (Pr(>|t|)) 
# 			--> of the t-tests on the regression parameters --> Null hypothesis is that the value is equal to zero
#
# Residual standard error: --> the standard error of the residuals (the unexplained variance); here, note especially the degrees of freedom, as well as the number of missing values 
# 			--> to understand the degrees of freedom: it is the number of records minus the number of estimated parameters, here two (intercept and slope).
#			--> thus check:
  
  nrow(IsleRoyaleData)
  
  # missing data = 12
  # N estimated parameters = 2
  # Degrees of freedome should hence be equal to:
  
  53-12-2
  
  # compare to what summary(m1) tells you
#
#
# Multiple R-squared:  0.5802,    Adjusted R-squared:  0.5694 --> tells you the amount of variance explained by the model (here, simply the effect of the Moose/Wolf ratio) - compare to the cor()^2 value!
# do not worry for now what 'adjusted R sqare' is ...
#
# F-statistic:  additional statistic, do not worry for now

# what do you conclude regarding our hypothesis?

###################################
# add regression line to the plot 
 
# Method A:   lines()
 
# what is the range of values of the explanatory variable?
  
  summary(IsleRoyaleData$MooseWolfRatio)
  
  # generate a vector covering the full range
  
  xvals <- seq(from = 15, to = 141.4, length = 100)
  
  # add estimated regression line - remember: y ~ a + bx
  
  lines(xvals, summary(m1)$coefficients[1] + summary(m1)$coefficients[2] * xvals, lty = 3, lwd = 2)	# remember what lty and lwd do!
 
# Method B: abline() 
    
    abline(m1, col = 2, lwd=2)
  

##############################################################################################################################################
# 1.2 Will the Turtle Dove be extinct within the next 10 years in the UK?

  plot(BBSindex ~ Year, data = DoveData, ylim=c(0,110), xlim=c(1994,2025))
  abline(h=0)	# why do I add this line?
    
  Dm1 <- lm(BBSindex ~ Year, data = DoveData) #note, you can copy simply the comand from the plot function a few lines above!
  
  summary(Dm1)
  
  predFuture <- data.frame(Year = 1994:2025)
  
  predDoves <- predict(Dm1, predFuture, interval="confidence")
  
  lines(1994:2025, predDoves[,1], lty=1)
  lines(1994:2025, predDoves[,2], lty=2)
  lines(1994:2025, predDoves[,3], lty=2)

# indeed a bleak prospect ...
# understand what the predict() function does -- it takes the values of the explanatory variable at which we want to obtain predicted values, then takes the estimated model and extracts the
# estimated parameters (intercept and slope) and standard errors and generates the model predictions

# inspect the object:
  
  predDoves


##############################################################################################################################################
# 1.3 Does the number of storks predict human birth rate?
  
  plot(BirthRate.KperYr ~ StorkPairs, data = StorkData, xlab = "Number of Stork Pairs", ylab = "Human birth rate per year")
  
  m1Stork <- lm(BirthRate.KperYr ~ StorkPairs, data = StorkData)

  summary(m1Stork)
  abline(m1Stork)
  
# try to use the predict() function as above to add confidence intervals

# think about spurious correlation and exogenous drivers



##############################################################################################################################################
# 1.4 Gannet data -- is body condition related to conspecific density and the density of marine mammals?
  
  modGannet1 <- lm(Condition.DI ~ Density.gannets, GannetDats)
  summary(modGannet1)
  
  modGannet2 <- lm(Condition.DI ~ Density.gannets + Mammals.marine, GannetDats)
  summary(modGannet2)
  
# How good are my models? Compare R2:

  summary(modGannet1)$r.squared
  
  summary(modGannet2)$r.squared

# remember, R2 tells us the proportion of variance in the response variable explained by the explanatory variable. It is a number ranging from 0 (no correlation) to 1 (perfect fit)

# compare model 1 and 2 -- we have a case of 'additive model' -- the two explanatory variables add to the variance of the response variable explained



##############################################################################################################################################
# 2. Summary tables, model parameters, model predictions - confidence & prediction intervals
# 	gannet body condition data as example

  
  md <- 200       # maximum density - set this value, as an example
  
  dens <- seq(0, md, 1)
  
  datNew <- data.frame(Density.gannets = dens)
  
  preds <- predict(modGannet1, datNew)
  
  plot(GannetDats$Density.gannets, GannetDats$Condition.DI, main = "a", xlab="P", ylab=expression(paste(Delta,"I")), ylim=c(-100,70), xlim=c(0,md))
  
  lines(dens, preds)

# now compare confidence and prediction intervals
  
  par(mfrow=c(1,2))
  
  preds <- predict(modGannet1, datNew, interval="confidence")
  
  plot(GannetDats$Density.gannets, GannetDats$Condition.DI, main="a", xlab="P", ylab=expression(paste(Delta,"I")), ylim=c(-100,70), xlim=c(0,md))
  
  lines(dens, preds[,1], lty=1)
  lines(dens, preds[,2], lty=2)
  lines(dens, preds[,3], lty=2)
  abline(h=0)
  
  preds<-predict(modGannet1, datNew, interval="prediction")
  
  plot(GannetDats$Density.gannets, GannetDats$Condition.DI, main="b", xlab="P", ylab=expression(paste(Delta,"I")), ylim=c(-100,70), xlim=c(0,md))
  
  lines(dens, preds[,1], lty=1)
  lines(dens, preds[,2], lty=2)
  lines(dens, preds[,3], lty=2)
  abline(h=0)
  
  par(mfrow=c(1,1))



##############################################################################################################################################
# 3. How good is my model: R2 vs. adjusted R2 -- tannins and caterpillar growth
  
  tanninData <- read.table("tannin.txt", header = TRUE)
  
  plot(growth ~ tannin, data = tanninData)
  
  lm1 <- lm(growth ~ tannin, data = tanninData)
  abline(lm1)

# add spurious, random covariates

  x2 <- rnorm(nrow(tanninData))
  x3 <- rnorm(nrow(tanninData))
  x4 <- rnorm(nrow(tanninData))

# see what happens to the R2 value when we add these to the model

  lm2 <- lm(growth ~ tannin + x2, data = tanninData)
  lm3 <- lm(growth ~ tannin + x2 + x3, data = tanninData)
  lm4 <- lm(growth ~ tannin + x2 + x3 + x4, data = tanninData)
  
  par(mfrow=c(1,2))
  plot(1:4,c(summary(lm1)[[8]],summary(lm2)[[8]],summary(lm3)[[8]],summary(lm4)[[8]]),type="b",xlab="N terms", ylab="R2",ylim=c(0.7,1), main="R2")
  
  plot(1:4,c(summary(lm1)[[9]],summary(lm2)[[9]],summary(lm3)[[9]],summary(lm4)[[9]]),type="b",xlab="N terms", ylab="R2",ylim=c(0.7,1), main="adj R2")



#######################################################################################################################################################
# 4. How good is my model? Regression diagnostics (residuals) -- tannin data model as example
  
#  Normality of residuals (‘errors’)
  
#  Constancy of variance (of the residuals)
  
#  ? Residual plots!

  
  
  par(mfrow=c(2,2)); plot(lm1); par(mfrow=c(1,1))


# influential points (Cooks diagnostics)
  
 # point 9 = highest leverage
 # point 7 = quite influential (closest to Cook’s distance contour
  
  tanninData[7,]
  lm1b <- update(lm1,subset=(tannin != 6))
  summary(lm1b)
  
  # 9% change in slope, 12% change in s.e.





#######################################################################################################################################################
# 5. Variable transformation to meet model assumptions - boxcox method & power transformations
#		Ozone pollution data as an example


  ozone.pollution<-read.table("ozone.data.txt",header=TRUE)
  
  model1 <- lm(ozone ~ temp + wind + rad, data= ozone.pollution)
  
  plot(model1); summary(model1)	# need to hit 'Enter' to progress among the residual plots
  
  model2 <-lm(log(ozone) ~ temp + wind + rad, data= ozone.pollution)
  plot(model2); summary(model2)

# Finding the best power transformation -- boxcox() is your best friend!

  
  library(MASS)
  # load a library for additional functions, somewhat akin to an additional ‘app’
  boxcox(ozone ~ temp + wind + rad, data= ozone.pollution)
  boxcox(ozone ~ temp + wind + rad, data= ozone.pollution, lambda=seq(0,0.5,0.01))	# to zoom in to the value
  
  # if lambda = 0 --> log-transform, otherwise use value as power exponent -- hence if =1, you do not need to do anything
  boxcox(ozone^0.2 ~ temp + wind + rad, data= ozone.pollution)


# compare:

  ozMod1 <- lm(ozone ~ temp + wind + rad, data= ozone.pollution)
  
  ozMod2 <- lm(ozone^0.2 ~ temp + wind + rad, data= ozone.pollution)

  par(mfrow = c(2,4))

  plot(ozMod1); plot(ozMod2)

  par(mfrow = c(2,4))
  


#########################################################################################################################################################
#####################################################################################################################################################################
# 1. Hypothesis tests for factorial covariates
  
# 2 factor levels (e.g. 'male vs. female') -- t.test(y ~ x) for the parametric test, 

# we know already the t-test - here a refresher using the classical student-sleep dataset:

  t.test(extra ~ group, data = sleep)
  
# it makes the normality assumption - if this can not be met (e.g. proportion data), you can use the Wilcoxon rank sum test:  wilcox.test(y ~ x)
# for example, using the airquality dataset, let  us assume we want to test for a difference between 2 specific months
  
  boxplot(Ozone ~ Month, data = airquality)
  wilcox.test(Ozone ~ Month, data = airquality, subset = Month %in% c(5, 8))
  
  # remember the subset comand and the use of %in% from previous practicals!  

# more than 2 levels? In the parametric case, ANOVA! Which you simply use as for regression: lm(y ~ x)
# The non-parametric test is called the Kruskal-Wallis rank sum test.
# it is easy to use with the formula interface: kruskal.test(y ~ x)
# let us use the airquality dataset that comes with R to test the hypothesis that Ozone concentration differs between the months
# null hypothesis is hence that there is no significant difference
# as usual, first we plot the data
  
  boxplot(Ozone ~ Month, data = airquality)
  # check what would have happened if you would have used 'plot()' instead:
  plot(Ozone ~ Month, data = airquality)
  
  # think about the difference - investigate also what kind of covariate 'Month' is:
  str(airquality)
  
  #what happens if we transform 'Month' into a factor? We can do this directly into the call to 'plot()'
  plot(Ozone ~ factor(Month), data = airquality)

# with boxplots we can also get a visual representation of differences between groups/medians, by adding 'notch = TRUE'  - there are significant differences if the nothces do not overlap
  plot(Ozone ~ factor(Month), data = airquality, notch = TRUE)
  # note the funny shape for month '6' - this is simply determined by lack of data

# now, inspect the plots and form an opinion if you expect there to be significant differences between the groups ('Months', in this case)
# then apply the kruskal test:

  kruskal.test(Ozone ~ Month, data = airquality)

# the official, formal, explanation of the test is: 
# "kruskal.test performs a Kruskal-Wallis rank sum test of the null that the location parameters of the distribution of x are the same in each group (sample). The alternative is that they differ in at least one."
# see that the test is based on the chi-square distribution.


##########################################################################################################################################
# 2. ANOVA - Analysis of Variance

# very similar to regression, just the covariate(s) is factorial and non a continuous numeric one
# we use the same lm(Y ~ x, data = ) comand

# as usual, we first plot our data. Two types of plots are of use here, box plots and bar plot (with error bars), as we discussed previously
# I use here the example from the Crawley book
# use the plant competition experiment dataset - the factorial covariate represents a control level and four experimental treatment -- 2 shoot pruning and 2 root pruning intensities
  
  comp <- read.table("competition.txt", header = TRUE)
  plot(biomass ~ clipping, xlab = "Competition treatment", ylab = "Biomass", data = comp)
  
# look if htere is skew in the data and if the medians differ ebtween control and treatments as well as between treatment levels
# use the 'notch' option
  
  plot(biomass ~ clipping, xlab = "Competition treatment", ylab = "Biomass", data = comp, notch = TRUE)
  
  # --> maybe not a good idea?
  
# for the barplot, we need to manually add the error bars
  
  
  mn <- with(comp, tapply(biomass, clipping, mean))
  se  <- with(comp, tapply(biomass, clipping, function(x) sd(x)/sqrt(length(x))))

  centres <-  barplot(mn, beside=TRUE)
  arrows(centres, mn+se, centres, mn-se, ang=90, code=3)  
  
# do yo think the ylimit need to be increased? How would you do it?
# interpretation: if the error bars overlap, this means the difference is less than 2 times the standard error, which is the rule of thumb for getting a siginficant t-test
# altenratively, multiply the error bars by Student's t: 2.570582 to get the 95% confidence intervals
  
# now, to model the data:

  m1 <- lm(biomass ~ clipping, data = comp)
  summary(m1)

# and for the overall significance:

  anova(m1)

# now, to interpret the factor levels, compare to the level means:

  with(comp, tapply(biomass, clipping, mean))

# and see for example:
  
  465.17 + 88.17

# what does this suggest you? Tecnically, what you see is called 'treatment constrasts levels'.

# test now the model assumptions
  
  plot(m1)
  # or, use  par(mfrow ...) to get all on one page (see last week)



#############################################################################################################  
# repeat these analyses using data from a factorial experiment  
  
  weights <- read.table("growth.txt", header = TRUE)
  
  # plot the data, and fit the models 
  # m2 <- lm(gain ~ supplement, data = weights)
  # m3 <- lm(gain ~ diet, data = weights)
  # m4 <- lm(gain ~ diet + supplement, data = weights)
  # more advanced one - we will discuss this
  
  # m5 <- lm(gain ~ diet * supplement, data = weights)

  

  
#############################################################################################################  
# repeat these analyses using the oneway.txt and oneway2.txt dataframes, as well as the ozone data for differences between all months
# also, see the gannet data (colony differences)
  
  
    ozone.pollution <- read.table("ozone.data.txt", header=TRUE)
  
    GannetDats <- read.csv("GannetConditionData.csv", header=TRUE)
   
  

#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
# 2. GLMs
#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################

# this part closely follows a tutorial by Germán Rodríguez (Princeton University; see: http://data.princeton.edu/R/glms.html) --> a very useful book/R resource, BTW!


# Relaxing the assumption of normal distribution (of residuals)
# glm() instead of lm()
# glm(formula, family, data, weights, subset, ...)
# family is hte key bit:

#Family 		Variance 	Link
#gaussian 		gaussian 	identity
#binomial 		binomial 	logit, probit or cloglog
#poisson 		poisson 	log, identity or sqrt
#Gamma 			Gamma 		inverse, identity or log
#inverse.gaussian 	inverse.gaussian 	1/mu^2
#quasi 	user-defined 	user-defined
#

# for example, to use non-default probit link:
  # glm( formula, family=binomial(link=probit))


# as example, we fit logistic regression models to contraceptive use data

  cuse <- read.table("http://data.princeton.edu/wws509/datasets/cuse.dat", header=TRUE)
  dim(cuse)
  str(cuse)
  summary(cuse)
  head(cuse)
  
  glm1 <- glm( cbind(using, notUsing) ~ age + education + wantsMore , family = binomial, data = cuse)
  
  # note the cbind() as response!
  # otherwise, use a vector of 1 (success) and failures (0)
  # here, cbind() is the correcct one to use (why?)

# inspect results
  
  summary(glm1)

# note the residual deviance - it is highly significant, as can be seen:

  1-pchisq(29.92,10)	# gives a value well below .001
  
  
# note the default treatment contrast - especially the alphabetical order 
# if you would like to change this, either use relevel() or define new variables with different order
# here an example to define indicators for women with high education and women who want no more children: 

  cuse$noMore <- with(cuse, wantsMore == "no")
  cuse$hiEduc <- with(cuse, education == "high")
  
  glm2 <- glm( cbind(using,notUsing) ~ age + hiEduc + noMore, family=binomial, data = cuse)
  
  summary(glm2)
  
  anova(glm2, test="Chisq")	# note hte need to add the test = "" part
  
  # how about an interaction term
  
  
  glm3 <- glm( cbind(using,notUsing) ~ age + hiEduc * noMore, family=binomial, data = cuse)
    
  summary(glm3)
    
  anova(glm3, test="Chisq")	# note the need to add the test = "" part
  
  
# remember the useful functions to extract results from the fit, such as:

    #residuals or resid, for the deviance residuals
    #fitted or fitted.values, for the fitted values (estimated probabilities)
    #predict, for the linear predictor (estimated logits)
    #coef or coefficients, for the coefficients, and
    #deviance, for the deviance.   
  

# model selection can be done in various ways, similar to lm() model selection

  # compare drop() to drop1() to step()
  
  drop1(glm3)
  drop(glm3)
  step(glm3)
  
  # can you explain the results?

# or you can do it by hand, by updating your models, followed by anova(m1,m2)

  glm4 <- update(glm3, ~ . - age:noMore)	# of course this will be identical to our glm2
  
  anova(glm4, glm2)

# to evaluate model assumptions you use the usual residual plots (note, here deviance residuals are used)

  plot(glm3)



###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
# overdispersion count data
# data = counts of high school students diagnosed with an infectious disease, measured as N days from an initial outbreak.
# data and example from:
# http://www.theanalysisfactor.com/generalized-linear-models-in-r-part-6-poisson-regression-count-variables/
# and
# https://www.theanalysisfactor.com/glm-r-overdispersion-count-regression/

  students <- read.csv("studentData.csv")

# fit Poisson GLM model
  
  glm1 <- glm(Students ~ Days, family = poisson)
  summary(glm1)
  # interpret the estimated coefficient for 'Days'
  
  glm2 <- glm(Students ~ Days, family = quasipoisson)
  summary(glm2)
  
  # note the hcange in the residual variance and assumed/estimated value for the Dispersion parameter
  # thus we actually do have under- dispersion here, not over dispersion (which happens if residual deviance > degrees of freedom
  
  plot(Students ~ Days, data - students)
   lines(predict(glm2, type = "response"))
   lines(predict(glm1, type = "response"), col = 2)
  # same predictions in this case!
  
  

  
###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
# GAM models

# use Mid-Atlantic Wage Data, from the package ILRS
# example modified after https://www.r-bloggers.com/generalized-additive-models/
  
  Wage <- read.csv("WageData.csv")
   
  
  gam1 <- gam(wage~s(age) + s(year) + education ,data = Wage)
  # this happens when there are not enough data to fit a more complex 'wiggly' line, hence reduce model complexity
  gam1 <- gam(wage~s(age, k = 6) + s(year, k = 6) + education ,data = Wage)
  # we can also directly plot it:
  plot(gam(wage~s(age, k = 6) + s(year, k = 6) + education ,data = Wage), pages = 1)
  # or plot(gam1)
  
  # compare to (note differences):
  
  plot(gam(wage~s(age, k = 6) + s(year, k = 6) + education ,data = Wage), pages = 1, scale = 0, shade = TRUE)
  # see:
  ?plot.gam
  
  # see model parameters:
  
  summary(gam1)
  
# read in detail:

  ?gam
  
  # especially read: http://127.0.0.1:17582/library/mgcv/html/formula.gam.html
  
  # and : http://127.0.0.1:17582/library/mgcv/html/smooth.terms.html
  # understand well what different smooth terms do
  
  
# example of logistic regression, by transforming response into binary variable
  
  gam2 <- gam(I(wage > 250) ~ s(age, k = 4) + s(year, k=4) + education , data=Wage, family=binomial)
  plot(gam2, pages = 1)
  
  # what are your conclusions?
  
  
  
#############################################################################################################
# seasonal temperature data modelling with GAM

  cet <- read.csv("CETdata.csv")

  ylab <- expression(Temperature ~ (degree*C))
  plot(Temperature ~ Year, data = annCET, type = "l", ylab = ylab, main = "CET")

  gamm1 <- gamm(Temperature ~ s(nMonth, bs = "cc", k = 12) + s(Time), data = cet)	# note, this is a GAMM model, not a GAM - contains a random effects part
  
  
  # k specifies the dimensions of the basis used for the spline. Here I set it to the maximum possible for nMonth, which is 12 the number of unique values. 
  # bs allows you to specify the basis type for the smooth term; 
  # “cc” indicates a cyclic cubic spline, which we want for the seasonal term as there should be no discontinuity between January and December. 
  
# read the excellent explanation on cyclic splines in: https://www.r-bloggers.com/modelling-seasonal-data-with-gams/

  summary(gamm1$gam)	# compare to summary(gamm1$lme)
  
  plot(gamm1$gam, pages = 1)
  
# now let's check for autocorrelation
  
  par(mfrow=c(1,2))
  acf(resid(gamm1$lme), lag.max = 36, main = "ACF")
  pacf(resid(gamm1$lme), lag.max = 36, main = "pACF")
  par(mfrow=c(1,1))
  
  
# Models with correlated errors
###############################

  
  # we have to play around a bit with the control parameters
  ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")
  
  ## AR(1)
  m1 <- gamm(Temperature ~ s(nMonth, bs = "cc", k = 12) + s(Time, k = 20),  data = cet, correlation = corARMA(form = ~ 1|Year, p = 1),     control = ctrl)
  
  ## AR(2)
  m2 <- gamm(Temperature ~ s(nMonth, bs = "cc", k = 12) + s(Time, k = 20),  data = cet, correlation = corARMA(form = ~ 1|Year, p = 2),     control = ctrl)
  
  ## AR(3)
  m3 <- gamm(Temperature ~ s(nMonth, bs = "cc", k = 12) + s(Time, k = 20),   data = cet, correlation = corARMA(form = ~ 1|Year, p = 3),    control = ctrl)
  

# compare models
  
  anova(gamm$lme, m1$lme, m2$lme, m3$lme)

layout(matrix(1:2, ncol = 2))
res <- resid(m2$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(2) errors")
pacf(res, lag.max = 36, main = "pACF- AR(2) errors")
layout(1)


want <- seq(1, nrow(cet), length.out = 200)
pdat <- with(cet,              data.frame(Time = Time[want], Date = Date[want],                         nMonth = nMonth[want]))

## predict trend contributions
p  <- predict(gamm1$gam,  newdata = pdat, type = "terms", se.fit = TRUE)
p1 <- predict(m1$gam, newdata = pdat, type = "terms", se.fit = TRUE)
p2 <- predict(m2$gam, newdata = pdat, type = "terms", se.fit = TRUE)
p3 <- predict(m3$gam, newdata = pdat, type = "terms", se.fit = TRUE)

## combine with the predictions data, including fitted and SEs
pdat <- transform(pdat,              p  = p$fit[,2],  se  = p$se.fit[,2],       p1 = p1$fit[,2], se1 = p1$se.fit[,2],         p2 = p2$fit[,2], se1 = p2$se.fit[,2],              p3 = p3$fit[,2], se1 = p3$se.fit[,2])



op <- par(mar = c(5,4,2,2) + 0.1)
ylim <- with(pdat, range(p, p1, p2, p3))
ylim[1] <- floor(ylim[1])
ylim[2] <- ceiling(ylim[2])
ylab <- expression(Temperature ~ (degree*C ~ centred))
plot(Temperature - mean(Temperature) ~ Date, data = cet, type = "n",      ylab = ylab, ylim = ylim)
lines(p  ~ Date, data = pdat, col = "black")
lines(p1 ~ Date, data = pdat, col = "red")
lines(p2 ~ Date, data = pdat, col = "blue")
lines(p3 ~ Date, data = pdat, col = "forestgreen", lwd = 1)
legend("topleft",       legend = c("Uncorrelated Errors", paste0("AR(", 1:3, ") Errors")),       bty = "n", col = c("black","red","blue","forestgreen"),       lty = 1, lwd = c(1,1,1))
par(op)


  
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
# END
############################################################################################################################################################

























