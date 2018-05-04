#####################################################################
## MovEcol Workshop, UNESP Rio Claro, Brazil, 19/02 - 23/02 - 2018 ##
##						                   ##
##               Movement Path Analysis 	                   ##
##	        Luca Borger, 20/02/2018		         	   ##
##			part 2				           ##	
#####################################################################

###################################################################################################################################################
# we continue by using the DF we had ended up with at yesterday's practical
# I wrote it to file, so you can load it without having to run everything again
# code to write a dataframe to a cvs file: 
# write.csv(elkDataSrtB, file = "elkDataGpsSrtB.csv")
  
#  elkDataSrtB <- read.csv("elkDataGpsSrtB.csv")

# if you had to reload the data above, you will need to format/set the Date/Time again:

  elkDataSrtB$DateTime <- as.POSIXct(elkDataSrtB$DateTime, format="%Y-%m-%d %H:%M:%S", tz = "UTC") 
  elkDataSrtB$Date <- as.Date(elkDataSrtB$DateTime)  


###################################################################################################################################################


################################################################
# add time as hour of the day - use it to subset and regularize the data  
# select first location each hour, so to retain one location per hour only
# use aggregate() to find the first record/position for each individual
# use merge() to select those record from the original data frame

   
  elkDataSrtB$hour <- as.numeric(strftime(as.POSIXlt(elkDataSrtB$DateTime),format="%H"))
  
  elkDataSrtB$nrow <- 1:nrow(elkDataSrtB)
  
  foo <- aggregate(nrow ~ individual.local.identifier + Date + hour, data = elkDataSrtB, FUN = min)
  
  #alternative way
  #foo <- aggregate(DateTime ~ individual.local.identifier + Date + hour, data = elkDataSrtB, FUN = min)
    
  foo2 <- merge(elkDataSrtB, foo, by = "nrow")
  
  dim(foo); dim(foo2)
  
  foo3 <- subset(foo2, select = -c(SL, SLhvs, dt,brn, ta,  hour.x, individual.local.identifier.y, Date.y, hour.y))
  
  names(foo3)[2] <- "individual.local.identifier"
  
  foo4 <- foo3[order(foo3$individual.local.identifier,foo3$DateTime),]
  
  rm(foo, foo2, foo3)
  
  
# calculate again time between steps/locations
  
  foo <- difftime(foo4$DateTime[2:nrow(foo4)], foo4$DateTime[1:(nrow(foo4)-1)], units = "secs")
  foo <- c(NA, foo)
  summary(as.numeric(foo))
  foo <- ifelse(foo4$firstRec == 1, NA, foo)
  summary(as.numeric(foo))
  foo4$dt <- foo
  rm(foo)  
  
# identify and remove locations at < 2 hour distance
# allow some imprecision around the 2 hour time
  
  foo <- which(foo4$dt < 7000)
  
  foo5 <- foo4[-foo,]
  rm(foo)
  
# calculate again time between steps/locations
  
  foo <- difftime(foo5$DateTime[2:nrow(foo5)], foo5$DateTime[1:(nrow(foo5)-1)], units = "secs")
  foo <- c(NA, foo)
  summary(as.numeric(foo))
  foo <- ifelse(foo5$firstRec == 1, NA, foo)
  summary(as.numeric(foo))
  foo5$dt <- foo
  rm(foo)  
    
  hist(foo5[foo5$dt < 8000,'dt'])
  hist(foo5[foo5$dt < 7500,'dt'])
  
# add back in step length values
  
  foo <- sqrt((foo5$utm.easting[1:(nrow(foo5)-1)] - foo5$utm.easting[2:(nrow(foo5))])^2 + (foo5$utm.northing[1:(nrow(foo5)-1)] - foo5$utm.northing[2:(nrow(foo5))])^2)
  foo5$SL <- c(NA, foo)
  rm(foo)
  foo5$SL <- ifelse(foo5$firstRec == 1, NA, foo5$SL)
  summary(foo5$SL) # notice the difference
  
  # step length at 2 hour scale
  
  summary(foo5[foo5$dt < 7500,'SL'])
  
  # a few high values to inspect
  # additional criterion to use is a 'speed filter' -- exclude locations with two high speed

# inspect temporal variation 2-hour step length
# but need to fix a few column names first

  names(foo5)
  names(foo5)[2] <- "ID"
  names(foo5)[3] <- "individual.local.identifier"
  names(foo5)[30] <- "Date"
 

  library(lattice)
  xyplot(log(SL + 1) ~ DateTime|individual.local.identifier, data = foo5, subset = dt < 7500, type = "b")
  
  library(mgcv)
  plot(gam(log(SL + 1) ~ s(day, bs="cc"), data = foo5, subset = dt < 7500 & individual.local.identifier == "YL42"))
    # bs="cc" specifies a cyclic cubic regression splines
  
# compare to using a segmentation method  - does it identify break points?
# let us select SL values for one individual  
  
  SL_YL42 <- foo5[foo5$dt < 7500 & foo5$individual.local.identifier == "YL42", 'SL']
  SL_YL42 <- na.omit(SL_YL42)

  # install.packages('Segmentor3IsBack')  
  require('Segmentor3IsBack')
  
  # Exact change-point algorithm for the segmentation of profiles according to the log-likelihood criterion for 5 possible models: 
  # Poisson, Gaussian homoscedastic, negative binomial, Gaussian with constant mean and Exponential.
  # for exemplification, we use the default Poisson one
  # see: ?Segmentor3IsBack
  
  ##  Los breakpoint me ayuda por definicion para segmentar los pasos de los bichos 
  #por ejemplo me ayuda a definir , por cuantos segmentos podre realizar los pasos de los animales.
 ?Segmentor
   res100 <- Segmentor(data=as.numeric(SL_YL42),Kmax=100)
  str(res100)
  breaks100 <- as.numeric(res100@breaks[100,])
  plot(1:100,res100@likelihood[,1])  
  
  # inpsect with more potential breakpoints
  
  res500 <- Segmentor(data=as.numeric(SL_YL42),Kmax=500)
  str(res500)
  breaks500 <- as.numeric(res500@breaks[500,])
  plot(1:500,res500@likelihood[,1])  
  
  res1000 <- Segmentor(data=as.numeric(SL_YL42),Kmax=1000)
  str(res1000)
  breaks1000 <- as.numeric(res1000@breaks[1000,])
  plot(1:1000,res1000@likelihood[,1])  

  
  
# plot the time series and break points
  
  plot(1:length(SL_YL42), log(SL_YL42 + 1), type = "l")
  points((1:length(SL_YL42))[breaks100], (log(SL_YL42 + 1))[breaks100], col=2, pch=19,cex=1.5)

  plot(1:length(SL_YL42), log(SL_YL42 + 1), type = "l")
  points((1:length(SL_YL42))[breaks500], (log(SL_YL42 + 1))[breaks500], col=2, pch=19,cex=1.5)

  plot(1:length(SL_YL42), log(SL_YL42 + 1), type = "l")
  points((1:length(SL_YL42))[breaks1000], (log(SL_YL42 + 1))[breaks1000], col=2, pch=19,cex=1.5)


# think about what you actually might want to obtain from this and compare to previous GAM model, as well as observed pattern in plotted data


################################################################  
  
##########################################################################################################################################################################
# Movement Path Segmentation - method of Lavielle 

# Lavielle method: method to identify breaks in timeseries (e.g. of speed)
# assumes data generated by process: Yi = µi + si ei
# can assume that only mean, or standard deviation, or both changes between segments, and assess adequacy of these assumptions using three contrast functions
# need to specify a minimum number of locations for each segment
# the number K of partitions to achieve are determined by a contrast function in relation to K, to find Kopt
# method is implemented in function lavielle()

  # load library if needed
  # library(adehabitatLT)
  
# continue with the ltraj object: Elktraj
# convert to ltraj object (type II) if needed

  # Elktraj <- as.ltraj(xy = elkDataSrtB[,c("utm.easting","utm.northing")], date = elkDataSrtB$DateTime, id = elkDataSrtB$individual.local.identifier)
  
 
  l1 <- lavielle(na.omit(Elktraj[1][[1]]$dist), Lmin=3, Kmax=50, type="mean")
  l1
  str(l1)

 # These functions allow to perform a non-parametric segmentation of a time series using the penalized contrast method of Lavielle (1999, 2005). 
 # The function lavielle computes the contrast matrix (i.e., the matrix used to segment the series) either from a series of observations or from 
 # an animal trajectory. The function chooseseg can be used to estimate the number of segments building up the trajectory. The function findpath 
 # can be used to find the limits of the segments (see Details).


  chooseseg(l1) # see also plot --> the Jk value becomes much less negative starting with K=12. To check, redo with higher K

  l2 <- lavielle(na.omit(Elktraj[1][[1]]$dist), Lmin=3, Kmax=100, type="mean")
  l2
  chooseseg(l2)

# continues to clearly decrease, although clearly K = 17 is a breakpoint
  
  l3 <- lavielle(na.omit(Elktraj[1][[1]]$dist), Lmin=3, Kmax=500, type="mean")
  l3
  chooseseg(l3)
  
  ########Elktraj[1][[1]]$dist <- 1 es el individuo y el otro 1 es el estadistico.


# we can visualize the segmentation

  fp <- findpath(l1, 12)
  fp2 <- findpath(l2, 50)
  fp3 <- findpath(l3, 200)
  
  
# Discuss and compare to ideal case in the help file:
  
  ?lavielle()

# do the same for a few other individuals



##############################################################################################################################
# now do for the residence time (method of Barraquand & Benhamou 2008)
# choose 1km radius and 6 hours as maximum time threshold that the animal is allowed to spend outside the patch 
# before we consider that the animal actually left the patch
  
  elk4049 <- Elktraj[1]
  res4049 <- residenceTime(elk4049, radius = 1000, maxt=6, units="hour")
  plot(res4049)

# maybe lets increase to 2km and 12 hours
  res4049 <- residenceTime(elk4049, radius = 2000, maxt=12, units="hour")
  plot(res4049)

# add this to the infolocs slot

  res4049 <- residenceTime(elk4049, radius = 2000, maxt=12, addinfo = TRUE, units="hour")
  res4049

# repeat for the other individuals





###################################################################################################################################################################################
# Behavioral Change Point Analysis in R:
# The bcpa package
# see paper by  Gurarie et al. 2009 Ecol. Lett.
# key points: allows irregular time series --> continuous time!
# multidimensionality, autocorrelations, robust to messy data/internal structure

# The BCPA uses a likelihood-based method for identifying significant changes in movement parameter values across long, complex datasets by sweeping an analysis window over the timeseries 
# and identifying the most likely changepoints, while simultaneously testing which, if any, of the parameters might have changed at that changepoint.

# as with lavielle, choose response variable (veloctiy = speed/time or Vcos(theta) ('persistence velocity')

  library(bcpa)

# exemplify first with a simulated dataset:

  data(Simp)
  plot(Simp)
  ?GetVT
  Simp.VT <- GetVT(Simp)
  ?WindowSweep
 
   Simp.ws <- WindowSweep(Simp.VT, "V*cos(Theta)", windowsize = 50, windowstep = 1, progress=FALSE)
  plot(Simp.ws, threshold=7)
  plot(Simp.ws, type="flat", clusterwidth=3)
  PathPlot(Simp, Simp.ws)
  PathPlot(Simp, Simp.ws, type="flat")
  DiagPlot(Simp.ws)


###################################################################
# application to 4049 (just to start exemplifying -- will require more in depth data & model exploration)


  X <- elkDataSrtB[elkDataSrtB$individual.local.identifier == "4049",'utm.easting']
  Y <- elkDataSrtB[elkDataSrtB$individual.local.identifier == "4049",'utm.northing']
  Time <- as.numeric(elkDataSrtB[elkDataSrtB$individual.local.identifier == "4049",'DateTime'])

??MakeTrack

  mytrack <- MakeTrack(X,Y,Time)
  
  plot(mytrack)

# get step lengths, turning anlges and othr relevant statistics

  mytrack.VT <- GetVT(mytrack)
  head(mytrack.VT)
  summary(mytrack.VT)

# plot step lenghts and movement persistence distribution

  par(mfrow=c(1,2))
  hist(mytrack.VT$V, breaks=20, col="grey")
  hist(mytrack.VT$Theta, breaks=20, col="grey")
  par(mfrow=c(1,1))


# apply window sweep approach to find break points

  mytrack.ws <- WindowSweep(mytrack.VT, "V*cos(Theta)", windowsize=50, progress=FALSE, K=2)
  str(mytrack.ws)

# plot output

  plot(mytrack.ws, type = "smooth")

  plot(mytrack.ws, type = "flat")

  ###tal ves tenga que sacar el simp de la siguiente formula
  
  PathPlot(mytrack, mytrack.ws, type="flat", clusterwidth = 3, main="Flat BCPA")
  PathPlot(mytrack, mytrack.ws, type="smooth", main="Smooth BCPA")


  

# An additional, potentially interesting visualization of the analysis is the "phase plot", which illustrates how
# the three parameters change with respect to each other:

  PhasePlot(mytrack.ws, type="smooth", clusterwidth = 3)

# 
  DiagPlot(mytrack.ws)
  vignette("bcpa")
  vignette("adehabitatHR")

# things to improve, likely ...

###########################################


###########################################################################################################################################################################################
## END of Movement Path Segmentation
###########################################################################################################################################################################################


  
  
  
  
  