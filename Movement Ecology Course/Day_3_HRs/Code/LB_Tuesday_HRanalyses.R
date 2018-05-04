#####################################################################
## MovEcol Workshop, UNESP Rio Claro, Brazil, 19/02 - 23/02 - 2018 ##
##						                   ##
##               Home Range Analysis 	                   	   ##
##	        Luca Borger, 21/02/2018		         	   ##
##							           ##	
#####################################################################

###########################################
# we continue with the dataset: elkDataSrtB 
  
  elkDataSrtB <- read.csv("elkDataGpsSrtB.csv")

# set the date & time; inlcude also a 'month' column

  elkDataSrtB$Date <- as.Date(elkDataSrtB$Date, form = "%Y-%m-%d")
  elkDataSrtB$DateTime <- as.POSIXct(elkDataSrtB$timestamp, format="%Y-%m-%d %H:%M:%S", tz = "UTC") 
  elkDataSrtB$month <- as.numeric(strftime(as.POSIXlt(elkDataSrtB$DateTime),format="%m"))

# add a column 'IND' as individual identifier with a shorter name

  elkDataSrtB$IND <- elkDataSrtB$individual.local.identifier
  
  
# load the libraries

  library(adehabitatHR)
  library(ks)
  library(lme4)

##################################################################################
# subset to one location every 2 hours as we did yesterday
# add time as hour of the day - use it to subset and regularize the data  
# select first location each hour, so to retain one location per hour only
# use aggregate() to find the first record/position for each individual
# use merge() to select those record from the original data frame

   
  elkDataSrtB$hour <- as.numeric(strftime(as.POSIXlt(elkDataSrtB$DateTime),format="%H"))
  
  elkDataSrtB$nrow <- 1:nrow(elkDataSrtB)
  
  foo <- aggregate(nrow ~ individual.local.identifier + Date + hour, data = elkDataSrtB, FUN = min)
   
  foo2 <- merge(elkDataSrtB, foo, by = "nrow")
  
  dim(foo); dim(foo2)
  
  foo3 <- subset(foo2, select = -c(SL, SLhvs, dt,brn, ta,  hour.x, individual.local.identifier.y, Date.y, hour.y))
  names(foo3)
  
  names(foo3)[3] <- "individual.local.identifier"
  names(foo3)[30] <- "Date"
  
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
  rm(foo, foo4)
    

  
# select only elks sampled over longer time and with similar N locs in the summer months (supposed to be more stationary period)

  elks <- c("YL74","YL77", "YL78", "YL5", "YL59", "YL64", "YL15", "YL2", "YL25", "YL29", "YL42", "4049", "GP2", "GR182")
   
  elkSummer <- subset(foo5, month > 5 & month < 9 & IND %in% elks)
  elkSummer <- droplevels(elkSummer)

  with(elkSummer, (table(IND, month,year)))

# clean up
  
  rm(foo5)

#################################################################################
# transform into spatial data frame for estimating homoe ranges with adehabitatHR

  elkSPDF <- SpatialPointsDataFrame(coords = elkSummer[,c("utm.easting","utm.northing")], data = data.frame(IND = elkSummer[,'IND']))


# kernelUD will be our friend - let us inspect what it wants and does

# kernelUD(xy, h = "href", grid = 60,
#         same4all = FALSE, [...]	# have excluded some
#         kern = c("bivnorm", "epa"), extent = 1,
#         boundary = NULL)


#Arguments
# xy	
# An object inheriting the class SpatialPoints containing the x and y relocations of the animal. 
# If xy inherits the class SpatialPointsDataFrame, it should contain only one column (factor) 
# corresponding to the identity of the animals for each relocation.
#
# h	
# a character string or a number. If h is set to "href", the ad hoc method is used for the 
# smoothing parameter (see details). If h is set to "LSCV", the least-square cross validation method is used. 
# Note that "LSCV" is not available if kern = "epa". Alternatively, h may be set to any given numeric value
#
# grid	
# a number giving the size of the grid on which the UD should be estimated. Alternatively, this parameter 
# may be an object inheriting the class SpatialPixels, that will be used for all animals. For the function 
# kernelUD, it may in addition be a list of objects of class SpatialPixels, with named elements corresponding 
# to each level of the factor id.
#
# kern	
# a character string. If "bivnorm", a bivariate normal kernel is used. If "epa", an Epanechnikov kernel is used.
#
# extent	
# a value controlling the extent of the grid used for the estimation (the extent of the grid on the abscissa is 
# equal to (min(abscissa.relocations) + extent * diff(range(abscissa.relocations))), and similarly for the ordinate).
#
# same4all	
# logical. If TRUE, the same grid is used for all animals. If FALSE, one grid per animal is used. Note that when 
# same4all = TRUE, the grid used for the estimation is calculated by the function (so that the parameter grid cannot 
# be a SpatialPixels object).
#
# boundary	
# If, not NULL, an object inheriting the class SpatialLines defining a barrier that cannot be crossed by the animals. 
# There are constraints on the shape of the barrier that depend on the smoothing parameter h (***see details***)
#


     
  allUD <- kernelUD(subset(elkSPDF, select=IND), extent=1, grid=200, same4all=TRUE)
  image(allUD)
      
      
  # the UD provides an estimate of probability density function, i.e. the probability of finding an animal in a certain point in space
  # the home range can than be defined at different levels of probability, e.g. the 90% value corresponds to the smallest area on which the
  # location probability is equal to 90%
  # to get these values use the functions getvolumeUD and, especially, getverticeshr
      
  allUDHrefiso95 <- getverticeshr(allUD, percent = 95)
  allUDHrefiso90 <- getverticeshr(allUD, percent = 90)
  allUDHrefiso50 <- getverticeshr(allUD, percent = 50)
      
  # This generates a 'SpatialPolygonsDataFrame' with loads of information
      
  str(allUDHrefiso95)
      
  # plot isolines  
  plot(allUDHrefiso95)
  plot(allUDHrefiso95, col=c(1:8, "orange")) # --> note the strong overlap between individuals
  plot(allUDHrefiso95, col=topo.colors(16)) # 
  plot(allUDHrefiso95, col=rainbow(16)) # 
  plot(allUDHrefiso95, col=heat.colors(16)) # 
  plot(allUDHrefiso95, col=terrain.colors(16)) # 
  plot(allUDHrefiso95, col=cm.colors(16)) # 
      
      
  # another way to plot it, including also the UD, and the labels of the contour lines correspond to the probability levels of the home range isopleths
      
  par(mfrow=c(4,4))
  for (i in unique(elkSummer$IND)) {
  udKerHref <- kernelUD(xy=SpatialPoints(subset(elkSPDF, IND==i)), h = "href", grid=200, extent=1, kern = "bivnorm")  
  volUD <- getvolumeUD(udKerHref)
  xyz <- as.image.SpatialGridDataFrame(volUD)
  image(xyz,col=topo.colors(15), main=i)
  points(subset(elkSPDF, IND == i),cex=0.5)   
  contour(xyz,col=topo.colors(15), levels = seq(25,95,by=5),add=TRUE,lwd=1)
  }
  par(mfrow=c(1,1)) 
      
      
  # now that we've learned to plot the UDs, back to the (in)famous smoothing parameter 'h'...
  # as highlighted in all stats books on kernel density estimation, visual inspection of the impact of different h values is crucial
  # in order to compare, keep the same xy limits as used for the href-based HR estimates
  # to exemplify, let's change from the standard href value to a very large value, which will hence induce marked oversmoothing
      
  allUD <- kernelUD(subset(elkSPDF, select=IND), extent=0.5, grid=200, same4all=TRUE)
  image(allUD)
      
# let us remind of the range of href values for the individuals, then choose a much larger value for all
  summary(sapply(allUD, function(x) x@h$h))
      
  allUDsilly <- kernelUD(subset(elkSPDF, select=IND), h = 20000, extent=0.5, grid=200, same4all=TRUE)
  image(allUDsilly)
      
  # pretty clear, eh? 
      
# Now, the href can be considered as a useful, conservative, baseline estimate, which not rarely already provides the necessary information 
# (e.g. for comparing differences between individuals or populations)
# the LSCV is best avoided (often fails with movement data), whereas a useful alternative seems to be the plug-in estimation approach 
# (not implemented in adehabitatHR, though) --> will use library(ks) for that!
# an interesting and worthwhile rule is to take the h value obtained from the href procedure and to decrease the value until the continuous home range
# outline breaks up into multiple polygons (note that this procedure is the same as the recommended procedure for choosing the k value of nearest
# neighbours for the LoCoH non-parametric smoothing method of Getz et al. 
# we have seen both with Gabriele, so let's move to the plug-in estimator, followed then by the Likelihood cross-validation method
# we first exemplify with Apollo
      
  library(ks)
  elk4049 <- droplevels(as.data.frame(subset(elkSummer, IND == "4049")))
  elk4049 <- elk4049[,c('utm.easting','utm.northing')]
      
      band1 <- Hpi(elk4049)
      HRplugin <- kde(x = elk4049, H = band1, binned=FALSE, xmin =  c(min(elk4049$utm.easting)-2000,min(elk4049$utm.northing)-2000), xmax = c(max(elk4049$utm.easting)+2000,max(elk4049$utm.northing)+2000),gridsize = 200) 
      plot(HRplugin, cont=c(1,5,10,25,50,75,95),drawpoints=TRUE,display="slice")
      plot(HRplugin, cont=c(1,5,10,25,50,75,95),drawpoints=TRUE,display="persp")
      plot(HRplugin, cont=c(1,5,10,25,50,75,95),drawpoints=TRUE,display="image")
      
# compare href and plug in
      
      # note different syntax using 'SpatialPoints' object
      elk4049.UD <- kernelUD(xy=SpatialPoints(coords = elkSummer[elkSummer$IND=="4049",c("utm.easting","utm.northing")]), h = "href", grid=200, extent=0.5, kern = "bivnorm") 
      par(mfrow=c(1,2))
      plot(HRplugin, cont=seq(25,95,by=10),drawpoints=TRUE,display="slice",lwd=2, col = "orange",cex=0.2, col.pt=1)
      xyz <- as.image.SpatialGridDataFrame(elk4049.UD)
      plot(utm.northing ~ utm.easting, data = subset(elkSummer, IND == "4049"),cex=0.2, pch= 19, xlim = c(min(utm.easting)-10000, max(utm.easting)+10000), ylim = c(min(utm.northing)-10000, max(utm.northing)+10000))
      contour(xyz,col=1:length(seq(25,95,by=10)),add=TRUE,lwd=2)
      contour(xyz,col=topo.colors(8), levels = seq(25,95,by=10),add=TRUE,lwd=1)
      par(mfrow=c(1,1))
      
      
      # compare to the 'minimum-href' estimate
      
      elk4049sp <- subset(elkSummer, IND=="4049")
      
      hValsCorr <- seq(1.2, 0.3, by=-0.05)     
      hValsA <- numeric(length(hValsCorr))
      nPolygons <- numeric(length(hValsCorr))      
      for (j in 1:length(hValsCorr)) {
      
      # calculate the kernel where different h values are given in each round of the loop
      udKerHvals <- kernelUD(
      # we feed to kernelUD() one individual at the time as a spatialpoint object
      xy=SpatialPoints(coords = elkSummer[elkSummer$IND=="4049",c("utm.easting","utm.northing")]), h = elk4049.UD@h$h*hValsCorr[j], grid=200, extent=0.5, kern = c("bivnorm"), same4all=T)
      
      hValsA[j] <- as.numeric(udKerHvals@h$h)
      hrKer95 <- getverticeshr(udKerHvals, percent = 95)
      nPolygons[j] <- length((hrKer95@polygons[[1]]@Polygons))
      }
      
      # inspect values
      
      hValsA
      nPolygons
      
      #' We take the h value just one step above to estimate home ranges
      minHval <- hValsA[max(which(nPolygons==1))]
      # check it ...
      hValsA[7]
      minHval
      
      
      #' And now we estimate and plot the 95% isopleth for each resident group using the h value that we just obtained. 
      
           
      min4049 <- kernelUD(xy=SpatialPoints(coords = elkSummer[elkSummer$IND=="4049",c("utm.easting","utm.northing")]), h = minHval, grid=200, extent=0.5, kern = c("bivnorm"))
      min4049Iso95 <- getverticeshr(min4049, percent = 95)
      href4049Iso95 <- getverticeshr(elk4049.UD, percent = 95)
      min4049Iso50 <- getverticeshr(min4049, percent = 50)
      href4049Iso50 <- getverticeshr(elk4049.UD, percent = 50)  
      
      #xlim = c(min(utm.easting)-10000, max(utm.easting)+10000), ylim = c(min(utm.northing)-10000, max(utm.northing)+10000)
      #xlim = c(763920, 792590), ylim = c(7821630, 7851545)
      
      
      par(mfrow=c(1,2))
      plot(HRplugin, cont=c(95,50),drawpoints=TRUE,display="slice",lwd=2, col = "orange",cex=0.2, col.pt=1)
      plot(href4049Iso95, col = "light grey", lwd = 2, border = "blue" )
      plot(min4049Iso95, add = TRUE, col = NA, lwd = 3, border = "dark green")
      plot(href4049Iso50, add = TRUE, col = NA, lwd = 3, border = "pink")
      plot(min4049Iso50, add = TRUE, col = NA, lwd = 3, border = "orange")
      points(elkSummer[elkSummer$IND=="4049", ], pch = ".")
      par(mfrow=c(1,1))
      
      ##############
      
      
      # Another alternative to href is likelihood cross-validation (CVh; Horne and Garton 2006)
      # Here we use code by Jon Horne, modified after Horne et al. (2018)
      # Load the CVh function:
      
      minfunc = function(h,Data,LocNums){
      CVhScore = 0
      for (i in 1:nrow(Data)){
      densityatiwouti = 0
      sqdist = (Data$X[i]-Data$X)^2+(Data$Y[i]-Data$Y)^2
      Part1 = 1/(2*pi*(h^2))
      Part2 = exp(-sqdist/(2*(h^2)))
      densityati = Part1*Part2
      indices = which(!LocNums==i)
      densityatiwouti =sum(densityati[indices])
      if (densityatiwouti<10^-323){densityatiwouti = 10^-323}	
      ln_densityatiwouti = log(densityatiwouti)
      CVhScore = CVhScore+ln_densityatiwouti
      } #end for i
      CVhScore = -CVhScore
      return(CVhScore)
      } #end function
      
      
      # Create an input dataframe for calculating likelihood cross-validation smoothing parameter.  
      # You need a column for a unique identifier for each individual and the x and y coordinates of each location.
      
      elk4049 <- droplevels(as.data.frame(subset(elkSummer, IND == "4049")))
      elk4049 <- elk4049[,c('IND','utm.easting','utm.northing')]
      CVh_data <- elk4049
      colnames(CVh_data) = c("ID","X","Y")
      
      # Set the lower limit to search for CVh:
      lowerlimit = .05 	#Lower limit of search for CVh is lowerlimit*hRef; Upper limit for search is Href
      
      
      # Calculate CVh and create an output dataframe "Results".  
      
      UniqueIDs = levels(as.factor(CVh_data$ID))
      Results = as.data.frame((array(UniqueIDs[1],1)))
      names(Results)="ID"
      Results$Href = 0
      Results$CVh = 0
      ResultsDataRow = 1
      for (id in 1:length(UniqueIDs)){
      if(nrow(CVh_data[CVh_data$ID==UniqueIDs[id],])>1){
      CurrentData = CVh_data[CVh_data$ID==UniqueIDs[id],]			
      meanx = mean(CurrentData$X)
      meany = mean(CurrentData$Y)
      varx = var(CurrentData$X)
      vary = var(CurrentData$Y)
      Href = sqrt((varx+vary)/2)*nrow(CurrentData)^(-1/6)	# note how easy in fact it is to estimate href!
      LocNums = 1:nrow(CurrentData)
      optimizeRes = optimise(f = minfunc, interval = c((lowerlimit*Href),Href), Data = CurrentData, LocNums = LocNums)
      h_LCV = optimizeRes$minimum
      if (ResultsDataRow==1){
      Results$Href[ResultsDataRow] = Href
      Results$CVh [ResultsDataRow] = h_LCV	
      ResultsDataRow = ResultsDataRow+1		
      } else {
      Results = rbind (Results,Results[1,])
      Results$ID[ResultsDataRow] = UniqueIDs[id]
      Results$Href[ResultsDataRow] = Href
      Results$CVh [ResultsDataRow] = h_LCV	
      ResultsDataRow = ResultsDataRow+1						
      }#end if
      }#end if there is at least one location	
      } #end uniqueID loop
      
      # so in this case LCV leads to a drastic reduction of smoothing
      
      Results
      
      # check if href values are the same with adehabitatHR
      
      elk4049.UD@h$h	# all looks fine
      
      # use hLCV to estimate UD
      # plot the results and compare to href and min_href UDs
      
      
      Lcv4049 <- kernelUD(xy=SpatialPoints(coords = elkSummer[elkSummer$IND=="4049",c("utm.easting","utm.northing")]), h = Results$CVh, grid=200, extent=0.5, kern = c("bivnorm"))
      
      Lcv4049Iso95 <- getverticeshr(Lcv4049, percent = 95)
      Lcv4049Iso50 <- getverticeshr(Lcv4049, percent = 50)
      
      par(mfrow=c(1,2))
      plot(Lcv4049Iso95, col = "light grey", lwd = 2, border = "blue", xlim = c(574371 - 5000, 593181 + 5000), ylim = c(5745112 - 5000, 5773448 + 5000))
      plot(href4049Iso95, col = NA, add = TRUE, border = "black", lwd=3)
      plot(min4049Iso95, add = TRUE, col = NA, lwd = 3, border = "dark green")
      
      plot(Lcv4049Iso50, col = "light grey", lwd = 2, border = "blue", xlim = c(574371 - 5000, 593181 + 5000), ylim = c(5745112 - 5000, 5773448 + 5000))
      plot(href4049Iso50, col = NA, add = TRUE, border = "black", lwd=3)
      plot(min4049Iso50, add = TRUE, col = NA, lwd = 3, border = "dark green")
      
      points(elkSummer[elkSummer$IND=="4049", ], pch = ".")
      par(mfrow=c(1,1))
      
      # compare especially the difference with the Lcv estimate
      
      #######################################################################
      # now a key question - do these differences matter? It depends on ... your question/aims! 
      # Often this involves estimating HR area over a certain time interval
      # for a good discussion of sampling related issues, see Borger et al. (2006) J. Anim. Ecol.
      # hence here the functions for estimation HR area: our friend is the function kernel.area()
      # the useful feature is that it can estimate home range size for different probability 
      # (isopleth) levels
      # this is important as analysing home range size/structure at different probability levels 
      # can increase our biological understanding
      # for further discussions and an extensive application see Borger et al. (2006) Am. Nat. 
      # --> especially consider the aspects I discuss in htat paper related to obtaining different 
      # inferences with different time scales and different spatial definitions (isopleth values)
      
      HRsizeHref <- data.frame(kernel.area(allUD, percent = c(1, seq(5,95,by=5), 99)))  
      
      # to reshape into long format
      
      HRsizeHrefLong <- reshape(HRsizeHref, varying = list(names(HRsizeHref)), times = names(HRsizeHref), ids = rownames(HRsizeHref), direction = "long")
      names(HRsizeHrefLong)[3] <- "isopleth"
      names(HRsizeHrefLong)[1] <- "IND"
      names(HRsizeHrefLong)[2] <- "area"
      HRsizeHrefLong$isopleth <- as.numeric(HRsizeHrefLong$isopleth)
      HRsizeHrefLong$IND <- as.factor(HRsizeHrefLong$IND)
      head(HRsizeHrefLong)
      
      # take the values for elk4049 for Lcv
      
      HRsizeLcv <- data.frame(kernel.area(Lcv4049,percent = c(1, seq(5,95,by=5), 99)))  
      names(HRsizeLcv)[1] <- "area"
      HRsizeLcv$isopleth <- as.numeric(rownames(HRsizeLcv))
      HRsizeLcv$IND <- as.factor(rep("elk4049", nrow(HRsizeLcv)))
      
      # take the values for elk4049 for minHref
 
      HRsizeMinHref <- data.frame(kernel.area(min4049, percent = c(1, seq(5,95,by=5), 99)))  
      names(HRsizeMinHref)[1] <- "area"
      HRsizeMinHref$isopleth <- as.numeric(rownames(HRsizeMinHref))
      HRsizeMinHref$IND <- as.factor(rep("Apollo", nrow(HRsizeMinHref)))
      
      # take the values for elk4049 for plug-in 
      
      # Calculates the UD size at the different isopleths
      # note the 'approx=FALSE/TRUE part
      
      elk4049Plug_approx <- contourSizes(HRplugin, cont = c(1, seq(5,95,by=5), 99))	# approx = TRUE by default
      elk4049Plug <- contourSizes(HRplugin, cont = c(1, seq(5,95,by=5), 99), approx = FALSE)
      
      # from the documentation: If approx=FALSE, then the exact KDE is computed. Otherwise it is interpolated from 
      # an existing KDE grid. This can dramatically reduce computation time for large data sets.
      # here, however, the difference is mostly small:
      
      round((elk4049Plug_approx/elk4049Plug - 1)*100,1)
      
      # note that it reports UD values the other way round as opposed to adehabitatHR
      
      elk4049Plug
      
      # collect HR estimates into dataframe
      
      HRsizePlugIn <- data.frame(elk4049Plug)  
      names(HRsizePlugIn)[1] <- "area"
      HRsizePlugIn$isopleth <- c(1, seq(5,95,by=5), 99)
      HRsizePlugIn$IND <- as.factor(rep("4049", nrow(HRsizePlugIn)))
      
      
      
      # compare HR accumulation curves by isopleth and method for Apollo
      # for the href estimates, subset to Apollo only for convenience
      
      HRsizeHrefLong_4049 <- droplevels(subset(HRsizeHrefLong, subset = IND == "X4049"))
      
      # add PA & IV
      
      HRsizeHrefLong_4049$PA <- HRsizeHrefLong_4049$area/(max(HRsizeHrefLong_4049$area))
      HRsizeHrefLong_4049$IV <- HRsizeHrefLong_4049$isopleth/100
      
      HRsizeLcv$PA <- HRsizeLcv$area/(max(HRsizeLcv$area))
      HRsizeLcv$IV <- HRsizeLcv$isopleth/100
      
      HRsizeMinHref$PA <- HRsizeMinHref$area/(max(HRsizeMinHref$area))
      HRsizeMinHref$IV <- HRsizeMinHref$isopleth/100
      
      HRsizePlugIn$PA <- HRsizePlugIn$area/(max(HRsizePlugIn$area))
      HRsizePlugIn$IV <- HRsizePlugIn$isopleth/100
      
      
      # plot area vs. UD% curves  
      
      plot(seq(0,1,length = 50), seq(0,1,length = 50), type = "l",lty = 3)
      lines(PA ~ IV, data = HRsizeHrefLong_4049)
      lines(PA ~ IV, data = HRsizeLcv, col = 2)
      lines(PA ~ IV, data = HRsizeMinHref, col = 3)
      lines(PA ~ IV, data = HRsizePlugIn, col = 4)
      # little DIY-task -- add a proper figure legend :-)
      
      
      # identify core area as point of maximum deviation from area/%UD equality line
      
      with(HRsizeHrefLong_4049, which((IV-PA) == max(IV-PA)))
      HRsizeHrefLong_4049$IV[15]
      # should give 0.7, i.e. the 70% isopleth
      
      # do the same for Lcv, Plug-in and HrefMin estimates
      
      with(HRsizeLcv, which((IV-PA) == max(IV-PA)))
      HRsizeLcv$IV[14]
      # --> 65%
      
      with(HRsizeMinHref, which((IV-PA) == max(IV-PA)))
      HRsizeMinHref$IV[14]
      # --> 65%
      
      with(HRsizePlugIn, which((IV-PA) == max(IV-PA)))
      HRsizePlugIn$IV[14]
      # --> 65%
      
      # quantify intensity of use (Samuel, Pierce, Garton (1985) Identifying areas of concentrated use within the home range. Journal of Animal Ecology 54,711ñ719)
      
      # I = %CoreArea / %Core Area of total HR area
      # if I > 1 --> core area used more intensively than rest of HR area, and viceversa; if = 1 --> uniform use --> hence no core area by definition!
      
      HRsizeHrefLong_4049[15, 'IV'] / HRsizeHrefLong_4049[15,'PA']
      HRsizeLcv[14, 'IV'] / HRsizeLcv[14,'PA']
      HRsizeMinHref[14, 'IV'] / HRsizeMinHref[14,'PA']
      HRsizePlugIn[14, 'IV'] / HRsizePlugIn[14,'PA']
      # --> note the very similar values!
      
      
      ###########################################################################################
      ## END of HR codes 
      ###########################################################################################
      
      