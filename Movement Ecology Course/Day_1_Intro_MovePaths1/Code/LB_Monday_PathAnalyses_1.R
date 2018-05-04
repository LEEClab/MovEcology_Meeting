#####################################################################
## MovEcol Workshop, UNESP Rio Claro, Brazil, 19/02 - 23/02 - 2018 ##
##						                   ##
##               Movement Path Analysis 	                   ##
##	        Luca Borger, 19/02/2018		         	   ##
##			part 1				           ##	
#####################################################################

###################################################################################################################################################
# We use the elk data from the study "Ya Ha Tinda elk project, Banff National Park (data from Hebblewhite et al. 2008)"
# The data were donwloaded from the Movebank Data Repository (www.movebank.org) - on 18/02/2018
# The data package includes three data files and two ReadMe files:
#
# Ya Ha Tinda elk project, Banff National Park (data from Hebblewhite et al. 2008).csv				# GPS data
# Ya Ha Tinda elk project, Banff National Park (data from Hebblewhite et al. 2008)_VHF.csv			# VHF data; name changed
# Ya Ha Tinda elk project, Banff National Park (data from Hebblewhite et al. 2008)-reference-data.csv		# Individual attribute data 
# README_GPS.txt
# README_radiotransmitters.txt
#
# Save these data in your working folder - we will use these all days
# set your R working direct directory to that folder
###################################################################################################################################################

################################################################
# What we will do:
# 1. Import elk data
# 2. Inspect the data
# 3. Calculate basic movement paths statistics
# 4. We will first calculate basic stats using custom written functions, 
#    trying to maximise speed in R, also with large datasets --> use vector calculations!
# 5. We compare to the output produced by adehabitatLT
# 6. Turning angles & circular data/statistics
################################################################


# For a fresh start
  rm(list= ls())	# just to start fresh - ONLY TODAY! - successive sessions will built upon previous ones


# Load the data - we start with the GPS data first, together with the individual attribute data
  
  elksTinda <- read.csv("Ya Ha Tinda elk project, Banff National Park (data from Hebblewhite et al. 2008).csv", header = TRUE)
  elksInfo <- read.csv("Ya Ha Tinda elk project, Banff National Park (data from Hebblewhite et al. 2008)-reference-data.csv", header = TRUE)


# we inspect the data 
 
  dim(elksTinda); str(elksTinda); summary(elksTinda)
  
  dim(elksInfo); str(elksInfo); summary(elksInfo)
  
###########################################################################################################################################################
#Data Attributes
#These definitions come from the Movebank Attribute Dictionary, available at www.movebank.org/node/2381.
#
#animal ID: An individual identifier for the animal, provided by the data owner. This identifier can be a ring number, a name, the same as the associated tag ID, etc. 
#	If the data owner does not provide an Animal ID, an internal Movebank animal identifier may sometimes be shown.
#	example: 91876A, Gary
#	same as: individual-local-identifier
#
#
#event ID: An identifier for the set of information associated with each record or event in a data set. A unique event ID is assigned to every time-location or other time-measurement record in Movebank.
#	example: 6340565
#	units: none
#
#visible: Determines whether an event is visible on the Movebank Search map. Values are calculated automatically, with FALSE indicating that the event has been marked as an outlier by manually marked outlier 
#	or algorithm marked outlier. Allowed values are TRUE or FALSE.
#
#timestamp: The date and time a sensor measurement was taken.
#	example: 2008-08-14 18:31:00.000
#	format: yyyy-MM-dd HH:mm:ss.sss
#	units: UTC (Coordinated Universal Time) or GPS time, which is a few leap seconds different from UTC
#
#longitude (decimal degree): The geographic longitude of a location along an animal track as estimated by the processed sensor data. Positive values are east of the Greenwich Meridian, negative values are west of it.
#	example: -121.1761111
#	units: decimal degrees, WGS84 reference system
#	same as: location long
#
#latitude (decimal degree): The geographic longitude of a location along an animal track as estimated by the processed sensor data. Positive values are east of the Greenwich Meridian, negative values are west of it.
#	example: -121.1761111
#	units: decimal degrees, WGS84 reference system
#	same as: location lat
#	
#temperature external: The temperature measured by the tag (different from ambient temperature or internal body temperature of the animal).
#	example: 32.1
#	units: degrees Celsius
#
#GPS fix type: The type of GPS fix. 1 = no fix; 2 = 2D fix (altitude typically not valid); 3 = 3D fix (altitude typically valid).
#	example: 3
#	units: none
#
#GPS satellite count: The number of GPS satellites used to estimate the location.
#	example: 8
#	units: none
#
#height above mean sea level: The estimated height of the tag above mean sea level returned by the GPS unit. (If altitudes are calculated as height above an ellipsoid, use height above ellipsoid.)
#	example: 34
#	units: meters
#	same as: height above msl
#
#migration stage custom: The migration stage of the animal. Values are specific to the study. To use a controlled list of migration stages that can be compared across studies, use migration stage standard.
#	example: Stopover #1d
#	same as: migration stage
#THIS DATASET: 0 = resident, 1 = migrant, defined following Hebblewhite et al. 2008
#
#study-specific measurement: Values for a study-specific attribute.
#	units: undefined
#THIS DATASET: Values for GPS records contain 4 entries separated by ":"
#(1) year
#(2) season
#(3) bioseason (a value combining year/s and season)
#(4) day/night
#Values for radio transmitter records contain the year.
#
#tag technical specification: Values for a tag-specific technical attribute.
#	example: 8.31, YES
#	units: undefined
#	same as: tag tech. spec.
#THIS DATASET: Values include three entries separated by ":"
#(1) dilution of precision (DOP) measured by the GPS
#(2) Lotek GPS axis 1 activity count
#(3) Lotek GPS axis 2 activity count
#
#sensor type: The type of sensor with which data were collected. Values are chosen from a controlled list:
#	acceleration: The sensor collects acceleration data.
#	accessory measurements: The sensor collects accessory measurements, such as battery voltage.
#	Argos Doppler shift: The sensor is using Argos Doppler shift for determining position.
#	barometer: The sensor records air or water pressure.
#	bird ring: The animal is identified by a ring that has a unique ID.
#	GPS: The sensor uses GPS to find location and stores these.
#	magnetometer: The sensor records the magnetic field.
#	natural mark: The animal is identified by a natural marking.
#	radio transmitter: The sensor is a classical radio transmitter.
#	solar geolocator: The sensor collects light levels, which are used to determine position (for processed locations).
#	solar geolocator raw: The sensor collects light levels, which are used to determine position (for raw light-level measurements).
#
#taxon: The scientific name of the species on which the tag was deployed, as defined by the Integrated Taxonomic Information System (ITIS, www.itis.gov). 
#	If the species name can not be provided, this should be the lowest level taxonomic rank that can be determined and that is used in the ITIS taxonomy. 
#	Additional information can be provided using the term taxon detail.
#	example: Buteo swainsoni
#	same as: species, animal taxon, individual taxon canonical name
#
#tag ID: A unique identifier for the tag, provided by the data owner. If the data owner does not provide a tag ID, an internal Movebank tag identifier may sometimes be shown.
#	example: 2342, ptt_4532
#	same as: tag local identifier
#	
#study: The name of the study in Movebank in which data are stored.
#
#
#utm.easting / utm.northing / utm.zone
#
#
#study.timezone / study.local.timestamp
#
#
#tag ID = tag local identifier
#
#
#sex: The sex of the biological individual(s) represented in the Occurrence. Values are from a controlled list:
#	m: male
#	f: female
#
#
###########################################################################################################################################################	




######################################################################  
# we merge individual info with location data
  
  ?merge# note, in tidyverse/dplyr you will use one of left_join, right_join, inner_join, full_join, anti_join
  
  names(elksTinda)
  names(elksInfo)
  levels(elksTinda$individual.local.identifier)
  levels(elksInfo$animal.id)
  
  elkData <- merge(elksTinda, elksInfo[,c('tag.id', 'animal.id','animal.sex', 'deploy.on.date', 'deploy.off.date')], by.x = "individual.local.identifier", by.y = "animal.id", all.x = TRUE, all.y = FALSE)

# check that everything went correctly

  dim(elkData); dim(elksTinda)


######################################################################
# set Date/Time
  
  
  # convert data/time format
  # I like to always add a new column, and keep the original column as imported, so for example I can check I did everything correctly
  
  elkData$DateTime <- as.POSIXct(elkData$timestamp, format="%Y-%m-%d %H:%M:%S", tz = "UTC") 
  	# note - we ignore here the milliseconds, as there are none recorded, but if you need to handle them, ?strptime explains it:
  	# elkData$DateTime <- as.POSIXct(elkData$timestamp, format="%Y-%m-%d %H:%M:%OS", tz = "UTC") 
  	# then you can control if you want milliseconds printed or not; for example:
  	# z <- strptime("2018-02-19 09:00:00.175", "%Y-%m-%d %H:%M:%OS")
	# z # prints without fractional seconds
	# op <- options(digits.secs=3)
	# z
 	# options(op) #reset options
  	
  # check
  summary(elkData$DateTime)
  
  # useful additional time columns
  
  elkData$Date <- as.Date(elkData$DateTime)  
  elkData$week <- as.numeric(strftime(as.POSIXlt(elkData$DateTime),format="%W"))
  elkData$year <- as.numeric(strftime(as.POSIXlt(elkData$DateTime),format="%Y"))
  elkData$day <- as.numeric(strftime(as.POSIXlt(elkData$DateTime),format="%j"))
  
  summary(elkData)


# Note - we do not do it here, but for date/time in R you want to inspect the lubridate package, as it simplifies handling date/time data in R and adds many useful functionalities
# check: 
# http://lubridate.tidyverse.org/
# http://r4ds.had.co.nz/dates-and-times.html




######################################################################
# inspect data
# N individuals; N data per individual; sampling period per individual; sampling regime per individual


  levels(elkData$individual.local.identifier)
   #[1] "4049"  "GP1"   "GP2"   "GR104" "GR182" "GR193" "GR196" "YL15"  "YL2"   "YL25"  "YL29"  "YL42"  "YL5"   "YL56"  "YL58"  "YL59"  "YL64"  "YL72"  "YL73"  "YL74"  "YL77"  "YL78"  "YL79"  "YL80"  "YL86"  "YL90"  "YL91"  "YL92"  "YL93" 
   #[30] "YL94"  "YL96" 
  
  length(levels(elkData$individual.local.identifier))
   #[1] 31

  
  sort(table(elkData$individual.local.identifier))
  #
  #  GP1  YL79  YL72 GR193  YL86 GR196  YL90  YL91  YL56  YL77   YL5  YL64  4049  YL74  YL59  YL78  YL94  YL96  YL92  YL29 GR182  YL15   YL2  YL42   GP2  YL25  YL80  YL73  YL58  YL93 GR104 
  #   49   108   167   716   878  1890  2040  2216  2405  2802  2863  3184  3248  3249  3616  3929  3949  4129  4442  5405  5470  5654  5753  5861  6082  6413  7260  9615  9863 11845 13332 


  # always good to also use graphs
  
  hist(table(elkData$individual.local.identifier), xlab = "N locations", main = "Distribution of individual sample size \n N = 31 individuals")

  
# add a numeric individual ID variable and plot a sort of 'Ghant chart' for each individual
  
  
  foo <- data.frame(individual.local.identifier = levels(elkData$individual.local.identifier), numID = 1:31)

  foo2 <- merge(elkData, foo, by = "individual.local.identifier", all.x = TRUE, all.y = FALSE)

  # check
  dim(elkData); dim(foo2)
  all.equal(elkData$ event.id, foo2$ event.id)
  
  # add column
  elkData$numID <- foo2$numID

  # clean the workspace
  rm(foo, foo2)

# here now our plot of sampling times per individual
  
  plot(numID ~ DateTime, data = elkData)

# or, even better:

  library(lattice)
  
  xyplot(numID ~ DateTime, data = elkData, groups = individual.local.identifier, auto.key=list(columns  =  8))

# thus depending on the analysis it will be necessary to carefully think about which individuals and sampling times to include 

######################################################################  
# to order a dataframe, by individual and time - it is important to always make sure your data are perfectly ordered
# as before, I usually prefer to create a new object and leave the original untouched, so I can always go back easily if in doubt

  elkDataSrt <- elkData[order(elkData$individual.local.identifier,elkData$DateTime),]
  

# quick way to check if any consecutive location has been taken at exactly the same time (hence likely a duplicated record)

  which(elkDataSrt$DateTime[1:(nrow(elkDataSrt)-1)] == elkDataSrt$DateTime[2:(nrow(elkDataSrt))])

# there are indeed a few - let us check them

  duplicatedLocs <- which(elkDataSrt$DateTime[1:(nrow(elkDataSrt)-1)] == elkDataSrt$DateTime[2:(nrow(elkDataSrt))])

  elkDataSrt[(duplicatedLocs[1]-2):(duplicatedLocs[1]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  # --> record 3 of the above is the one with 4 satellites only, but plotting suggests the successive one to be the one to remove:
  foo <- elkDataSrt[(duplicatedLocs[1]-2):(duplicatedLocs[1]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  plot(utm.northing ~ utm.easting, data = foo)
  rm(foo)
  # --> remove record on line 129082
  
  elkDataSrt[(duplicatedLocs[2]-2):(duplicatedLocs[2]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  foo <- elkDataSrt[(duplicatedLocs[2]-2):(duplicatedLocs[2]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  plot(utm.northing ~ utm.easting, data = foo)
  rm(foo)
  # --> remove record 129155
  
  elkDataSrt[(duplicatedLocs[3]-2):(duplicatedLocs[3]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  foo <- elkDataSrt[(duplicatedLocs[3]-2):(duplicatedLocs[3]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  plot(utm.northing ~ utm.easting, data = foo)
  rm(foo)
  # --> remove record 129256 
  
  elkDataSrt[(duplicatedLocs[4]-2):(duplicatedLocs[4]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  foo <- elkDataSrt[(duplicatedLocs[4]-2):(duplicatedLocs[4]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  plot(utm.northing ~ utm.easting, data = foo)
  rm(foo)
  # --> remove record 129357 
  
  elkDataSrt[(duplicatedLocs[5]-2):(duplicatedLocs[5]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  foo <- elkDataSrt[(duplicatedLocs[5]-2):(duplicatedLocs[5]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  plot(utm.northing ~ utm.easting, data = foo)
  rm(foo)
  # --> remove record 129458  
  
  elkDataSrt[(duplicatedLocs[6]-2):(duplicatedLocs[6]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  foo <- elkDataSrt[(duplicatedLocs[6]-2):(duplicatedLocs[6]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  plot(utm.northing ~ utm.easting, data = foo)
  rm(foo)
  # --> remove record 129559  

  elkDataSrt[(duplicatedLocs[7]-2):(duplicatedLocs[7]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  foo <- elkDataSrt[(duplicatedLocs[7]-2):(duplicatedLocs[7]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  plot(utm.northing ~ utm.easting, data = foo)
  rm(foo)
  # --> remove record 129660   

  elkDataSrt[(duplicatedLocs[8]-2):(duplicatedLocs[8]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  foo <- elkDataSrt[(duplicatedLocs[8]-2):(duplicatedLocs[8]+2), c('individual.local.identifier', 'DateTime', 'location.long', 'location.lat', 'utm.easting', 'utm.northing', 'visible', 'gps.satellite.count')]
  plot(utm.northing ~ utm.easting, data = foo)
  rm(foo)
  # --> remove record 129761   

# records to remove:
  
  outliers <- duplicatedLocs + 1
  # [1] 129082 129155 129256 129357 129458 129559 129660 129761


 
###########################################
# outliers removed
  
  elkDataSrtB <- elkDataSrt[-outliers,]
  dim(elkDataSrt)[1]-dim(elkDataSrtB)[1]
   # [1] 8 --> OK!

# clean up
  
  rm(duplicatedLocs, outliers)

######################################################################
# it can come handy to add an identifier variable, identifying the first location of each individual - let us call this 'firstRec'

  foo <- which(elkDataSrtB$individual.local.identifier[1:(nrow(elkDataSrtB)-1)] != elkDataSrtB$individual.local.identifier[2:(nrow(elkDataSrtB))])

  elkDataSrtB$firstRec <- rep(0,nrow(elkDataSrtB))

  elkDataSrtB$firstRec[foo+1] <- 1

  elkDataSrtB$firstRec[1] <- 1

# let us check if this is correct

  length(unique(elkDataSrtB$individual.local.identifier)) # count N individuals
  sum(elkDataSrtB$firstRec)	# sum = 16, looks fine!
  elkDataSrtB[sort(c(foo-1,foo,foo+1)),c('individual.local.identifier','DateTime','firstRec')] # first records seem correctly identified
  rm(foo) 	# keep workspace clean




######################################################################
######################################################################
# step lengths - Euclidean distance formula, as we area dealing with projected coordinates
# note the way to use vector calculations -- works well even with large datasets

  foo <- sqrt((elkDataSrtB$utm.easting[1:(nrow(elkDataSrtB)-1)] - elkDataSrtB$utm.easting[2:(nrow(elkDataSrtB))])^2 + (elkDataSrtB$utm.northing[1:(nrow(elkDataSrtB)-1)] - elkDataSrtB$utm.northing[2:(nrow(elkDataSrtB))])^2)

# let us check what we got
  summary(foo)	# OK, all numeric values, no NAs or other surprises
  elkDataSrtB$SL <- c(NA, foo)
  
  rm(foo)

# now think about the large max step length value and why we do the next function below
  elkDataSrtB$SL <- ifelse(elkDataSrtB$firstRec == 1, NA, elkDataSrtB$SL)
  
summary(elkDataSrtB$SL) # notice the difference

##########################################################################

##########################################################################  
 #if the coordinates are non-projected, and if you deal with large distances, you need to estimate great circle distances
 #various packages and approaches exist - here with the fossil library - let us redo the calculations and compare to step length estimates using UTM & Euclidean distance

  library(fossil)
  # ?deg.dist
  # syntax: deg.dist(long1, lat1, long2, lat2)
  # notice the syntax - provide longitudes + lat of the two points (in our case actually a vector of points), then proceed as above
  # it provides distances in km, hence here we transform it into meter scale

  foo <- deg.dist(elkDataSrtB$location.long[1:(nrow(elkDataSrtB)-1)], elkDataSrtB$location.lat[1:(nrow(elkDataSrtB)-1)], elkDataSrtB$location.long[2:nrow(elkDataSrtB)], elkDataSrtB$location.lat[2:nrow(elkDataSrtB)])
  foo <- foo*1000
  
  # add to the dataframe, set first value to NA for each individual, then compare the two step length calculations
  
  elkDataSrtB$SLhvs <- c(NA, foo)
  elkDataSrtB$SLhvs <- ifelse(elkDataSrtB$firstRec == 1, NA, elkDataSrtB$SLhvs)

  summary(elkDataSrtB$SLhvs)
  summary(elkDataSrtB$SL)
  plot(elkDataSrtB$SL, elkDataSrtB$SLhvs)
  hist(elkDataSrtB$SL - elkDataSrtB$SLhvs)
  summary(elkDataSrtB$SL - elkDataSrtB$SLhvs)
  length(which(elkDataSrtB$SL - elkDataSrtB$SLhvs > 10))
  
  # [1] 125 --> not too bad out of >138000 locations
  # might want to inspect further which ones these are (maybe border between 2 UTM zones?)

##########################################################################
##########################################################################
# time between steps
# ?difftime
# syntax: difftime(then,now,units="secs")

  foo <- difftime(elkDataSrtB$DateTime[2:nrow(elkDataSrtB)], elkDataSrtB$DateTime[1:(nrow(elkDataSrtB)-1)], units = "secs")
  foo <- c(NA, foo)
  summary(as.numeric(foo))
  foo <- ifelse(elkDataSrtB$firstRec == 1, NA, foo)
  summary(as.numeric(foo))
  elkDataSrtB$dt <- foo
  rm(foo)  

##########################################################################
# investigate distribution of step length and time lags between steps

  hist(elkDataSrtB$dt); summary(elkDataSrtB$dt)		
  hist(elkDataSrtB[elkDataSrtB$dt < 8000,'dt'])		# think about the sampling regime -- 7200s = 2h; 3600s = 1h; 900s = 15min; likely a resampling and/or imputation will be required for analyses
  hist(elkDataSrtB[elkDataSrtB$dt < 4500 & elkDataSrtB$dt > 2000,'dt'])



# if the relationship between step length and time is linear, we can just model the velocity/speed; if not, we need to think a bit more about it

  library(mgcv)
  plot(gam(SL ~ s(dt), data = elkDataSrtB))
  plot(gam(SL ~ s(dt), data = elkDataSrtB, subset = dt < 8000))
  plot(gam(SL ~ s(dt, by = individual.local.identifier), data = elkDataSrtB, subset = dt < 8000), pages = 1)		# fit a different smooth for each individual, plot all on one page
  plot(gam(SL ~ s(dt, by = individual.local.identifier), data = elkDataSrtB, subset = dt < 8000), pages = 1, scale = 0)	# allow also a different y axis for each plot

# who knows what GAM models are?
# can you explain what the different codes do?




##########################################################################
# Turn angle calculation (modified after code from Simon Benhamou)

# Basic operations:
##################################################################################  
# direction1 <- atan2(y2-y1,x2-x1)
# direction2 <- atan2(y3-y2,x3-x2)
# angle <- direction2-direction1
# from: ?atan2: The arc-tangent of two arguments atan2(y, x) returns the angle 
# between the x-axis and the vector from the origin to (x, y), i.e., for positive 
# arguments atan2(y, x) == atan(y/x).
##################################################################################

# let us check this:
  
  # 90 degree turn right turn
  direction1 <- atan2(-1-0,0-0)
  direction2 <- atan2(0-0,1-0)
  angle <- direction2-direction1
  angle
  # in degrees
  angle*180/pi
  # correct
  
  # 90 degree left turn
    direction1 <- atan2(-1-0,0-0)
    direction2 <- atan2(0-0,-1-0)
    angle <- direction2-direction1
    angle
    # in degrees
    angle*180/pi
  # why 270 degrees? Because in reality this gives the external angle
  # to obtain the internal one, need to do pi - angle (or 180 - angle in degrees)
    
    180 - angle*180/pi
  
  # 0 degree (straight on)
      direction1 <- atan2(-1-0,0-0)
      direction2 <- atan2(1-0,0-0)
      angle <- direction2-direction1
      angle
      # in degrees
      180 - angle*180/pi

# Do yourself:

  # 180 degree turn
      direction1 <- atan2(-1-0,0-0)#y
      direction2 <- atan2(-1-0,0-0)#x
      angle <- direction2-direction1
      angle
      180 - angle*180/pi

  # +45 degree turn (right turn)
      
      
      
      

##########################################################################
# the fossil library provides also handy functions to estimate bearings, and hence turns:

  #?earth.bear
  #syntax is: earth.bear(long1, lat1, long2, lat2)

 foo <- earth.bear(elkDataSrtB$location.long[1:(nrow(elkDataSrtB)-1)], elkDataSrtB$location.lat[1:(nrow(elkDataSrtB)-1)], elkDataSrtB$location.long[2:nrow(elkDataSrtB)], elkDataSrtB$location.lat[2:nrow(elkDataSrtB)])
 foo <- c(NA, foo)
 summary(foo)
 foo <- ifelse(elkDataSrtB$firstRec == 1, NA, foo)
 summary(foo)
 elkDataSrtB$brn <- foo
 # plot for each individuals
 histogram(~brn|individual.local.identifier, data = elkDataSrtB)


 # ta = bearing2 - bearing1  				# note - a turning angle is always a value between 3 locations, thus first two values for example will be = NA

 ta <- foo[2:length(foo)] - foo[1:(length(foo)-1)]
 ta <- c(NA,ta)
 elkDataSrtB$ta <- ta
 rm(foo,ta)
 histogram(~ta|individual.local.identifier, data = elkDataSrtB)		# think - what does this indicate, prevalence of turns or of straight movements?
 
# to-do: is this the external or internal angle? Think and discuss
 
#############################################


  xyplot(log(SL + 1) ~ external.temperature|individual.local.identifier, data = elkDataSrtB, subset = dt < 4000 & dt > 3200)
  xyplot(log(SL + 1) ~ external.temperature|individual.local.identifier, data = elkDataSrtB, subset = dt < 7500 & dt > 7000)
  plot(gam(log(SL + 1) ~ s(external.temperature, by = individual.local.identifier), data = elkDataSrtB, subset = dt < 7500 & dt > 7000), pages = 1, scale = 0)






################################################################################################################################################################
#######################################
# adehabitatLT

  library(adehabitatLT)

# convert to ltraj object (type II)

  Elktraj <- as.ltraj(xy = elkDataSrtB[,c("utm.easting","utm.northing")], date = elkDataSrtB$DateTime, id = elkDataSrtB$individual.local.identifier)
  Elktraj
  str(Elktraj)	# understand the structure as list of dataframes, with basic movement parameters calculated for each individual
# re the latter, note that that dx,dy,dist take the units of the coordinates x,y (thus metres here) and abs.angle, rel.angle are in radians.

  plot(Elktraj)

# convert from ltraj to data.frame and back

  ElktrajDF <- ld(Elktraj)
  head(ElktrajDF)

# to convert back to ltraj use function dl()
# otherwise you need to use functions to access lists - compare these two codes

  summary(Elktraj)
  summary(Elktraj[[1]])

# lapply() and sapply() are your friends here -- check them!

# back to the DF object created -- adehabitatLT has estimated a bunch of movement path statistics

  head(ElktrajDF)


##################################
# Turn Angles
# if you need a refresher on degrees and radians: http://www.mathwarehouse.com/trigonometry/radians/convert-degee-to-radians.php
# see also ?cos  

  library(CircStats)

# histograms

par(mfrow=c(2,2))
indvs <- levels(ElktrajDF$id)
for(i in 1:4){
     hist((ElktrajDF[ElktrajDF$id==indvs[i] & ElktrajDF$dt < 8000,'rel.angle']),main=indvs[i], xlab = "Turning angle (radians)")
}

# the same, but converting radians to degrees

par(mfrow=c(2,2))
indvs <- levels(ElktrajDF$id)
for(i in 1:4){
     hist(na.omit(ElktrajDF[ElktrajDF$id==indvs[i] & ElktrajDF$dt < 8000,'rel.angle'])*180/pi,main=indvs[i], xlab = "Turning angle (degrees)")
}


# Rose diagram

par(mfrow=c(2,2))
indvs <- levels(ElktrajDF$id)
for(i in 1:4){
     rose.diag(na.omit(ElktrajDF[ElktrajDF$id==indvs[i] & ElktrajDF$dt<8000,'rel.angle'])*180/pi,bins=24,prop=2.5,main=indvs[i])
}
# play around with the 'bins' and 'prop' values
# discuss: uniform vs. non-uniform distribution of turn angles?


# summary statistics for circular data (mean direction and sample mean resultant length --> measure of concentration of unimodal circular data
# discuss if data are unimodal --> see graphs

CircSummaries <- list()
for(i in 1:31){
     CircSummaries[[i]] <- circ.summary(na.omit(ElktrajDF[ElktrajDF$id==indvs[i] & ElktrajDF$dt<12000000,'rel.angle'])*180/pi)
}
names(CircSummaries) <- levels(ElktrajDF$id)
CircSummaries

# handy code to turn this into a dataframe

  CircSummariesDF <- setNames(do.call(rbind.data.frame, CircSummaries), c("n", "mean.dir", "rho"))

# if you want to add the individual ID

  CircSummariesDF$IND <- as.factor(rownames(CircSummariesDF))
  

# if you want to transform it into degrees and add it to the dataframe

  CircSummariesDF$mean.dir.degrees <- CircSummariesDF$mean.dir*180/pi

# Rayleigh tests of uniformity
# from the documentation: 
# Performs a Rayleigh test of uniformity, assessing the significance of the mean resultant length. 
# The alternative hypothesis is a unimodal distribution with unknown mean direction and unknown mean resultant length.
# Returns a list with two components: the mean resultant length, r.bar, and the p-value of the test statistic, p.value.
# degree: logical flag: if TRUE, data is assumed to be measured in degrees rather than radians. Default is FALSE.


RayleighTests <- list()
indvs <- levels(ElktrajDF$id)
for(i in 1:31){
     RayleighTests[[i]] <- r.test(na.omit(ElktrajDF[ElktrajDF$id==indvs[i] & ElktrajDF$dt<8000,'rel.angle']), degree=FALSE)
}
names(RayleighTests) <- levels(ElktrajDF$id)
RayleighTests

RayleighTestsDF <- setNames(do.call(rbind.data.frame, RayleighTests), c("r.bar", "p.value"))


# mean cos() and sin() are often useful to calculate

meanCosF <- function(x) {
     meanCos <- mean(cos(x), na.rm=TRUE)
     return(meanCos)
} 

with(ElktrajDF, tapply(rel.angle, id, FUN = meanCosF))*180/pi



##########################################################################################################
# cuttting burst into several segments

# define function to identify step length duration above a certain value; here, we use 4 hours

foo <- function(dt) {
     return(dt > (3600*4))
}

# might take quite some time - hence commented out here
#Elktraj2 <- cutltraj(Elktraj, "foo(dt)", nextr = TRUE)
# note the warning message regarding a large number of relocations deleted

  head( Elktraj2)
  
# note bursts start int he evening and end in the morning


# adehabitatLT contains then convenient ways to select specific individuals, bursts, etc.
# also, functions to insert missing locations to create regular trajectories
# similarly, functions to test if the locations are missing at random --> see runsNAltraj()
# also functions to rediscretize a trajectory so that all steps are of equal duration: redisltraj()
# we will not cover these, also because we are aiming to cover continuous time RW approaches which do not require regular trajectories
# see also the opportunity to store metadata on the data in the 'infolocs' slot
# see the very useful package handbook: https://cran.r-project.org/web/packages/adehabitatLT/vignettes/adehabitatLT.pdf

# adehabitatLT is very convenient for plotting the movement statistics over time for each individual:

  plotltr(Elktraj, "dist")  
  plotltr(Elktraj, "dt") 
  plotltr(Elktraj, "R2n") 

# also useful can be the function to calculate and plot the statistics with a sliding window over time: sliwinltr() # not shown here; can take time
# similary, the function for testing/plotting autocorrelation of movement parameters -- but these work only on regular trajectories of type I
# there are also functions to create such regular trajectories
# even more so, functions to rasterize a movement path or create null models of animal trajectories
#

# write DF to file, for next practicals
  
  write.csv(elkDataSrtB, file = "elkDataGpsSrtB.csv")
  
############################################
# END
############################################

############################################
# Home work - or if you finished early
# Apply the above to the elk VHF data
############################################
  
  library(MASS)
  library(spatstat)
  library(splancs)
  library(MCMCpack)
  library(nlme)
  library(lattice)
  library(adehabitatHR)
  library(adehabitatLT)
  library(sp)
  library(ks)
  library(lme4)
  library(mgcv)
  library(CircStats)
  library(bcpa)
  library(rgbif)
  library(data.table)
  