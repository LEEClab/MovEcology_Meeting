#####################################################################
## MovEcol Workshop, UNESP Rio Claro, Brazil, 19/02 - 23/02 - 2018 ##
##						                   ##
##               Squared Displacement /Analysis 	                   	   ##
##	        Luca Borger, 22/02/2018		         	   ##
##							           ##	
#####################################################################
library(nlme)
library (lattice)
###########################################
# we continue with the dataset: elkDataSrtB 
  
  elkDataSrtB <- read.csv("elkDataGpsSrtB.csv")
  elkDataSrtB$Date <- as.Date(elkDataSrtB$Date, form = "%Y-%m-%d")
  elkDataSrtB$DateTime <- as.POSIXct(elkDataSrtB$timestamp, format="%Y-%m-%d %H:%M:%S", tz = "UTC") 
  
######################################
# add additional time covariates

  elkDataSrtB$yrDay <- as.numeric(strftime(as.POSIXlt(elkDataSrtB$DateTime),format="%j"))
  elkDataSrtB$jday <- round(as.numeric(julian(elkDataSrtB$Date)),0)
  elkDataSrtB$serialDay <- elkDataSrtB$jday - min(elkDataSrtB$jday) # i.e. minus the value for first location


##########################################################################################################################################################################
# transform dataframe to a list, then use sapply(), to speed up the calculations of NSD
# I previously used to calculate NSD for each individual using a for loop -- it works, but can take time with large datasets
# adehabitatLT calculates NSD, but does also a bunch of other stuff and with large datasets I have had memory issues in the past
# and having your own function, for your specific task only, gives you full control
# so, here the function, applied to the elk data, followed by a performance test using a simulated large dataset (2000 animals, with 1000 locations each)
# a good entry point to apply/lapply/sapply ...:
# https://www.r-bloggers.com/apply-lapply-rapply-sapply-functions-in-r/


  elkDataSrtBLS <- split(elkDataSrtB, list(elkDataSrtB$individual.local.identifier))

  NSD <- sapply(elkDataSrtBLS, function(x) {
           
    nsd <- (x$utm.easting - x$utm.easting[1])^2 + (x$utm.northing - x$utm.northing[1])^2
  })
#### NSD es la distancia ecluidiana entre el primero y segundo  segundo y tercero
# check the values obtained

  sapply(NSD,summary)
  apply(sapply(NSD,summary),2,sqrt)

# in the case of Great circle distance, need to load the fossil() library and use this line, instead of the Euclidean distance measure:

# deg.dist(x$LONGITUDE, x$LATITUDE, x$LONGITUDE[1], x$LATITUDE[1]))^2

#####################################################################  
# now simulate a huge dataset and test the speed of the NSD function

###Aqui lo que hace es crear columnas de longitud y latitud las function (i) puede ser envuelto en cada palabra dentro del mismo 
  n <- 1000
  rndAnim <- lapply(1:2000, function(i) {
       LONGITUDE <- rnorm(n)
       LATITUDE <- rnorm(n)
       data.frame(LONGITUDE = LONGITUDE, LATITUDE = LATITUDE)
  })

  str(rndAnim[[1]])
  
# do not use 'summary()' on this large object
  ?system.time
  
  system.time(rnNSD <- sapply(rndAnim, function(x) {
       nsd <- (x$LONGITUDE - x$LONGITUDE[1])^2 + (x$LATITUDE - x$LATITUDE[1])^2
  })
  )
# --> 0.31 seconds only!
################################################################################################  

######################
# clean the workspace

rm(rndAnim, rnNSD)

#####################################################################
## now add back NSD values to the dataframe
## given how we have ordered the dataframes and done the operations so far, elkDataSrtB & unlist(NSD) are ordered in the same way
## let us check it

# transform back into a dataframe 

NSDdf <- data.frame(  IND = as.factor(rep(names(sapply(NSD,function(x) length(x))), as.numeric(sapply(NSD,function(x) length(x))))),
                      NSD = as.numeric(unlist(NSD))
)

  all.equal(elkDataSrtB$individual.local.identifier, NSDdf$IND)
# --> TRUE

## hence just unlist() the NSD values and add them to the existing elkDataSrtB dataframe

  elkDataSrtB$NSD = as.numeric(unlist(NSD))


#######
## NSDdf is not needed, hence we can delete it  
  rm(NSDdf)


############################################################################
# calculate days from first location for each indivINDual, then add to the DF


  nDays <- sapply(elkDataSrtBLS, function(x) {
       nday <- (x$jday - min(x$jday))
  })

  elkDataSrtB$nDays = as.numeric(unlist(nDays))
  

  rm(nDays)


####################################################################################
# plot the NSD patterns 
###Datos del displacement de verdad

 ?xyplot
   xyplot(NSD~nDays|individual.local.identifier, data = elkDataSrtB, type=c("l"))

####################################################################################
# add a column 'IND' as individual identifier with a shorter name

  elkDataSrtB$IND <- elkDataSrtB$individual.local.identifier


####################################################################################
# take out the within-day variability by taking the average NSD per day

  elkDataSrtC <- aggregate(NSD ~ IND + nDays, data = elkDataSrtB, FUN = mean)

  xyplot(NSD~nDays|IND, data = elkDataSrtC, type=c("l"))


# select only individuals monitored over approx the entire year
  
   elks <- c("YL96","YL92","YL93","YL73","YL74","YL77", "YL78", "YL80", "YL5", "YL58", "YL59", "YL64", "YL15", "YL2", "YL25", "YL29", "YL42", "4049", "GP2", "GR104", "GR182", "GR193")
   
   elkDataSrtD <- subset(elkDataSrtC, elkDataSrtC$IND %in% elks)
   
   elkDataSrtD <- droplevels(elkDataSrtD)
   
   xyplot(NSD~nDays|IND, data = elkDataSrtD, type=c("l"))

   
   
#####################################################################################
########################################################################################################################################  
# start MSD models - first with daily-scale data

library(nlme)

##############################################################################
## Control parameters in nlme
#  This may not be the best strategy because nlme has iterations within 
#  iterations and you are changing the maximum number of iterations for 
#  both types.  I'll use Don Watts' terms of "innerations" and 
#  "outerations".  The 'maxiter' parameter is the maximum number of 
#  "outerations".  Increasing that parameter is fine.  The other parameters 
#  control different aspects of the "innerations" which consist of a 
#  penalized nonlinear least squares (pnls) step followed by optimization 
#  of a locally linear mixed model (llmm).  The llmm step itself has two 
#  stages: EM iterations and general optimization.
#  
#  The 'niterEM' parameter is the number of iterations that will be done on 
#  the first llmm problem.  There is no convergence criterion for the EM 
#  iterations.  If you set niterEM = 1000 then it will do exactly 1000 
#  iterations.  This will do no harm but it may be a waste of nDays for two 
#  reasons - EM iterations are slow to converge and these iterations apply 
#  to the first llmm problem, which may be changed radically by the next 
#  pnls step so you are spending nDays getting an accurate answer to the 
#  wrong problem.  The default is 25.  I somenDayss increase it to 100 but 
#  rarely beyond that.
#  
#  The 'msMaxIter' parameter is the maximum number of iterations allowed in 
#  the general optimization stage of each llmm step.  That could be 
#  increased but probably not beyond a couple of hundred.  If the optimizer 
#  can't converge after, say, 500 iterations then the llmm step is in trouble.
#  
#  The 'pnlsMaxIter' parameter is the maximum number of iterations in each 
#  pnls step.  The default is 7.  It is not a good idea to make this very 
#  large because getting an accurate answer to a pnls step, especially the 
#  first few, is another case of worrying about getting an accurate answer 
#  to the wrong problem.  The idea is that you only take a few steps in the 
#  early pnls problems, then switch to the llmm problem which will provide 
#  a different pnls problem to solve.
# if you increase minScale then the pnls 
# steps will terminate earlier.  It is unlikely that decreasing this 
# parameter will help ensure convergence.
# The overall convergence is declared when the relative change in the 
# parameters after a pnls/llmm pair is below the convergence criterion.
# The pnlsTol applies only to the pnls step.
# The llmm problem is always 
# parameterized in terms of the relative precision matrices, which are the 
# inverses of the relative variance-covariance matrices.  A variance of 
# zero corresponds to an infinite precision and cannot be represented by 
# finite parameter values.
##############################################################################


  library(nlme)
  

#### nomad 
#-> D = diffusion coef > 4*D*hours gives a parameter of space use
#####################################################################################################
all.linearB <- nlme(NSD ~ 4*D*nDays,
                    data = elkDataSrtD,
                    fixed = D ~ 1,
                    random = D ~ 1,
                    groups = ~ IND,
                    start = c(D = 100))

# nomad
#####################################################################################################
# all.powerC <- nlme(NSD ~ D*nDays^b,
#                   data = elkDataSrtD,
#                   fixed = D + b ~ 1,
#                   random = D ~ 1,
#                   groups = ~ IND,
#                   control = nlmeControl(pnlsMaxIter=20, niterEM = 25, returnObject=TRUE),
#                   start = c(D = 1000, b = 1))

# nomad
#####################################################################################################
#all.powerC2 <- nlme(NSD ~ nDays^b,
#                    data = elkDataSrtD,
#                    fixed = b ~ 1,
#                    random = b ~ 1,
#                    groups = ~ IND,
#                    start = c(b = 1))
#

## 
#########################################
all.powerC3 <- nlme(NSD ~ D*nDays^b,
                    data = elkDataSrtD,
                    fixed = D + b ~ 1,
                    random = D + b ~ 1,
                    groups = ~ IND,                           
                    verbose = TRUE,
                    control = nlmeControl(pnlsMaxIter=7, niterEM = 50, returnObject=TRUE),
                    start = c(D = 4619946, b = 1.5))


#### null model
#################################
null.HRmod <- nlme(NSD ~ A,
                   data = elkDataSrtD,
                   fixed = A ~ 1,
                   random = A ~ 1,
                   groups = ~ IND,
                   start = c(A=507900000))



###Dispersal mode with only 1 random parameter (asymptote)
##########################
ranef1.Dispmod <- nlme(NSD ~ Asym/(1 + exp((xmid-nDays)/scal)),
                       data = elkDataSrtD,
                       fixed = Asym + xmid + scal ~ 1,
                       random = Asym ~ 1,
                       groups = ~ IND,
                       na.action = na.exclude,
                       start = c(Asym = summary(null.HRmod)$tTable[1], xmid = 1, scal = 1),verbose=F)


### 
### 
########################################################
ranef2.Dispmod <- nlme(NSD ~ Asym/(1 + exp((xmid-nDays)/scal)),
                       data = elkDataSrtD,
                       fixed = Asym + xmid + scal ~ 1,
                       random = Asym + xmid ~ 1,
                       groups = ~ IND,
                       control = nlmeControl(pnlsMaxIter=20, niterEM = 25, returnObject=TRUE),
                       start = c(Asym = summary(ranef1.Dispmod)$tTable[1], xmid = summary(ranef1.Dispmod)$tTable[2], scal = summary(ranef1.Dispmod)$tTable[3]),
                       verbose=T)



##
########################################################
full.Dispmod <- nlme(NSD ~ Asym/(1 + exp((xmid-nDays)/scal)),
                     data = elkDataSrtD,
                     fixed = Asym + xmid + scal ~ 1,
                     random = Asym + xmid + scal ~ 1,
                     groups = ~ IND,
                     na.action = na.exclude,
                     control = nlmeControl(pnlsMaxIter=20, niterEM = 25, returnObject=TRUE),
                     start = c(Asym = summary(ranef1.Dispmod)$tTable[1], xmid = summary(ranef1.Dispmod)$tTable[2], scal = summary(ranef1.Dispmod)$tTable[3]),
                     verbose = T)



### HR model
########################################################

asym.HRmod <- nlme(NSD ~ Asym*(1 - exp(lrc*nDays)),
                   data 	= elkDataSrtD,
                   fixed 	= Asym + lrc ~ 1,
                   random 	= Asym ~ 1,
                   groups 	= ~ IND,
                   start 	= c(Asym = summary(null.HRmod)$tTable[1], lrc = -0.002))



### 
############################################################################  
full.HRmod <- nlme(NSD ~ Asym*(1 - exp(lrc*nDays)),
                   data 	= elkDataSrtD,
                   fixed 	= Asym + lrc ~ 1,
                   random 	= Asym + lrc ~ 1,
                   control = nlmeControl(pnlsMaxIter=20, niterEM = 25, returnObject=TRUE),
                   groups 	= ~ IND,
                   verbose = TRUE,
                   start 	= c(Asym = summary(asym.HRmod)$tTable[1], lrc = -0.02))




# MIGRATION MODEL
Asym.Migrmod <- nlme(NSD ~ Asym/(1+exp((xmidA-nDays)/scal1)) + (-Asym/(1+exp((xmidB-nDays)/scal2))),
                     data = elkDataSrtD,
                     na.action = na.omit,
                     fixed = Asym + xmidA + xmidB + scal1 + scal2 ~ 1,
                     random = Asym ~ 1,
                     groups = ~ IND,
                     start = c(Asym = summary(ranef1.Dispmod)$tTable[1], xmidA = 58, xmidB = 210,  scal1 = 1.9, scal2 = 2.5), verbose=T) 

###

ranef2.Migrmod <- nlme(NSD ~ Asym/(1+exp((xmidA-nDays)/scal1)) + (-Asym/(1+exp((xmidB-nDays)/scal2))),
                     data = elkDataSrtD,
                     na.action = na.omit,
                     fixed = Asym + xmidA + xmidB + scal1 + scal2 ~ 1,
                     random = Asym + xmidA + xmidB ~ 1,
                     groups = ~ IND,
                     start = c(Asym = summary(ranef1.Dispmod)$tTable[1], xmidA = 58, xmidB = 210,  scal1 = 1.9, scal2 = 2.5), verbose=T) 

###

 full.Migrmod <- nlme(NSD ~ Asym/(1+exp((xmidA-nDays)/scal1)) + (-Asym/(1+exp((xmidB-nDays)/scal2))),
                      data = elkDataSrtD,
                      na.action = na.omit,
                      fixed = Asym + xmidA + xmidB + scal1 + scal2 ~ 1,
                      random = Asym + xmidA + xmidB + scal1 + scal2 ~ 1,
                      groups = ~ IND,
                      control = nlmeControl(pnlsMaxIter=20, returnObject=TRUE),
                      start = c(Asym = summary(ranef2.Migrmod)$tTable[1], xmidA = summary(ranef2.Migrmod)$tTable[2], xmidB = summary(ranef2.Migrmod)$tTable[3],  scal1 = 1.9, scal2 = 2.5), verbose=T) 







########################################################
########################################################
### GOF indiv level
### Goodness Of Fit evaluated at the individual level

NullMod        <- null.HRmod
Dispmod1       <- ranef1.Dispmod 
Dispmod2       <- ranef2.Dispmod
Dispmod3       <- full.Dispmod 
nomadMod1      <- all.linearB
#nomadMod2     <- all.powerC
#nomadMod3     <- all.powerC2
nomadMod4      <- all.powerC3
HRmodA 	       <- asym.HRmod
HRmodB 	       <- full.HRmod
Migrmod1	<- Asym.Migrmod
Migrmod2	<- ranef2.Migrmod
Migrmod3	<- full.Migrmod


# GOF function 

CCidFUN <- function(LocData, MoveMod, ...) {1 - (sum((LocData[which(LocData$IND == levels(LocData$IND)[k]),'NSD'] -
                                                           fitted(MoveMod)[which(LocData$IND == levels(LocData$IND)[k])])^2)) / (
                                                                (sum((LocData[which(LocData$IND == levels(LocData$IND)[k]),'NSD'] -
                                                                           mean(LocData[which(LocData$IND == levels(LocData$IND)[k]),'NSD']))^2)) +
                                                                     (sum((fitted(MoveMod)[which(LocData$IND == levels(LocData$IND)[k])] -
                                                                                mean(fitted(MoveMod)[which(LocData$IND == levels(LocData$IND)[k])]))^2)) +
                                                                     (length(LocData[which(LocData$IND == levels(LocData$IND)[k]),'NSD'])*
                                                                           ((mean(LocData[which(LocData$IND==levels(LocData$IND)[k]),'NSD']) -
                                                                                  mean(fitted(MoveMod)[which(LocData$IND == levels(LocData$IND)[k])]))^2)))
}

### CC1-idlev
########################################################

###
CC1id.NullMod <- numeric(length(levels(elkDataSrtC$IND)))
for(k in 1:length(levels(elkDataSrtC$IND))) {
     CC1id.NullMod[k] <- CCidFUN(elkDataSrtC, NullMod)
}

###
CC1id.Dispmod1 <- numeric(length(levels(elkDataSrtC$IND)))
for(k in 1:length(levels(elkDataSrtC$IND))) {
     CC1id.Dispmod1[k] <- CCidFUN(elkDataSrtC,Dispmod1)
}

###
CC1id.Dispmod2 <- numeric(length(levels(elkDataSrtC$IND)))
for(k in 1:length(levels(elkDataSrtC$IND))) {  
     CC1id.Dispmod2[k] <- CCidFUN(elkDataSrtC,Dispmod2)
}

CC1id.Dispmod3 <- numeric(length(levels(elkDataSrtC$IND)))
for(k in 1:length(levels(elkDataSrtC$IND))) {  
     CC1id.Dispmod3[k] <- CCidFUN(elkDataSrtC,Dispmod3)
}

###
CC1id.nomadMod1 <- numeric(length(levels(elkDataSrtC$IND)))
for(k in 1:length(levels(elkDataSrtC$IND))) {  
     CC1id.nomadMod1[k] <- CCidFUN(elkDataSrtC,nomadMod1)
}

###
#CC1id.nomadMod2 <- numeric(length(levels(elkDataSrtC$IND)))
#for(k in 1:length(levels(elkDataSrtC$IND))) {  
#     CC1id.nomadMod2[k] <- CCidFUN(elkDataSrtC,nomadMod2)
#}

#CC1id.nomadMod3 <- numeric(length(levels(elkDataSrtC$IND)))
#for(k in 1:length(levels(elkDataSrtC$IND))) {  
#     CC1id.nomadMod3[k] <- CCidFUN(elkDataSrtC,nomadMod3)
#}

CC1id.nomadMod4 <- numeric(length(levels(elkDataSrtC$IND)))
for(k in 1:length(levels(elkDataSrtC$IND))) {  
     CC1id.nomadMod4[k] <- CCidFUN(elkDataSrtC,nomadMod4)
}


###
CC1id.HRmodA <- numeric(length(levels(elkDataSrtC$IND)))

for(k in 1:length(levels(elkDataSrtC$IND))) {
     
     CC1id.HRmodA[k] <- CCidFUN(elkDataSrtC,HRmodA)
}


CC1id.HRmodB <- numeric(length(levels(elkDataSrtC$IND)))

for(k in 1:length(levels(elkDataSrtC$IND))) {
     
     CC1id.HRmodB[k] <- CCidFUN(elkDataSrtC,HRmodB)
}


###
CC1id.Migrmod1 <- numeric(length(levels(elkDataSrtC$IND)))

for(k in 1:length(levels(elkDataSrtC$IND))) {
     
     CC1id.Migrmod1[k] <- CCidFUN(elkDataSrtC,Migrmod1)
}


CC1id.Migrmod2 <- numeric(length(levels(elkDataSrtC$IND)))

for(k in 1:length(levels(elkDataSrtC$IND))) {
     
     CC1id.Migrmod1[k] <- CCidFUN(elkDataSrtC,Migrmod2)
}

CC1id.Migrmod3 <- numeric(length(levels(elkDataSrtC$IND)))

for(k in 1:length(levels(elkDataSrtC$IND))) {
     
     CC1id.Migrmod1[k] <- CCidFUN(elkDataSrtC,Migrmod3)
}





# classify according to best fit
####################################################################
SpaceUseClass <- data.frame(IND = levels(elkDataSrtC$IND),
                            CC1.HRmodA = CC1id.HRmodA,CC1.HRmodB = CC1id.HRmodB,
                            CC1.Dispmod1 = CC1id.Dispmod1,CC1.Dispmod2 = CC1id.Dispmod2,CC1.Dispmod3 = CC1id.Dispmod3,
                            CC1.nomadModA = CC1id.nomadMod1,CC1.nomadModD = CC1id.nomadMod4,
                            CC1.NullMod = CC1id.NullMod,
                            CC1.Migrmod1 = CC1id.Migrmod1,CC1.Migrmod2 = CC1id.Migrmod2, CC1.Migrmod3 = CC1id.Migrmod3)

dim(SpaceUseClass) 

### add column with space use classification according to highest CC1 value
maxCC1column <- (apply(SpaceUseClass[,2:12], 1, which.max))

SpaceUseClass$bestMod.CC1ID <- ifelse(maxCC1column == 1, "HRmodA", ifelse(maxCC1column == 2, "HRmodB", 
                                                                          ifelse(maxCC1column == 3, "Dispmod1",ifelse(maxCC1column == 4, "Dispmod2",ifelse(maxCC1column == 5, "Dispmod3",
                                                                                          ifelse(maxCC1column == 6, "NomadA",ifelse(maxCC1column == 7, "NomadB",ifelse(maxCC1column == 8, "NullMod",
                                                                                                        ifelse(maxCC1column == 9, "Migrmod1", ifelse(maxCC1column == 10, "Migrmod2", "Migrmod3"))))))))))


SpaceUseClass$bestMod.CC1ID <- factor(SpaceUseClass$bestMod.CC1ID)

table(SpaceUseClass$ bestMod.CC1ID)
# Dispmod2   HRmodA Migrmod1 
#      11        2       18 


# how well do the models fit?

summary((apply(SpaceUseClass[,2:13], 1, max)))
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##0.001032 0.038327 0.131844 0.157746 0.181084 0.537377  


#################################################################################################################3
# plot some individual model predictions against observed data 

par(mfrow=c(4,4))
for(i in 1:16){
     plot(NSD~nDays,data=elkDataSrtD,subset=c(IND==levels(elkDataSrtD$IND)[i]), main=as.character(levels(elkDataSrtD$IND)[i]))
     lines(fitted(NullMod) ~ nDays, data= elkDataSrtD, subset=c(IND==levels(elkDataSrtD$IND)[i]),col=1, lty=3)
     lines(fitted(Dispmod1) ~ nDays, data= elkDataSrtD, subset=c(IND==levels(elkDataSrtD$IND)[i]),col=2)
     lines(fitted(Dispmod2) ~ nDays, data= elkDataSrtD, subset=c(IND==levels(elkDataSrtD$IND)[i]),col=2,lty=2)
     lines(fitted(Dispmod3) ~ nDays, data= elkDataSrtD, subset=c(IND==levels(elkDataSrtD$IND)[i]),col=2,lty=3)
     lines(fitted(Dispmod4) ~ nDays, data= elkDataSrtD, subset=c(IND==levels(elkDataSrtD$IND)[i]),col=2,lty=4)
     lines(fitted(nomadMod1) ~ nDays, data= elkDataSrtD, subset=c(IND==levels(elkDataSrtD$IND)[i]),col=4)
     lines(fitted(nomadMod2) ~ nDays, data= elkDataSrtD, subset=c(IND==levels(elkDataSrtD$IND)[i]),col=4,lty=3)
     lines(fitted(nomadMod3) ~ nDays, data= elkDataSrtD, subset=c(IND==levels(elkDataSrtD$IND)[i]),col=4,lty=4)
     lines(fitted(nomadMod4) ~ nDays, data= elkDataSrtD, subset=c(IND==levels(elkDataSrtD$IND)[i]),col=4,lty=5)
}



#####################################################################################################################
# select ID codes and model name of individuals which settled (i.e. for which dispersal model provided best fit)

DispIndivs <- SpaceUseClass[SpaceUseClass$bestMod.CC1ID %in% c("Dispmod4"),c('IND','bestMod.CC1ID')]
DispIndivs <- droplevels(DispIndivs)

# extract for each disperser the estimated individual model parameters (Asym, xmid, scal)
# remember to backtransform where necessary

DispDist <- list()
DispXmid <- list()
DispScal <- list()

for(j in 1:length(levels(DispIndivs$IND))) {
     if(DispIndivs$bestMod.CC1ID[j] == "Dispmod1") {DispDist[[j]] <- coef(Dispmod1)[which(rownames(coef(Dispmod1))==DispIndivs$IND[j]),'Asym']; 
     DispXmid[[j]] <- coef(Dispmod1)[which(rownames(coef(Dispmod1))==DispIndivs$IND[j]),'xmid'];
     DispScal[[j]] <- coef(Dispmod1)[which(rownames(coef(Dispmod1))==DispIndivs$IND[j]),'scal']}
     
     if(DispIndivs$bestMod.CC1ID[j] == "Dispmod2") {DispDist[[j]] <- exp(coef(Dispmod2)[which(rownames(coef(Dispmod2))==DispIndivs$IND[j]),'Asym']); 
     DispXmid[[j]] <- exp(coef(Dispmod2)[which(rownames(coef(Dispmod2))==DispIndivs$IND[j]),'xmid']);
     DispScal[[j]] <- exp(coef(Dispmod2)[which(rownames(coef(Dispmod2))==DispIndivs$IND[j]),'scal'])}						     
     
     if(DispIndivs$bestMod.CC1ID[j] == "Dispmod3") {DispDist[[j]] <- exp(coef(Dispmod3)[which(rownames(coef(Dispmod3))==DispIndivs$IND[j]),'Asym']); 
     DispXmid[[j]] <- exp(coef(Dispmod3)[which(rownames(coef(Dispmod3))==DispIndivs$IND[j]),'xmid']);
     DispScal[[j]] <- exp(coef(Dispmod3)[which(rownames(coef(Dispmod3))==DispIndivs$IND[j]),'scal'])}
     
     if(DispIndivs$bestMod.CC1ID[j] == "Dispmod4") {DispDist[[j]] <- exp(coef(Dispmod4)[which(rownames(coef(Dispmod4))==DispIndivs$IND[j]),'Asym']); 
     DispXmid[[j]] <- exp(coef(Dispmod4)[which(rownames(coef(Dispmod4))==DispIndivs$IND[j]),'xmid']);
     DispScal[[j]] <- exp(coef(Dispmod4)[which(rownames(coef(Dispmod4))==DispIndivs$IND[j]),'scal'])}
}

DispParams <- data.frame(IND = levels(DispIndivs$IND), DispDist = unlist(DispDist), DispXmid = unlist(DispXmid), DispScal = unlist(DispScal))	
DispParams$SettlTime <- ceiling(with(DispParams, DispXmid + 3*DispScal))  
DispParams$DispDistMeter <- sqrt(DispParams$DispDist)

#######################################################################################################################
# select ID codes and model name of individuals which did not settled (i.e. for which nomad model provided best fit)

NomIndivs <- SpaceUseClass[SpaceUseClass$bestMod.CC1ID %in% c("NomadA","NomadB","NomadC","NomadD"),c('IND','bestMod.CC1ID')]
NomIndivs <- droplevels(NomIndivs)

# extract for each nomad the estimated individual model parameters (D + exponent)

NomD <- vector(mode = 'list', length = length(levels(NomIndivs$IND))) 
NomExp <- vector(mode = 'list', length = length(levels(NomIndivs$IND))) 

for(j in 1:length(levels(NomIndivs$IND))) {
     if(NomIndivs$bestMod.CC1ID[j] == "NomadA") {NomD[[j]] <- 4 * coef(nomadMod1)[which(rownames(coef(nomadMod1))==NomIndivs$IND[j]),'D']; 
     NomExp[[j]] <- 1}
     
     if(NomIndivs$bestMod.CC1ID[j] == "NomadB") {NomD[[j]] <- coef(nomadMod2)[which(rownames(coef(nomadMod2))==NomIndivs$IND[j]),'D']; 
     NomExp[[j]] <- coef(nomadMod2)[which(rownames(coef(nomadMod2))==NomIndivs$IND[j]),'b']}
     
     if(NomIndivs$bestMod.CC1ID[j] == "NomadC") {NomD[[j]] <- 1; 
     NomExp[[j]] <- coef(nomadMod3)[which(rownames(coef(nomadMod3))==NomIndivs$IND[j]),'b']}
     
     if(NomIndivs$bestMod.CC1ID[j] == "NomadD") {NomD[[j]] <- coef(nomadMod4)[which(rownames(coef(nomadMod4))==NomIndivs$IND[j]),'D']; 
     NomExp[[j]] <- coef(nomadMod4)[which(rownames(coef(nomadMod4))==NomIndivs$IND[j]),'b']}    
}

NomParams <- data.frame(IND = levels(NomIndivs$IND), NomD = unlist(NomD), NomExp = unlist(NomExp))	



#########################################################################################################################  
### END
#########################################################################################################################



