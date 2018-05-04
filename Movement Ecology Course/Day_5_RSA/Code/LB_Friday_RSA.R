#####################################################################
## MovEcol Workshop, UNESP Rio Claro, Brazil, 19/02 - 23/02 - 2018 ##
##						                   ##
##               Resource Selection Analysis 	                   ##
##	        Luca Borger, 23/02/2018		         	   ##
##modified after code & worksheet by Jonathan Potts (Sheffield, UK)##	
#####################################################################

  require(survival)

  rsfex1 <- read.csv("sim_rsf_rf1_beta1_1.csv")

  beta1sim <- clogit(Observed ~ resource + strata(strata), data = rsfex1)


  rsfex3 <- read.csv("sim_rsf_rf1_beta3_1.csv")
  beta3sim = clogit(Observed ~ resource + strata(strata), data = rsfex3)
  beta3sim
  
  
  rsfex5 <- read.csv("sim_rsf_rf1_beta5_1.csv")
  beta5sim = clogit(Observed ~ resource + strata(strata), data = rsfex5)
  beta5sim
  
  
  beta1sim_quad = clogit(Observed ~ I((resource)^2) + strata(strata), data = rsfex1)
  beta1sim_quadlin = clogit(Observed ~ resource + I((resource)^2) + strata(strata), data = rsfex1)
  beta1sim_quad
  beta1sim_quadlin


# The file sim_rsf_rf1_quad.csv contains data simulated from the model with a linear and quadratic term with
# beta 1 = 1 and beta2 = 0.5. Fit these data to the models with quadratic and linear + quadratic terms. 
# Which fits best? Do the inferred parameter values reflect the "real" parameters from which the data were simulated?
# Notice the p-value when tting the model from Equation (1). What does this imply for fitting
# models to real data, when you do not know the "real" parameter values.
  
  rsfex1quad <- read.csv("sim_rsf_rf1_quad.csv")

# continue on your own



#############################################################################  
# Analysis of real data
#############################################################################

# For this, we will use some Elk data, found in thefile elk positions.csv. This is just a subsample
# of the complete dataset, studied by (Hebblewhite et al., 2008). For RSA, we need some control
# samples. The minimum longitude is -115.91494 and the max is -115.48174. Latitudes are between
# 51.72699 and 52.1541. So we'll take a uniform distribution over the rectangle dened by these four
# points as our `availability kernel'.

# To sample from this kernel and write the results to file, use the following commands:

  longrand<-runif(32320, min=-115.91494, max=-115.48174)
  latrand<-runif(32320, min=51.72699, max=52.1541)
 
  require(MASS)
  write.matrix(longrand, file = "longrand.csv", sep=",")
  write.matrix(latrand, file = "latrand.csv", sep=",")

# Then paste the outputs of these files into elk positions.csv at the bottom of the latitude
# and longitude columns. We now need to nd the elevations at these control points, which we will
# import from Google Maps. For this, rst install the rgbif package and import it using the command
  require(rgbif) 
  
# Next, you need to get a personal key, available from 
# https://developers.google.com/maps/documentation/elevation/start#api_key. 
# I'll call this YourAPIKey as a placeholder in the below.
# The command for getting elevation data is:

  altrand<-elevation(latitude=latrand,longitude=longrand,key="YourAPIKey")
  write.matrix(altrand, file = "altrand.csv", sep=",")

# Open altrand.csv in Excel, copy and paste the values to the bottom of the altitude column. Now
# add two columns, one labelled `Observed' and the other labelled `strata'. Put a `1' in the Observed
# column for every row that corresponds to an elk position and a `0' in every other row (the controls).
# In the strata column, label each of the observed points with a unique number. Then match each
# of these numbers to 10 of the controls (as done, for example, in sim rsf rf1 beta1 1.csv). An
# example of the resulting file is given in elk_positions_samples.csv. Your tasks:

# 1. Use RSA to see if there is a correlation between position and altitude for this elk.
# 2. Experiment with other covariates - e.g. the square of the altitude. Does anything interesting
# arise?


#############################################################################
# END
#############################################################################