###The following is code for program R to: 1) create and plot 3D utilization distributions via a multivariate kernel density estimator (Duong et al. 2007), and 2) quantify 3D spatial overlap using VI and UDOI (adapted from Fieberg & Kochanny 2005). Portions of the code were adapted from Simpfendorfer et al. (2012).

#Loads necessary libraries
library("ks")
library("MASS")
library("KernSmooth")
library("CircStats")
library("odesolve")##paquete no posible de instalar
library("coda")
library("deldir")
library("igraph")
library("RandomFields")

#Reads files for Animal A and Animal B into R workspace, where "Animal a.csv" is a file with X,Y,Z #coordinates
#a <- read.csv("Animala.csv")
#b <- read.csv("Animalb.csv")
# but as I do not have any adequate data at hand, I simulate some

  a_sim <- simm.mou(date = 1:100, x0 = c(0, 0), a=diag(c(0.1,0.1)),sigma=diag(c(30,30)))
  b_sim <- simm.mou(date = 1:100, x0 = c(20, 20), a=diag(c(0.1,0.1)),sigma=diag(c(30,30)))
  
  aDF <- ld(a_sim)
  a <- aDF[,c('x','y')]
  
  bDF <- ld(b_sim)
  b <- aDF[,c('x','y')]
  
  a$z <- runif(100, min = 0, max = 20)
  b$z <- runif(100, min = 0, max = 15)
  
  
#Calls the plug-in bandwidth estimator for dataset "a" and "b"
  BandA <- Hpi(a)
  BandB <- Hpi(b)

#Joins the 2 datasets and then determines the minimum and maximum boundaries for each dimension. 
# It is important that overlapping territories are evaluated in the same physical space. Also 
# adds small buffer so that no part of the territories are cut off.

  c <- rbind(a,b)

  minX<-min(c$x)-25
  minY<-min(c$y)-25
  minZ<-0

  maxX<-max(c$x)+25
  maxY<-max(c$y)+25
  maxZ<-max(c$z)+5

  install
  ??kde
# Runs the kernel density analysis
  fhata <- kde(x=a, H=BandA, binned=FALSE, xmin=c(minX,minY,minZ), xmax=c(maxX,maxY,maxZ),gridsize = 151) 
  fhatb <- kde(x=b, H=BandB, binned=FALSE, xmin=c(minX,minY,minZ), xmax=c(maxX,maxY,maxZ),gridsize = 151)

# Defines 95th isopleth
  CL95a <- contourLevels(fhata, cont=95, approx=FALSE) 
  CL95b <- contourLevels(fhatb, cont=95, approx=FALSE)

# Calculates the volume at the 95th isopleth
  Vol95a<-contourSizes(fhata, cont=95)
  Vol95b<-contourSizes(fhatb, cont=95)

# Allows for use of other isopleths when calculating VI and UDOI (see eqn 5)
  fhata$estimate <- ifelse(fhata$estimate>=CL95a,fhata$estimate/.95,0)
  fhatb$estimate <- ifelse(fhatb$estimate>=CL95b,fhatb$estimate/.95,0)

# Computes individual spatial overlap at 95th isopleth
  fhat.overlap <- fhata
  fhat.overlap$estimate <- fhata$estimate>CL95a & fhatb$estimate>CL95b

#Quantifies volume of overlapped space
  Overlap95<-contourSizes(fhat.overlap, abs.cont=.5)

#Computes percentage of territory bird "a" that is overlapped by bird "b" and vice versa
  PercentOverlap95a<-(Overlap95/Vol95a)*100
  PercentOverlap95b<-(Overlap95/Vol95b)*100

#Computes voxel size
  vol.cell <- prod(sapply(fhata$eval.points, diff)[1,]) 

#Calculates 3D versions of VI (eqn 2) and UDOI (eqn 4)
  VI3D <- sum(pmin(fhata$estimate,fhatb$estimate)*vol.cell)
  UDOI3D<-Overlap95*sum(fhata$estimate*fhatb$estimate*vol.cell)

#Plots 95th isopleth of neighboring animals on same graph
  plot(fhata,cont=95,colors="red",drawpoints=TRUE,xlab="", ylab="", zlab="",size=2, ptcol="black") 
  plot(fhatb,cont=95,colors="blue",add=TRUE,drawpoints=TRUE,xlab="", ylab="", zlab="",size=2,ptcol="grey")

###########################################################################################################################################################
