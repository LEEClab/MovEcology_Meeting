
# Simulate switching random walk (from HR to CRW and back)
# use it with the segmentation methods to evaluate how well they identify these switches

#?simm.mou
  
  phase1 <- simm.mou(date = 1:20, x0 = c(0, 0), a=diag(c(0.1,0.1)),sigma=diag(c(30,30)))
  phase2 <- simm.crw(21:50, r = 0.8,h = 90, x0=c(phase1[[1]]$x[20], phase1[[1]]$y[20]))	
  phase3 <- simm.mou(date=51:125, a=diag(c(0.1,0.1)),sigma=diag(c(30,30)),b=c(phase2[[1]]$x[30], phase2[[1]]$y[30]))
  phase4 <- simm.crw(126:250, r = 0.65, h = 90, x0=c(phase3[[1]]$x[75], phase3[[1]]$y[75]))
  phase5 <- simm.mou(date=251:400, a=diag(c(0.1,0.1)),sigma=diag(c(20,20)),b=c(phase4[[1]]$x[125], phase4[[1]]$y[125]))

# combine into a dataframe
# note, I had not inlcuded phase5 by mistake, but keep it like this for now

  MovePaths01 <- data.frame(X = c(phase1[[1]]$x, phase2[[1]]$x, phase3[[1]]$x, phase4[[1]]$x), Y = c(phase1[[1]]$y, phase2[[1]]$y, phase3[[1]]$y, phase4[[1]]$y))
  
# add time info
  
  time1 <- as.POSIXct("2017-03-01 08:00:00", format="%Y-%m-%d %H:%M:%S", tz = "UTC")
    
  MovePaths01$DateTime <- rep(time1, nrow(MovePaths01)) + (3600*24)*(0:(nrow(MovePaths01) - 1))

# add random ID

  MovePaths01$ID <- rep("ID01", nrow(MovePaths01))
  
# transform into ltraj object
  # library(adehabitatLT)
  
  Move01traj <- as.ltraj(xy = MovePaths01[,c("X","Y")], date = MovePaths01$DateTime, id = MovePaths01$ID)
  
# apply Lavielle method on step lengths
  
   l1sim <- lavielle(na.omit(Move01traj[1][[1]]$dist), Lmin=3, Kmax=20, type="mean")
   chooseseg(l1sim) 
   # strong reduction until 4 breakpoints, then only small incremental increases
   # compare 4 against 10, 20, 30, 40, using findpath
   
   par(mfrow=c(2,2))
   findpath(l1sim, 4)
   findpath(l1sim, 10)
   findpath(l1sim, 20)
   findpath(l1sim, 40)
   par(mfrow=c(1,1))
   
   # quite clear that after 4 break points, not much is added and in fact k = 4 nails exactly all simulated break points!
   par(mfrow=c(2,2))
   phaseRnd <- simm.crw(1:500, r=0.99, h= 90, x0=c(0,0))
   plot(phaseRnd[[1]]$x, phaseRnd[[1]]$y)
   
   phase5 <- simm.mou(date=251:400, a=diag(c(0.1,0.1)),sigma=diag(c(20,20)),b=c(phase4[[1]]$x[125], phase4[[1]]$y[125]))
   plot(phase5[[1]]$x, phase5[[1]]$y)
   
   
   