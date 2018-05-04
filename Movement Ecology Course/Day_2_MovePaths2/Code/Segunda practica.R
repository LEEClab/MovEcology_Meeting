####AFTERNOON PRACTICE

MovePaths01 <- read.csv("MovePaths01.csv")

#add time info
time1 <- as.POSIXct("2017-03-01 08:00:00", format="%Y-%m-%d %H:%M:%S", tz = "UTC") 
MovePaths01$DateTime <- rep(time1, nrow(MovePaths01)) + (3600*24)*(0:(nrow(MovePaths01)-1))


##Add identity to my points
MovePaths01$ID <- rep("ID01", nrow(MovePaths01))

MovePaths01$Date <- as.Date(MovePaths01$DateTime)  

MovePaths01$hour <- as.numeric(strftime(as.POSIXlt(MovePaths01$DateTime),format="%H"))

MovePaths01$nrow <- 1:nrow(MovePaths01)

foo <- aggregate(nrow ~ ID + Date + hour, data = MovePaths01, FUN = min)

foo2 <- merge(MovePaths01, foo, by = "nrow")

rm(foo, foo2)
#######
Movepathtraj <- as.ltraj(xy = MovePaths01[,c("X","Y")], date = MovePaths01$DateTime, id = MovePaths01$ID)
Movepathtraj

names(Movepathtraj)
movel1 <- lavielle(na.omit(Movepathtraj[1][[1]]$dist), Lmin=3, Kmax=20, type="mean")
movel1
str(movel1)
chooseseg(movel1)

fp <- findpath(movel1,4 )

####residence time

Moving1<- Movepathtraj
Moving <- residenceTime(Moving1, radius = 1000, maxt=6, units="hour")
plot(Moving)





