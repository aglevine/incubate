library(plyr)

#Get great circle distance
getDistance <- function(latStart,longStart,latEnd,longEnd){
  #convert from degrees to radians
  latStart <- latStart/180.0*pi
  longStart <- longStart/180.0*pi
  latEnd <- latEnd/180.0*pi
  longEnd <- longEnd/180.0*pi
  deltaLat <- latEnd - latStart
  deltaLong <- longEnd - longStart
  
  #Use Haversine formula
  A = (sin(deltaLat/2)^2) +cos(latStart)*cos(latEnd)*(sin(deltaLong/2)^2)
  B = sqrt(A)
  D = 6378.1*2*asin(B) #radius of Earth: 6378.1 km
 
  return D
}
  
 

bikeShare_df <- read.csv("201503-citibike-tripdata.csv")

dim(bikeShare_df)
str(bikeShare_df)
tripLength <- bikeShare_df$tripduration #triplength
summary(tripLength)
bikeShareRound_sub <- subset(bikeShare_df,start.station.id == end.station.id) #round trip trips
str(bikeShareRound_sub)
bikeID <- unique(bikeShare_df[c("bikeid","end.station.id","start.station.id")]) #trim data down a bit to find unique bikes
str(bikeID)
dim(bikeID)
bikeUniques <- unique(bikeShare_df$bikeid)
str(bikeUniques)
#count(bikeID,c("bikeid","end.station.id"))
bikeStations <- vector(mode="numeric", length=0)
for (uniqueBike in bikeUniques){
  bikeID_sub <- subset(bikeID,bikeid==uniqueBike)
  #str(bikeID_sub)
  startStation <- bikeID_sub$start.station.id
  endStation <- bikeID_sub$end.station.id
  stations <- append(startStation,endStation)
  bikeStations <- append(bikeStations, length(unique(stations)))
  #print(length(unique(stations)))
}
str(bikeStations)
sd(bikeStations)
#length(unique(bikeID)) #number of bikes



