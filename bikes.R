library(plyr)
options(digits=10)

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
 
  return(D)
}


bikeShare_df <- read.csv("201503-citibike-tripdata.csv")

dim(bikeShare_df)
totalRides <- nrow(bikeShare_df)
length(bikeShare_df)
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
bikeMoves <- vector(mode="numeric",length=0)
for (uniqueBike in bikeUniques){
  bikeID_sub <- subset(bikeID,bikeid==uniqueBike,select=c(end.station.id,start.station.id)) #get all stations,substations for unique bikeID
  bikeIDStart <- bikeID_sub$start.station.id #all the places the bike started from
  bikeIDEnd <- bikeID_sub$end.station.id #all the places the bike ended up
  deleteRow <- length(bikeIDEnd)-1
  bikeIDStart <- bikeIDStart[-1] #not counting where the bike started
  bikeIDEnd <- bikeIDEnd[-deleteRow] #not counting where the bike ended, we want to know moves
  bikeIDChanges <- (bikeIDStart-bikeIDEnd)
  bikeMoves <- append(bikeMoves,length(bikeIDChanges[bikeIDChanges != 0])) #nonzero is where the points differed and the bike moves
  
  bikeStations <- append(bikeStations,length(unique(unlist(bikeID_sub)))) #add unique bike station visists
}
str(bikeStations)
sd(bikeStations)


bikeDistance <- subset(bikeShare_df,start.station.id != end.station.id, select=c(start.station.latitude,start.station.longitude,end.station.latitude,end.station.longitude))
distances <- getDistance(bikeDistance$start.station.latitude,bikeDistance$start.station.longitude,bikeDistance$end.station.latitude,bikeDistance$end.station.longitude)
distances <- distances[distances<20.0 && distances > 0.08] #make sure distances are at least a city block and not out of nyc
mean(distances)
#length(unique(bikeID)) #number of bikes

timeMonth <- vector(mod="numeric",length=0)
#for (m in 3:3){ #loop through months

bikeMonth <- subset(bikeShare_df,as.numeric(format(strptime(starttime,format='%m/%d/%Y %H:%M'),'%m')) == 3, select=c(tripduration))
timeMonth <- append(timeMonth,mean(bikeMonth$tripduration))
#}
str(timeMonth)
print(max(timeMonth)-min(timeMonth))


stationsStart <- subset(bikeShare_df,select = c(start.station.id,starttime))
stations <- unique(stationsStart$start.station.id)
usageRatioStation <- vector(mod="numeric",length=0)
usageRatio <-vector(mod="numeric",length=0)

for (h in 0:23){ #calculate total usage ratios for each hour
  hourUsageRatio <-  nrow(subset(stationsStart,as.numeric(format(strptime(starttime,format='%m/%d/%Y %H:%M'),'%H')) == h))/totalRides
  usageRatio <- append(usageRatio,hourUsageRatio)
  #print(hourUsageRatio)
}
#print(sum(usageRatio))
for (uniqueStation in stations){ #for each station
  stationRides <- subset(stationsStart, uniqueStation == start.station.id)
  totalStationRides <- nrow(stationRides)
  for (h in 0:23){ #for each hour
        hourUsageRatioStation <- nrow(subset(stationRides,as.numeric(format(strptime(starttime,format='%m/%d/%Y %H:%M'),'%H')) == h))/totalStationRides
        #print(hourUsageRatioStation/usageRatio[h+1])
        usageRatioStation <-append(usageRatioStation,hourUsageRatioStation/usageRatio[h+1])
  }
}
print(max(usageRatioStation))

tripsOver <- subset(bikeShare_df,(usertype == "Subscriber"&& tripduration > 2700) || (usertype == "Customer" && tripduration > 1800),select=c(tripduration))
print(nrow(tripsOver)/totalRides)
mean(bikeMoves)
