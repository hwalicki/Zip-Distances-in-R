################### GET DISTANCES BETWEEN ZIPS IN MULTIPLE REGIONS ############

### Load functions
baseFp <- "C:\\Users\\Heather\\Documents\\R\\Other\\Zip-Distances-in-R\\"
setwd(baseFp)
source("zipFuns.r")

### Load data 
# Original Source: https://www.aggdata.com/node/86
origDat <- read.csv(paste(baseFp, "us_postal_codes.csv", sep=""))

desiredRegions <- c("Delaware","Rhode Island") #For ease, just do a few small states
dat <- origDat[(origDat$State %in% desiredRegions),]
gc()

### Sort
regions <- as.vector(unique(dat$State)) #this can be any partition in dataset
ct <- sapply(1:length(regions), function(h) {
  reg <- regions[h]
  len <- nrow(dat[dat$State == reg,])
  return(len)
})
srt <- as.data.frame(cbind(regions, ct))
srt$ct <- as.numeric(as.character(srt$ct))
srtd <- srt[with(srt, order(ct, regions)),]
regions <- as.character(srtd$regions) # Now regions will go in asc computational effort order
gc()


### Create combos & Calculate distances
df <- data.frame(Region=character(), Zip1=integer(), Zip2=integer(), Zip1Lat=double(), Zip1Long=double(),
                      Zip2Lat=double(), Zip2Long=double(), DistanceInMiles=double())

calcDistances <- lapply(1:length(regions), function(i) {
  
  region <- regions[i]
  reg <- dat[dat$State == region,][c(1,6,7)]
  reg$ZipLatLong <- paste(reg$Postal.Code, reg$Latitude, reg$Longitude)
  
  combos <- apply(combn(unlist(reg$ZipLatLong),2),1,rev) #Get combinations/reverse matrix
  combinations <- cbind(read.table(text = combos[,1], sep=" ", colClasses = "character"),
                        read.table(text = combos[,2], sep=" ", colClasses = "character"))
  colnames(combinations) <- c("Zip1", "Zip1Lat", "Zip1Long", "Zip2", "Zip2Lat", "Zip2Long")
  combinations$Region <- region
  gc()
  
  # Calc Distance
  combinations$DistanceInMiles <- getDistances(nrow(combinations), combinations$Zip1Lat, combinations$Zip1Long, 
                      combinations$Zip2Lat, combinations$Zip2Long)
  
  df <<- rbind(df, combinations)
  print(i)
})

### Break down file & Save
# Breakdown necessary if df > 1M rows for Excel
maxNum = 1000000
subNum <- max(round(nrow(df)/maxNum,0),1)
subNrow <- round(nrow(df)/subNum)  # ~1M
saves <- cutAndSaveDfs(subNum, subNrow, df, baseFp)
