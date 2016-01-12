################## ZIP DISTANCE FUNCTIONS ###################

## Distances
getDistances <- function(rownum, lat1s, long1s, lat2s, long2s) {
  
  distances <- sapply(1:rownum, function(h) {
    getDist(as.numeric(lat1s[h]), as.numeric(long1s[h]), as.numeric(lat2s[h]), as.numeric(long2s[h]))
  })

  return(distances)
}


## Haversine Formula
getDist <- function(lat1, long1, lat2, long2) {
  
  earthRadiusMi <- 3959
  deltaLat <- (lat2 - lat1)
  deltaLong <- (long2 - long1)
  a <- sin(deltaLat/2)^2 + cos(lat1) * cos(lat2) * sin(deltaLong/2)^2
  b <- 2 * asin(min(1,sqrt(a)))
  dist = earthRadiusMi * b
  
  gc()
  return(dist)
}


## Slice large df
cutDf <- function(totalNum, subNum, subNrow, df) {
  
  if (totalNum == 1)
    finalDf <- df[1:subNrow,]
  
  if (totalNum>1 && totalNum<subNum)
    finalDf <- df[(subNrow*(k-1)+1):(subNrow*k),]
  
  if (subNum>1 && totalNum==subNum)
    finalDf <- df[(subNrow*(k-1)+1):nrow(finalDf),]
  
  return(finalDf)
}


## Breakdown dfs
cutAndSaveDfs <- function(subNum, subNrow, df) {
  
  finalSaves <- lapply(1:subNum, function(k) {
    
    finalDf <- cutDf(k, subNum, subNrow, df)
    finalDf <- finalDf[c(7,1,4,2,3,5,6,8)]
    
    fp <- paste("ZipOutputs", k, ".csv", sep="")
    write.csv(finalDf, file=fp, row.names=F) # Save
    print(paste("CSV #", k, " saved"))
  })
  
}


## Add leading 0s for east coast zips 
# [OBSOLETE]
padZip <- function(zip) {
  
  charNum <- nchar(zip)
  if ((charNum) < 5)
    zip <- paste(paste(rep("0",5-nchar(zip)),collapse=""), zip, sep="")
  
  return(zip)
}