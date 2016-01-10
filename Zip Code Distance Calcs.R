################### GET DISTANCES BETWEEN ZIPS IN MULTIPLE REGIONS ############
library(XML)

basefp <- "C:\\Users\\Heather\\Documents\\R\\Other\\Zip Distances\\"
### Load data (Original Source: https://www.aggdata.com/node/86)
orig_dat <- read.csv(paste(basefp,"us_postal_codes.csv",sep=""))

desired_regions <- c("Delaware","Rhode Island") #For ease, just do a few small states
dat <- orig_dat[(orig_dat$State %in% desired_regions),]
gc()

# Sort
regions <- as.vector(unique(dat$State)) #this can be any region in dataset
ct <- sapply(1:length(regions),function(h) {
  reg <- regions[h]
  len <- nrow(dat[dat$State == reg,])
  return(len)
})
srt <- as.data.frame(cbind(regions,ct))
srt$ct <- as.numeric(as.character(srt$ct))
srtd <- srt[with(srt,order(ct,regions)),]
regions <- as.character(srtd$regions) # Now regions will go in asc computational effort order
gc()

## Get data frames & Save
j <- 1
cutregs <- lapply(1:length(regions), function(i) {
  
  region <- regions[i]
  reg <- dat[dat$State == region,][1]
  df <- as.data.frame(apply(combn(unlist(reg),2),1,rev)) #unlist, get combinations, reverse matrix, & make df
  colnames(df) <- c("Zip1","Zip2")
  df$Region <- region
  gc()
  
  ## Break down file (necessary when dfs get very large)
  sub_num <- max(round(nrow(df)/15000,0),1)
  sub_nrow <- round(nrow(df)/sub_num)  # ~15000
  saves <- lapply(1:sub_num, function(k) {
    
    if (k==1) {dfs <- df[1:sub_nrow,]}
    if (k>1 & k<sub_num) {dfs <- df[(sub_nrow*(k-1)+1):(sub_nrow*k),]}
    if (k==sub_num && sub_num>1) {dfs <- df[(sub_nrow*(k-1)+1):nrow(df),]}
    
    fp <- paste(basefp,"ZipCombos",j,".rdata", sep="")
    save(dfs,file=fp) ## Save down
    j <<- j + 1
    return()
  })
  print(i)
  return()
}) 



## Loop for distances
runLoop <- sapply(1:2, function(h) { # Specify iteration through mutliples files (ex 1:15)
  
  zipfp <- paste(basefp,"ZipCombos",h,".rdata", sep="") #filepath
  load(zipfp)
  currentRegion <- dfs$Region[1]
  
  ## Grab distances
  getDist <- sapply(1:nrow(dfs), function(g) {
    zip1 <- dfs$Zip1[g] #grab first zip
    if (nchar(zip1)==4) {zip1 <- paste("0",zip1,sep="")} #Add leading zeros for east coast zips
    zip2 <- dfs$Zip2[g] #grab second zip
    if (nchar(zip2)==4) {zip2 <- paste("0",zip2,sep="")}
    url <- paste("http://maps.google.com/maps?f=d&source=s_d&saddr=",zip1,"&daddr=",zip2,sep="") #create URL
    dist <- tryCatch(dist <- as.numeric(strsplit(readHTMLList(url)[[5]][[1]]," ")[[1]][[1]]),
                     error=function(e){  #data munge results & error catch
                       dist <- NA
                       return(dist)})
    #globalVar <<- g
    print(g) #turn off if real-time output undesired
    gc()
    return(dist)    
  }) 
  gc()
  
  dfs$DrivingDistanceMi <- getDist
  dfs <- dfs[c(1,2,4,3)]
  
  fps <- paste(basefp,"ZipOutputs ",currentRegion," ",h,".csv", sep="") #save to filepath
  write.csv(dfs,fps)  #Save CSV
  print(paste("CSV #",h,"saved"))
  
  ## If iterating through heavy data, use restart below
  #system("R --Wooden Christmas-Tree") #Restart R (user version)
  
  return()
  
})

#q("no") #close w/o save
