library(sp)
library(raster)
library(dplyr)

tmp <- read.csv('/home/put_data/moth/metadata/metadata_flickr_summary_complete.csv', stringsAsFactors = F)
tmp <- tmp[c('Latitude', 'Longititude', 'Filename', 'date')]
d <- read.csv('/home/put_data/moth/metadata/weather/species_regression_v2.csv')
d <- merge(d, tmp, all.x=TRUE, by='Filename')

mm <- substr(d$date, 5, 6)

srad <- rep(NA, length(mm))
counter = 0
for(m in unique(mm)){
  print(paste("month:", m, "start", sep = " "))
  idx = which(mm == m)
  filename = paste0("srad/wc2.0_30s_srad_",m,".tif")
  raster_data<-brick(filename) 
  for(i in idx){
    counter = counter + 1
    grid_data <- extract(raster_data, SpatialPoints(matrix(c(d$Longititude[i],d$Latitude[i]),1,2)))
    srad[i] <- grid_data[1]
    cat(paste0(counter,"\r"))
  }
}

saveRDS(srad, file='srad.Rds')


#####################

tavg <- rep(NA, length(mm))
counter = 0
for(m in unique(mm)){
  print(paste("month:", m, "start", sep = " "))
  idx = which(mm == m)
  filename = paste0("tavg/wc2.0_30s_tavg_",m,".tif")
  raster_data<-brick(filename) 
  for(i in idx){
    counter = counter + 1
    grid_data <- extract(raster_data, SpatialPoints(matrix(c(d$Longititude[i],d$Latitude[i]),1,2)))
    tavg[i] <- grid_data[1]
    cat(paste0(counter,"\r"))
  }
}



dtype <- c('prec', 'wind', 'vapr', 'tmin', 'tmax'
           #,'srad','tavg'
           )
for(type in dtype){
  res <- rep(NA, length(mm))
  counter = 0
  for(m in unique(mm)){
    print(paste(type,", month:", m, "start", sep = " "))
    idx = which(mm == m)
    filename = paste0(type,"/wc2.0_30s_",type,"_",m,".tif")
    raster_data<-brick(filename) 
    for(i in idx){
      counter = counter + 1
      grid_data <- extract(raster_data, SpatialPoints(matrix(c(d$Longititude[i],d$Latitude[i]),1,2)))
      res[i] <- grid_data[1]
      cat(paste0(counter,"\r"))
    }
  }
  d[type] = res
  saveRDS(res, file=paste0(type,'.Rds'))
}



d$srad = srad
d$tavg = tavg

write.csv(d, file='/home/put_data/moth/metadata/weather/updated_species_regression_v2.csv')
