#' obtain information from a raster
#'
coord.by.trial <-function(cov.raster,reference, long, latd, trial){

  leg  = reference
  loc  = data.frame(x=leg[,long],y=leg[,latd])
  trial = leg[,trial]
  coordinates(loc)= ~x+y
  proj4string(loc) = CRS("+proj=longlat +datum=WGS84")
  
  for(i in 1:length(names(cov.raster))){trial = cbind(trial,data.frame(extract(cov.raster[[i]], loc)))}
  names(trial)[-1] = names(cov.raster)
  return(data.frame(unique(trial)))
}
