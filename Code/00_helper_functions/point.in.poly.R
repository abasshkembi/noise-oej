

point.in.poly <- function(x, y, sp = TRUE, duplicate = TRUE, ...) {
  if(!any(class(x)[1] == c("SpatialPoints", "SpatialPointsDataFrame", "sf"))) {
    stop("x is not a suitable point feature object class") }
  if(!any(class(y)[1] == c("SpatialPolygons", "SpatialPolygonsDataFrame", "sf"))) {
    stop("y is not a suitable polygon feature object class") }
  if(any(class(x) == "sfc")) { x <- sf::st_sf(x) }  
	if(duplicate == FALSE) {
	  if(!any(class(x)[1] == c("SpatialPoints", "SpatialPointsDataFrame"))) {
	    x <- methods::as(x, "Spatial")
		  if(dim(x@data)[2] == 0) stop("There are no attributes associated with points")
	  }	
      if(!any(class(y)[1] == c("SpatialPolygons", "SpatialPolygonsDataFrame"))) {
	    y <- methods::as(y, "Spatial")
		  if(dim(y@data)[2] == 0) stop("There are no attributes associated with polygons")
	  }
      o <- sp::over(x, y, returnList = TRUE)
      m <- max(unlist(lapply(o, nrow)))
        ids <- row.names(y)
          xy <- data.frame(t(sapply(1:length(o), 
            function(i) c(ids[i], c(o[[i]][,1], rep(NA, m))[1:m])
          )))
        colnames(xy) <- c("p",paste0("pid", 1:m))
      x@data <- data.frame(x@data, xy)
	    if( sp == FALSE ) sf::st_as_sf(x) 
	  return( x )
    } else {
  if(any( class(x) == c("SpatialPoints", "SpatialPointsDataFrame"))) {
    x <- sf::st_as_sf(x)
  }
  if(any(class(y) == "sfc")) { x <- sf::st_sf(y) }
  if(any( class(y) == c("SpatialPolygons", "SpatialPolygonsDataFrame"))) {
    y <- sf::st_as_sf(y)	
  } 
  if(dim(x)[2] == 1) x$pt.ids <- 1:nrow(x)	
    if(dim(y)[2] == 1) y$poly.ids <- 1:nrow(y)	  
      o <- sf::st_join(x, y, ...)
   if( sp ) o <- methods::as(o, "Spatial")
  # if( sp ) o <- sf::as_Spatial(o)
  return( o ) 
  } 
}
