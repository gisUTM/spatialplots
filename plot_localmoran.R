require(spdep)

plot_localMoran <- function(x, variable.name, local.moran, weights, sig = 0.05, plot.only.significant = TRUE){
  if(!inherits(locm, "localmoran"))
    stop("local.moran not an object of class localmoran")
  if(!inherits(weights, "listw"))
    stop("weight not a listw")
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST be sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT")
  if(sum(is.na(x[[variable.name]]))> 0)
    stop("x contains NA in variable.name. See function sp.remove.na.rows()")
  
  
  # Get length of x
  n <- length(x)
  
  #
  vec <- c(1:n)
  vec <- ifelse(local.moran[,5] < sig, 1,0)
  
  # Create the lagged variable
  lagvar <- lag.listw(weights, x[[variable.name]])
  
  # get the mean of each
  m.myvar <- mean(x[[variable.name]])
  m.lagvar <- mean(lagvar)
  
  myvar <- x[[variable.name]]
  
  # Derive quadrants
  q <- c(1:n) 
  
  for (i in 1:n){   
    if (myvar[[i]]>=m.myvar & lagvar[[i]]>=m.lagvar)
      q[i] <- 1
    if (myvar[[i]]<m.myvar & lagvar[[i]]<m.lagvar) 
      q[i] <- 2
    if (myvar[[i]]<m.myvar & lagvar[[i]]>=m.lagvar) 
      q[i] <- 3   
    if (myvar[[i]]>=m.myvar & lagvar[[i]]<m.lagvar) 
      q[i] <- 4
  }
  
  # set coloring scheme
  q.all <- q
  colors <- c(1:n)
  for (i in 1:n) {
    if (q.all[i]==1) 
      colors[i] <- "red"
    if (q.all[i]==2) 
      colors[i] <- "blue"
    if (q.all[i]==3) 
      colors[i] <- "lightblue"
    if (q.all[i]==4) 
      colors[i] <- "pink"
    if (q.all[i]==0) 
      colors[i] <- "white"   
    if (q.all[i]>4) 
      colors[i] <- "white"
  }
  
  # Mark all non-significant regions white
  locm.dt <- q*vec
  colors1 <- colors
  for (i in 1:n){
    if ( !(is.na (locm.dt[i])) )  {
      if (locm.dt[i]==0) colors1[i] <- "grey97"
    }
  }
  
  colors2 <- colors
  colors2 <- paste(colors2,vec)
  pos = list()
  for (i in 1:n) {
    pos[[i]] <- c(which(myvar==colors2["blue 0"]))
  }
  
  blue0 <- which(colors2=="blue 0")
  red0 <- which(colors2=="red 0")
  lightblue0 <- which(colors2=="lightblue 0")
  pink0 <- which(colors2=="pink 0")
  lb <- 6
  labels=c("High-High", "High-Low", "Low-High", "Low-Low", "Not Significant")
  # plot the map
  if (plot.only.significant == TRUE){
    plot(x, col=colors1,border=T, lwd=0.2) 
  }else{
    plot(x, col=colors,border=T)
  }
  legend("bottomleft", legend = labels, fill = c("red", "pink", "lightblue", "blue"), bty = "n")
  
}


sp.remove.na.rows <- function(x, variable) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data[[variable]]),arr.ind=TRUE))[,margin])
  cat("DELETING ROWS: ", na.index, "\n") 
  return( x[-na.index,]  )

}
