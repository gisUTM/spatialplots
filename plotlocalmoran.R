require(spdep)

plot.local.moran <- function(x, variable.name, local.moran, weights, sig = 0.05, plot.only.significant = TRUE, legend.location = "bottomleft", zero.policy = NULL){
  if(!inherits(local.moran, "localmoran"))
    stop("local.moran not an object of class localmoran")
  if(!inherits(weights, "listw"))
    stop("weight not a listw")
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST be sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT")

  
  # Check if local.moran subsetted missing data
  #x <- na.action(local.moran)
  na.act <- na.action(LISA)
  
  if (!is.null(na.act)) {
    # Rows to drop in weight matrix (weights)
    subVec <- !(1:length(weights$neighbours) %in% na.act)
    
    # Subset weights
    weights <- subset(weights, subVec, zero.policy = zero.policy)
    
    # Subset localmoran
    local.moran <- local.moran[subVec,]
    
    # Subset Polygons
    origPoly <- x
    x <- subset(x, subVec)
  }
  
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

      if (locm.dt[i]==0) colors1[i] <- "grey78"

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
  labels=c("High-High", "High-Low", "Low-High", "Low-Low", "Not Significant", "Missing Data")
  # plot the map
  # Plot out the full set of polygons (missing data will not be overlaid)
  plot(origPoly, col = "black")
  if (plot.only.significant == TRUE){
    plot(x, col=colors1,border=T, lwd=0.2, add = TRUE) 
  }else{
    plot(x, col=colors,border=T, add = TRUE)
  }
  legend(legend.location, legend = labels, fill = c("red", "pink", "lightblue", "blue", "grey78", "black"), bty = "n")

  
}
