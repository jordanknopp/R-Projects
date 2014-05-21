makeCluster<-function(data)
{
#digit1<-digit[,-1]
dists<-dist(data, "euclidean")
hClustering <- hclust(dists, method="single")
par(oma=c(2,2,2,2))
par(mai=c(1,1,1,1))
plot(hClustering,main="Single Clustering")
L <- cutree(hClustering,k=1)
myplclust(hClustering,lab.col=L)
}


kcenter<-function(data)
{
  NUM_CENTERS_TESTED <- 10
  results <- vector(mode="numeric",length=10)
  for (num_centers in 1:NUM_CENTERS_TESTED)
  {
    km <- kmeans(data,centers=num_centers)
    var_expl <- km$betweenss/km$totss
    results[num_centers] <- var_expl
    cat("Centers: ",num_centers,"variance explained: ", var_expl,"\n")
  }
  
  # plot variance explained for each number of centers...
  plot(1:NUM_CENTERS_TESTED, results,
       type="b",pch=16,col="black",
       xlab="Number of Centers",ylab="Variance Explained")
}

myplclust <- function( hclust, lab.col, hang=0.1, ... )
{
  y <- rep(hclust$height,2); x <- as.numeric(hclust$merge)
  y <- y[which(x<0)]; x <- x[which(x<0)]; x <- abs(x)
  y <- y[order(x)]; x <- x[order(x)]
  plot( hclust, labels=FALSE, hang=hang, ... ,main="Single Clustering")
  text( x=x, y=y[hclust$order]-(max(hclust$height)*hang),
        labels=hclust$order, col=lab.col[hclust$order], 
        srt=90, adj=c(1,0.5), xpd=NA, ... )
}


# example call: plot.data( d )
# d should be a row of 784 pixel values
plot.data <- function(data)
{
  D = 28  # image is square with D pixels on a side
  im1 <- as.single(1.0-data/255)  # adjust pixels
  im1 <- matrix( im1, nrow=D, ncol=D, byrow=T ) # make 2d
  image <- as.raster(im1)  # make a raster (an image)
  # create a plot... something
  plot(c(1,D),c(1,D), type = "n", xlab="", ylab="")
  rasterImage(image,1,1,D,D,interpolate=F)
}


# example call: plot.digit( df, 42 )
# df is a dataframe of digits (with no labels/initial columns)
# the rownumber is just that...
plot.digit <- function(digs,rownumber)
{
  D = 28  # image is square with D pixels on a side
  im1 <- digs[rownumber,]  # get first row
  #im1 <- im1[,-c(1,2)] # remove first two columns
  plot.data(im1)
}