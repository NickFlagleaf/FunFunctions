#' Jitter repel coordinates for plotting.
#'
#' @param x X coordinates.
#' @param y Y coordinates.
#' @param jitter.fact Jitter factor. Numeric. Higher values will make for variable jittering. Default is 10
#' @param jitteration Number of jitter iterations to repeat. Higher number will make points further apart. Default is 200.
#' @param min.dist The minimum distance that points can be apart. Default is .5
#'
#' @return A data frame of new x and y coordinates that should be further apart.
#' @export
#'
#' @examples
#' #Make some random data on different scales
#' x<-rnorm(1:40,mean = 1,sd = 0.1)
#' y<-rnorm(1:40,mean = 100,sd = 5)
#' labs<-paste(1:40,month.abb) #Make some random point labels
#' new.coords<-jit.repel(x,y) #jitter new point label coordinates
#' plot(x,y,pch=16,col="grey40",bty="n") #plot the points
#' arrows(x,y,new.coords[,1],new.coords[,2], #plot connecting lines to labels
#'       length = 0,xpd=T,col="grey40")
#' text(new.coords,labels = labs,xpd=T) #add labels with the new point label coordinates


jit.repel<-function(x,y,jitter.fact=10,jitteration=200,min.dist=0.5){
  set<-list()
  start.dist<-dist(scale(cbind(x,y))) #get a distance matrix among the points
  is.too.close<-apply(as.matrix(start.dist),2,FUN = function(d) min(d[d>0]))<min.dist #work out which points are too close to another
  for (j in 1:100) {
     set[[j]]<-cbind(jitter(x,factor = jitter.fact),jitter(y,factor = jitter.fact)) #make a load of different jittered coordinates
  }
  mins<-lapply(set, FUN = function(s) min(dist(scale(s)))) #find the one with the largest minimum distance
  out<-set[[which.max(mins)]]
  print("Repelling...")
  for (n in 1:jitteration) { #do a load of iterations increase the minimum distance
    cat("|")
    set<-list()
    for (j in 1:100) {
      set[[j]]<-cbind(out[,1],jitter(out[,2],factor = jitter.fact)) #Only on y axis?
    }
    mins<-lapply(set, FUN = function(s) min(c(dist(scale(s))),na.rm = T))
    out<-set[[which.max(mins)]]
    }
  out[!is.too.close,]<-cbind(x,y)[!is.too.close,] #replace the points that aren't too close together with the originals
  return(out) #get results
  }


