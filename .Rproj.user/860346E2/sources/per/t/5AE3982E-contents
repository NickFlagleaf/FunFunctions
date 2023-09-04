#' Diverse lines subset
#'
#' Select a subset of lines from a larger panel that you have a kinship matrix for. It will find a subset of \emph{n} unrelated lines that have a minimum kinship between any pair.
#'
#' @param K An nxn kinship matrix.
#' @param n.sub number of lines to subset.
#' @param n.gens Number of generations of shuffling.
#' @param n.iters Number of iterations of algorithm to try. It will automatically output the best one.
#' @param verbose Print output - logical.
#'
#' @return Returns a list of objects including:
#' "Best_subset" A vector of names of the selected subset of lines from the best iteration.
#' "Best_Max_K" The maximum kinship between lines in the selected subset
#' "Max_kinships" A list of vectors of maximum kinship values at each generation of shuffling for each iteration. Use these to check you are running enough generations.
#'
#' @export
#'
#' @examples
#' library(FunFunctions)
#' #load the A kinship matrix for 599 wheat lines from the BGLR package
#' data(Wheat599_Amat)
#' subset<-div.sub(K = wheat599,n.sub = 100,n.gens = 200,n.iters = 5,verbose = T)

div.sub<-function(K,
                  n.sub=100,
                  n.gens=400,
                  n.iters=20,
                  verbose=T
                  ){
  all.id<-rownames(K)
  all.iter.sels<-list()
  all.iter.gen.maxKs<-list()
  if(verbose==T){print("Iteration best max kinship:")}
  for(n in 1:n.iters){
    start.sels<-sample(all.id)[1:n.sub]
    max.k<-max(K[start.sels,start.sels][lower.tri(K[start.sels,start.sels])])

    all.gens<-list()
    all.gens[[1]]<-start.sels
    all.gens.max.k<-c()
    all.gens.max.k[1]<-max.k

    K.nodiag<-K
    diag(K.nodiag)<- -100
    for(g in 2:n.gens){
      top.sels<-all.gens[[g-1]]
      closest.pair<-top.sels[which(K.nodiag[top.sels,top.sels]==max(K.nodiag[top.sels,top.sels]),arr.ind=TRUE)[1,]]
      to.remove<-closest.pair[which.max(apply(K.nodiag[top.sels[!top.sels%in%closest.pair],closest.pair],2,max))]#remove the line of the most related pair that is most closely related to the rest
      to.ad<-colnames(K.nodiag)[!colnames(K.nodiag)%in%top.sels] #get the lines that could be used to replace the one to remove
      line.to.add<-to.ad[which.min(apply(K.nodiag[top.sels,to.ad],2,function(x) max(x)))] #get the line that is least related to those currently in selection
      new.gen<-top.sels
      new.gen[new.gen==to.remove]<-line.to.add #Replace line to remove with the one to add
      max.k<-max(K[new.gen,new.gen][lower.tri(K[new.gen,new.gen])]) #work out the max K among selections
      all.gens.max.k[g]<-max.k
      all.gens[[g]]<-new.gen
    }
    if(verbose==T){print(paste("Iteration",n,"=",min(all.gens.max.k[g])))}

    all.iter.gen.maxKs[[n]]<-all.gens.max.k
    all.iter.sels[[n]]<-all.gens[[g]]
  }

  best.iter.sel<-all.iter.sels[[which.min(unlist(lapply(all.iter.gen.maxKs,min)))]]
  all
  best.iter.maxK<-min(all.iter.gen.maxKs[[which.min(unlist(lapply(all.iter.gen.maxKs,min)))]])
  out<-list("Best_subset"=best.iter.sel,
            "Best_Max_K"=best.iter.maxK,
            "Max_kinships"=all.iter.gen.maxKs)
  return(out)
}

