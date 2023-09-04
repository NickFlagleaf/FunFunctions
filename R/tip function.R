#' Tip
#'
#' Have a cheeky look at the top corner of a large object
#'
#' @param x A two dimensional data frame or matrix object
#' @param nr Number of rows to display. Default=15
#' @param nc Number of columns to display. Default=5
#'
#' @return Displays object
#' @export
#'
#' @examples
#'
#' #Make a matrix of random numbers
#' mat<-matrix(round(rnorm(10000),2),100,100,
#'             dimnames = list(1:100,1:100))
#' tip(mat) #Have a look at the tip
#'

tip<-function(x,nr=15,nc=5){
  x[1:nr,1:nc]
}




