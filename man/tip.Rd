% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tip function.R
\name{tip}
\alias{tip}
\title{Tip}
\usage{
tip(x, nr = 15, nc = 5)
}
\arguments{
\item{x}{A two dimensional data frame or matrix object}

\item{nr}{Number of rows to display. Default=15}

\item{nc}{Number of columns to display. Default=5}
}
\value{
Displays object
}
\description{
Have a cheeky look at the top corner of a large object
}
\examples{

#Make a matrix of random numbers
mat<-matrix(round(rnorm(10000),2),100,100,
            dimnames = list(1:100,1:100))
tip(mat) #Have a look at the tip

}
