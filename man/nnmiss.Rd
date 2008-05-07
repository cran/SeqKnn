\name{nnmiss}
\alias{nnmiss}
\title{Selects k nearest neighbors and calculates weighted mean of them}
\description{
A function to select k nearest neighbors using Euclidean distance, and estimate missing value with weighted mean of selected neighbors. 
 }
 \usage{
nnmiss(x, xmiss, ismiss, K)
}
\arguments{
  \item{x}{data frame which contains only complete cases}
  \item{xmiss}{data frame which contains incomplete cases}
  \item{ismiss}{data frame with logical value(TRUE or FALSE) of  \code{xmiss}}
  \item{K}{number of nearest neighbors}
}
\details{
Appropriate number of k is 10-20. However, we need to control k smaller in case missing rate is high, especially k is larger than the size of complete set.
}
\examples{
\dontrun{
    data(khan05)
    x <- as.matrix(khan05)
    N <- dim(x)
    p <- N[2]
    N <- N[1]
    nas <- is.na(drop(x \%*\% rep(1, p)))
    xcomplete <- x[!nas, ]           ## complete set
    xbad <- x[nas, , drop = FALSE]       ## incomplete set
    xnas <- is.na(xbad)       
    xbadhat <- xbad
    xbadhat[1,]<-nnmiss(xcomplete, xbad[1,], xnas[1,], 10)
}
}
\keyword{classes}% at least one, from doc/KEYWORDS
