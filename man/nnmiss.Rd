\name{nnmiss}
\alias{nnmiss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Selects k nearest neighbors and calculates weighted mean of them}
\description{
A function to select k nearest neighbors using Euclidean distance, and estimate missing value with weighted mean of selected neighbors. 
 }
 \usage{
nnmiss(x, xmiss, ismiss, K)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data frame which contains only complete cases}
  \item{xmiss}{data frame which contains incomplete cases}
  \item{ismiss}{data frame with logical value(TRUE or FALSE) of  \code{xmiss}}
  \item{K}{number of nearest neighbors}
}
\details{
Appropriate number of k is 10-20. However, we need to control k smaller in case missing rate is high, especially k is larger than the size of complete set.
}
%\value{
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
%}
%\references{ ~put references to the literature/web site here ~ }
\author{Ki-Yeol Kim and Gwan-Su Yi}
%\note{ ~~further notes~~ }
% ~Make other sections like Warning with \section{Warning }{....} ~
%\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
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
%\concept{imputation}% at least one, from doc/KEYWORDS
\keyword{classes}% at least one, from doc/KEYWORDS
% \keyword{imputation}% __ONLY ONE__ keyword per line
