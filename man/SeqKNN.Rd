\name{SeqKNN}
\alias{SeqKNN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sequential KNN imputation method}
\description{
This function estimates missing values sequentially from the gene that has least missing rate in microarray data, using weighted mean of k nearest neighbors. This function requires 'nnmiss'.
}
\usage{
SeqKNN(data, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{matrix or dataframe, 1 row corresponds to 1 gene, 1 column to 1 sample,colnames and rownames can be used}
  \item{k}{number of nearest neighbors}
}
\details{
'SeqKNN' separates the dataset into incomplete and complete set that has or has not missing values respectively. The genes in incomplete set are imputed by the order of missing rate. Missing value is filled by the weighted mean value of corresponding column of the nearest neighbor genes in complete set. Once all missing values in a gene are imputed, the imputed gene is moved into the complete set and used for the imputation of the rest of genes in incomplete set. In this process, all missing values in one gene can be imputed simultaneously from the selected neighbor genes in complete set. This reduces execution time from previously developed KNN method that selects nearest neighbors for each imputation.
}
\references{ Ki-Yeol Kim, Byoung-Jin Kim, Gwan-Su Yi (2004.Oct.26) "Reuse of imputed data in microarray analysis increases imputation efficiency", BMC Bioinformatics 5:160.  }
\author{Ki-Yeol Kim and Gwan-Su Yi}
% \note{ ~~further notes~~ }
 % ~Make other sections like Warning with 
 % \section{Warning }{....} ~
% \seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
\dontrun{data(khan05)
imputedData<-SeqKNN(khan05,10)
}

 }
%\concept{imputation}% at least one, from doc/KEYWORDS
\keyword{classes}% at least one, from doc/KEYWORDS
% \keyword{imputation }% __ONLY ONE__ keyword per line