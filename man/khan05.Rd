\name{khan05}
\alias{khan05}
\docType{data}
\title{Khan et al.'s Small Round Blood Cell Tumor(SRBCT) data}
\description{
SRBCT dataset has 2308 genes and 63 experimental conditions, 8 Burkitt Lymphoma (BL), 23 Ewing Sarcoma (EWS), 12 neuroblastoma (NB), and 20 rhabdomyosarcoma (RMS) (Khan et al., 2001).
}
\usage{data(khan05)}
\arguments{
  \item{khan05}{data frame generated 5 percent missing entries randomly from original SRBCT data.}
}
%  \describe{
%    \item{V1}{a numeric vector}
%    \item{V2}{a numeric vector}
%    \item{V3}{a numeric vector}
%  }
}
%\details{
%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%   	Javed Khan, Jun S. Wei, Markus Ringner, Lao H. Saal, Marc Ladanyi, Frank Westermann, Frank Berthold, Manfred Schwab, Cristina R. Antonescu, Carsten Peterson, and Paul S. Meltzer (2001). Classification and diagnostic prediction of cancers using gene expression profiling and artificial neural networks. Nature Medicine, Volume 7, Number 6, June 
%}
\references{Javed Khan, Jun S. Wei, Markus Ringner, Lao H. Saal, Marc Ladanyi, Frank Westermann, Frank Berthold, Manfred Schwab, Cristina R. Antonescu, Carsten Peterson, and Paul S. Meltzer (2001). Classification and diagnostic prediction of cancers using gene expression profiling and artificial neural networks. Nature Medicine, Volume 7, Number 6, June
}
\examples{
data(khan05)
imputedKhan<-SeqKNN(khan05,10)
}
%\concept{datasets}
\keyword{datasets}