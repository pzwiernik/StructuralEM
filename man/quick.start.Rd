% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quick.start.R
\name{quick.start}
\alias{quick.start}
\title{The M-step of teh EM algorithm}
\usage{
quick.start(dat)
}
\arguments{
\item{X}{The data matrix}
}
\description{
This function computes quickly the starting point
in the current version we use fastme.bal()
from teh ape package. Since the algorithm may produce negative lengths
we replace all those instances by zeros
}
\examples{

}
\keyword{EM}
\keyword{algorithm}
\keyword{structural}

