\name{summary.msets}
\alias{summary.msets}
\docType{package}
\title{Summarize an msets Object}
\description{
  This function outputs summary statistics of a msets object.
}
\usage{
\method{summary}{msets}(object, degree=NULL, ...)
}
\arguments{
 \item{object}{a \code{msets} object.} 
 \item{degree}{a vector of intersection degrees to pull out.}
 \item{...}{additional arguments (not implemented).}
}
\value{
A list:
 \item{Barcode}{a vector of 0/1 character strings, representing the set composition of each intersection.}
 \item{otab}{a vector of observed intersection size between any combination of sets.}
 \item{etab}{a vector of expected intersection size between any combination of sets if background population size is specified.}
 \item{set.names}{set names.}
 \item{set.sizes}{set sizes.}
 \item{n}{background population size.}
 \item{P.value}{upper tail p value for each intersection if background population size n is specified.}
 \item{Table}{a data.frame containing degree, otab, etab, fold change, p value and the overlap elements.}
}
\author{
Minghui Wang <minghui.wang@mssm.edu>, Bin Zhang <bin.zhang@mssm.edu>
}
\references{
Minghui Wang, Yongzhong Zhao, and Bin Zhang (2015). Efficient Test and Visualization of Multi-Set Intersections. \emph{Scientific Reports} 5: 16923.
}
\examples{
\dontrun{
#set up fake data
x=list(S1=letters[1:20], S2=letters[10:26], S3=sample(letters,10), S4=sample(letters,10))
obj=supertest(x,n=26)
summary(obj)
}
}
\seealso{\code{\link{msets}}}
