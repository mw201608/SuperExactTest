\name{MSET}
\alias{MSET}
\docType{package}
\title{Exact Test of Multi-Set Intersection}
\description{
  Calculate FE and significance of intersection among multiple sets.
}
\usage{
MSET(x,n,lower.tail=TRUE,log.p=FALSE)
}
\arguments{
 \item{x}{list; a collection of sets.} 
 \item{n}{integer; background population size.}
 \item{lower.tail}{logical; if TRUE, probability is \code{P[overlap < m]}, otherwise, 
	\code{P[overlap >= m]}, where \code{m} is the number of elements overlap between all sets.}
 \item{log.p}{logical; if TRUE, probability p is given as log(p).}
}
\details{
This function implements an efficient statistical test for multi-set intersections. The algorithm behind this function was described in Wang et al 2015.
}
\value{
A list with the following elements:
 \item{intersects}{a vector of intersect items.}
 \item{FE}{fold enrichment of the intersection.}
 \item{p.value}{one-tail probability of observing equal to or larger than the number of intersect items.}
}
\author{
Minghui Wang <minghui.wang@mssm.edu>, Bin Zhang <bin.zhang@mssm.edu>
}
\references{
Minghui Wang, Yongzhong Zhao, and Bin Zhang (2015). Efficient Test and Visualization of Multi-Set Intersections. \emph{Scientific Reports} 5: 16923.
}
\seealso{\code{\link{supertest}}, \code{\link{cpsets}}, \code{\link{dpsets}}}
\examples{
\dontrun{
#set up fake data
x=list(S1=letters[1:20], S2=letters[10:26], S3=sample(letters,10), S4=sample(letters,10))
MSET(x, 26, FALSE)
}
}
