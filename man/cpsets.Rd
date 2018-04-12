\name{cpsets}
\alias{cpsets}
\alias{dpsets}
\docType{package}
\title{Multi-Set Intersection Probability}
\description{
  Density and distribution function of multi-set intersection test.
}
\usage{
dpsets(x,L,n,log.p =FALSE)
cpsets(x,L,n,lower.tail=TRUE,log.p=FALSE,
       simulation.p.value=FALSE,number.simulations=1000000)
}
\arguments{
 \item{x}{integer, number of elements overlap among all sets.}
 \item{L}{vector, set sizes.} 
 \item{n}{integer, background population size.}
 \item{lower.tail}{logical; if TRUE, probability is \code{P[overlap <= x]}, otherwise, \code{P[overlap > x]}.}
 \item{log.p}{logical; if TRUE, probability p is given as \code{log(p)}.}
 \item{simulation.p.value}{logical; if TRUE, probability p is computed from simulation.}
 \item{number.simulations}{integer; number of simulations.}
}
\value{
\code{dpsets} gives the density and \code{cpsets} gives the distribution function.
}
\author{
Minghui Wang <minghui.wang@mssm.edu>
}
\references{
Minghui Wang, Yongzhong Zhao, and Bin Zhang (2015). Efficient Test and Visualization of Multi-Set Intersections. \emph{Scientific Reports} 5: 16923.
}
\seealso{\code{\link{supertest}}, \code{\link{MSET}}}
\examples{
\dontrun{
#set up fake data
n=500; A=260; B=320; C=430; D=300; x=170
(d=dpsets(x,c(A,B,C,D),n))
(p=cpsets(x,c(A,B,C,D),n,lower.tail=FALSE))
}
}