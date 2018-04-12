\name{supertest}
\alias{supertest}
\alias{supertest,list-method}
\title{Calculate Intersections Among Multiple Sets and Perform Statistical Tests}
\description{
  This function calculates intersection sizes among multiple sets and performs statistical tests of the intersections.
}
\usage{
supertest(x, n=NULL, degree=NULL, ...)
}
\arguments{
 \item{x}{list; a collection of sets.}
 \item{n}{integer, background population size. Required for computing the statistical significance of intersections.}
 \item{degree}{a vector of intersection degrees for overlap analysis. E.g., when \code{degree=c(2:3)}, only those intersections involving two or three sets will be computed. By default, \code{degree=NULL}, all possible intersections are computed.}
 \item{...}{additional arguments (not implemented).}
}
\details{
This function calculates intersection sizes between multiple sets and, if background population size \code{n} is specified, performs statistical tests of the intersections.
For a brief introduction of using this package, please see \code{vignette("set_html")}.
}
\value{
An object of class \code{\link{msets}}.
}
\author{
Minghui Wang <minghui.wang@mssm.edu>, Bin Zhang <bin.zhang@mssm.edu>
}
\references{
Minghui Wang, Yongzhong Zhao, and Bin Zhang (2015). Efficient Test and Visualization of Multi-Set Intersections. \emph{Scientific Reports} 5: 16923.
}
\seealso{\code{\link{msets}}, \code{\link{MSET}}, \code{\link{Cancer}}, \code{\link{cpsets}}, \code{\link{dpsets}}}
\examples{
\dontrun{
#Analyze the cancer gene sets
data(Cancer)
Result=supertest(Cancer, n=20687)
summary(Result)
plot(Result,degree=2:7,sort.by='size')
}
}
