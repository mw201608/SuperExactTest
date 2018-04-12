\name{intersect}
\alias{intersect}
\alias{union}
\docType{package}
\title{Set Operations}
\description{
  Performs set union and intersection on multiple input vectors.
}
\usage{
union(x, y, ...)
intersect(x, y, ...)
}
\arguments{
 \item{x, y, ...}{vectors (of the same mode) containing a sequence of items (conceptually) with no duplicated values.}
}
\details{
These functions extend the the same functions in the \code{base} package to handle more than two input vectors.
}
\value{
A vector of the same mode as x or y for intersect, and of a common mode for union. 
}
\author{
Minghui Wang <minghui.wang@mssm.edu>, Bin Zhang <bin.zhang@mssm.edu>
}
\references{
Minghui Wang, Yongzhong Zhao, and Bin Zhang (2015). Efficient Test and Visualization of Multi-Set Intersections. \emph{Scientific Reports} 5: 16923.
}
\examples{
##not run##
}
