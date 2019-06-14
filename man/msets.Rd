\name{msets}
\alias{msets}
\title{Class to Contain Multi-Set Intersections}
\description{
  This object contains data regarding the intersections between multiple sets. This object is usually created by the \code{supertest} function.
}
\value{
 \item{x}{a list of sets from input.}
 \item{set.names}{names of the sets. If the input sets do not have names, they will be automatically named as SetX where X is an integer from 1 to the total number of sets.}
 \item{set.sizes}{a vector of set sizes.}
 \item{n}{background population size.}
 \item{overlap.sizes}{a named vector of intersection sizes. Each intersection component is named by a barcoded character string of '0' and '1'. See \code{Details} for barcode.}
 \item{overlap.expected}{a named vector of expected intersection sizes when item \code{n} is available.}
 \item{P.value}{a vector of p values for the intersections when item \code{n} is available.}
}
\details{
Intersection combination is denoted by a \code{barcode} string of '0' and '1', where a value of '1' in the \code{i}th position of the string indicates that the intersection is involved with the \code{i}th set, 0 otherwise. E.g., string '000101' indicates that the intersection is an overlap between the 4th and 6th sets. Function \code{\link{deBarcode}} can be used to decrypt the barcode.
Generic \code{summary} and \code{plot} functions can be applied to extract and visualize the results.
}
\author{
Minghui Wang <minghui.wang@mssm.edu>, Bin Zhang <bin.zhang@mssm.edu>
}
\references{
Minghui Wang, Yongzhong Zhao, and Bin Zhang (2015). Efficient Test and Visualization of Multi-Set Intersections. \emph{Scientific Reports} 5: 16923.
}
\keyword{classes}
\seealso{\code{\link{supertest}}, \code{\link{summary.msets}}, \code{\link{plot.msets}}, \code{\link{deBarcode}}}
