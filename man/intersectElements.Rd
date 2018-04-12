\name{intersectElements}
\alias{intersectElements}
\title{Find Intersection Membership}
\description{
  Find intersections and assign element to intersection combinations.
}
\usage{
intersectElements(x, mutual.exclusive=TRUE)
}
\arguments{
 \item{x}{list; a collection of sets.}
 \item{mutual.exclusive}{logical; see \code{Details}.}
}
\details{
See example below for the use of \code{mutual.exclusive}.
}
\value{
A data.frame with two columns:
 \item{Entry}{set elements.}
 \item{barcode}{intersection combination that each entry belongs to.}
}
\author{
Minghui Wang <minghui.wang@mssm.edu>
}
\examples{
set.seed(123)
sets=list(S1=sample(letters,10), S2=sample(letters,5), S3=sample(letters,7))
intersectElements(sets,mutual.exclusive=TRUE)
intersectElements(sets,mutual.exclusive=FALSE)
}
