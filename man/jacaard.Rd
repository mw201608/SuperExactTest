\name{jaccard}
\alias{jaccard}
\docType{package}
\title{Calculate Jaccard Index}
\description{
  This function calculates Jaccard indices between pairs of sets.
}
\usage{
jaccard(x)
}
\arguments{
 \item{x}{list, a collect of sets.} 
}
\value{
A matrix of pairwise Jaccard indices.
}
\author{
Minghui Wang <minghui.wang@mssm.edu>
}
\examples{
\dontrun{
#set up fake data
x=list(S1=letters[1:20], S2=letters[10:26], S3=sample(letters,10), S4=sample(letters,10))
jaccard(x)
}
}