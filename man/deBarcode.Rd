\name{deBarcode}
\alias{deBarcode}
\title{Decrypt Barcode}
\description{
  Decrypt barcode information.
}
\usage{
deBarcode(barcode, setnames, collapse=' & ')
}
\arguments{
 \item{barcode}{a vector of character strings, encoding the intersection combination.}
 \item{setnames}{set names.}
 \item{collapse}{an optional character string to separate the results. See \code{\link{paste}}.}
}
\details{
\code{barcode} are character strings of '0' and '1', indicating absence or presence of each set in a intersection combination.
}
\value{
A vector.
}
\author{
Minghui Wang <minghui.wang@mssm.edu>
}
\examples{
deBarcode(c('01011','10100'), c('S1','S2','S3','S4','S5'))
}
