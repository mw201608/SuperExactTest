\name{plot.msets}
\alias{plot.msets}
\docType{package}
\title{Draw Multi-Set Intersections}
\description{
  This function draws intersections among multiple sets.
}
\usage{
\method{plot}{msets}(x, Layout=c('circular','landscape'), degree=NULL,
	keep.empty.intersections=TRUE,
	sort.by=c('set','size','degree','p-value'),
	min.intersection.size=0, max.intersection.size=Inf,
	ylim=NULL, log.scale=FALSE, yfrac=0.8, margin=NULL,
	color.scale.pos=c(0.85, 0.9), legend.pos=c(0.85,0.25),
	legend.col=2, legend.text.cex=1, color.scale.cex=1, 
	color.scale.title=expression(paste(-Log[10],'(',italic(P),')')),
	color.on='#2EFE64', color.off='#EEEEEE', 
	show.overlap.size=TRUE, show.fold.enrichment=FALSE, show.set.size=TRUE,
	overlap.size.cex=0.9, track.area.range=0.3, bar.area.range=0.2,
	new.gridPage=TRUE, minMinusLog10PValue=0,
	maxMinusLog10PValue=NULL, show.elements=FALSE, ...)
}
\arguments{
 \item{x}{a \code{msets} object.} 
 \item{Layout}{layout for plotting.}
 \item{degree}{a vector of intersection degrees for plotting. E.g., when \code{degree=c(2:3)}, only those intersections involving two or three sets will be plotted. By default, \code{degree=NULL}, all possible intersections are plotted.}
 \item{keep.empty.intersections}{logical; if \code{FALSE}, empty intersection(s) will be discarded to save plotting space.}
 \item{min.intersection.size}{Minimum size of an intersection to be plotted.}
 \item{max.intersection.size}{Maximum size of an intersection to be plotted.}
 \item{sort.by}{how to sort intersections. It can be either one of the key words "\code{set}", "\code{size}", "\code{degree}", and "\code{p-value}", or a vector of custom ordered set combination strings.}
 \item{ylim}{the limits c(y1, y2) of plotting overlap size.}
 \item{log.scale}{logical; whether to plot with log transformed intersection sizes.}
 \item{yfrac}{numeric; the fraction (0 to 1) of canvas used for plotting bars. Only used for \code{landscape} Layout.}
 \item{margin}{numeric; a vector of 4 numeric values specifying the margins (bottom, left, top, & right) in unit of "lines". Default c(1,1,1,1)+0.1 for \code{circular} Layout and c(0.5,5,1.5,2)+0.1 for \code{landscape} Layout. Increase margin if the plot area is cropped.}
 \item{color.scale.pos}{numeric; x and y coordinates (0 to 1) for packing the color scale guide. It could be a keyword "\code{topright}" or "\code{topleft}" in the \code{landscape} layout, and one of "\code{topright}", "\code{topleft}", "\code{bottomright}" and "\code{bottomleft}" in the \code{circular} layout.}
 \item{legend.pos}{numeric; x and y coordinates (0 to 1) for packing the legend in the \code{circular} layout. It could be one of the keywords "bottomright", "bottomleft", "topleft" and "topright".}
 \item{legend.col}{integer; number of columns of the legend in the \code{circular} layout.}
 \item{legend.text.cex}{numeric; specifying the amount by which legend text should be magnified relative to the default.}
 \item{color.scale.cex}{numeric; specifying the amount by which color scale text should be magnified relative to the default.}
 \item{color.scale.title}{character or expression; a title for the color scale guide.}
 \item{color.on}{color code; specifying the color for set(s) which are "\code{present}" for an intersection. Can be a vector of colors. When \code{NULL}, a predefined list of colors will be used.}
 \item{color.off}{color code; specifying the color for set(s) which are "{absent}" for an intersection.}
 \item{show.overlap.size}{logical; whether to show overlap size on top of the bars. This will be set to \code{FALSE} if \code{show.fold.enrichment = TRUE}.}
 \item{show.fold.enrichment}{logical; whether to show fold enrichment if available rather than overlap size. This will impact \code{show.overlap.size}.}
 \item{show.set.size}{color code; whether to show set size in the \code{landscape} layout.}
 \item{overlap.size.cex}{numeric; specifying the amount by which overlap size text should be magnified relative to the default.}
 \item{track.area.range}{the magnitude of track area from origin in the \code{circular} layout.}
 \item{bar.area.range}{the magnitude of bar area from edge of the track area in the \code{circular} layout. The sum of \code{track.area.range} and \code{track.area.range} should not be larger than 0.5.}
 \item{new.gridPage}{logic; whether to start a new grid page. Set \code{FALSE} to allow for customized arrangement of the grid layout.}
 \item{minMinusLog10PValue}{numeric; minimum minus log10 P value for capping the scale of color map. Default 0.}
 \item{maxMinusLog10PValue}{numeric; maximum minus log10 P value for capping the scale of color map. Default maximum from the data.}
 \item{show.elements}{logical; whether to show the intersection elements on top of the bars with the \code{landscape} layout. See \code{Details} for more control options \code{elements.*}.}
 \item{...}{additional arguments for the plot function. See \code{Details}.}
}
\details{
 The plot canvas has coordinates 0~1 for both x and y axes. Additional optional plot parameters include:
 \itemize{
 \item \code{ylab}, a chracter string of y axis label.
 \item \code{circle.radii}, radii size of the circles in \code{landscape} Layout. Default 0.5.
 \item \code{heatmapColor}, a vector of customized heat colors.
 \item \code{show.expected.overlap}, whether to show expcted overlap in \code{landscape} Layout. Default 'FALSE'.
 \item \code{expected.overlap.style}, one of c("hatchedBox","horizBar","box"). Default 'hatchedBox'.
 \item \code{expected.overlap.lwd}, line width for expected.overlap "horizBar" and "box". Default 2.
 \item \code{color.expected.overlap}, color for showing expcted overlap in hatched lines. Default 'grey'.
 \item \code{alpha.expected.overlap}, alpha channel for transparency for showing expcted overlap hatched lines. Default 1 (normalized to  the range 0 to 1).
 \item \code{cex}, scale of text font size.
 \item \code{cex.lab}, scale of axis label text font size.
 \item \code{show.track.id}, logic, whether to show the track id in the \code{circular} layout. Default \code{TRUE}.
 \item \code{phantom.tracks}, number of phantom tracks in the middle in the \code{circular} layout. Default 2.
 \item \code{gap.within.track}, ratio of gap width over block width on the same track. Default 0.1.
 \item \code{gap.between.track}, ratio of gap width over track width. Default 0.1.
 \item \code{bar.split}, a vector of two values specifying a continuous range that will be cropped in the y axis with the \code{landscape} layout.
 \item \code{elements.list}, a data.frame or matrix such as the one generated by the \code{summary} function from a \code{msets} object, with row names matching the barcodes of intersection combinations and at least one column named "Elements" listing the elements to be displayed (the elements should be concatenated by separator ", ").
 \item \code{elements.cex}, numeric; specifying the amount by which intersection element text should be magnified. Default 0.9.
 \item \code{elements.rot}, numeric; the angle to rotate the text of intersection elements. Default 45.
 \item \code{elements.col}, colour for intersection element text. Default black.
 \item \code{elements.maximum}, maximum number of elements to show.
 \item \code{intersection.size.rotate}, logic, whether to rotate the text of intersection size.
 \item \code{flip.vertical}, logic, whether to flip the bars to downwards in \code{landscape} Layout. Default 'FALSE'.
 \item \code{title}, figure title. Default NULL.
 \item \code{cex.title}, scale of title text font size. Default 1.
 }
}
\value{
No return.
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
plot(obj)
}
}
\seealso{\code{\link{msets}}}
