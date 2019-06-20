## SuperExactTest [![CRAN](http://www.r-pkg.org/badges/version/SuperExactTest)](https://cran.r-project.org/package=SuperExactTest) [![Downloads](http://cranlogs.r-pkg.org/badges/SuperExactTest?color=brightgreen)](http://www.r-pkg.org/pkg/SuperExactTest) [![Total downloads]( https://cranlogs.r-pkg.org/badges/grand-total/SuperExactTest?color=brightgreen)](http://www.r-pkg.org/pkg/SuperExactTest)

#### Current version 1.0.7

### Description
`SuperExactTest` is an R package for statistical testing and visualization of mult-set intersections.

### Installation
`SuperExactTest` is available from `CRAN` so the simplest way to install in `R` is by running `install.packages("SuperExactTest")`.

To install the latest update from here in `github`, run `devtools::install_github("mw201608/SuperExactTest")` in `R`.

### Reference
[Minghui Wang, Yongzhong Zhao, and Bin Zhang (2015) Efficient Test and Visualization of Multi-Set Intersections. *Scientific Reports* 5: 16923.](https://www.nature.com/articles/srep16923)

### Vignette
Detailed description about the package and sample analysis code is available from `vignette("set_html",package="SuperExactTest")` ([link](examples/set_html.Md)) after installation.

### Example
```
library(SuperExactTest)
set.seed(1234)
#generate random strings
n=400
r_strings <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
input=lapply(c(40,60,80,200),function(x,s) sample(s,x),s=r_strings)
Result=supertest(input,n=n)
#plot the intersections with a split y-axis
#show elements of the intersections with no more than 20 elements
#png('examples/ex1.png',width=2000,height=2000,res=300)
plot(Result, Layout="landscape", sort.by="size", keep=FALSE,
	bar.split=c(70,180), show.elements=TRUE, elements.cex=0.7,
	elements.list=subset(summary(Result)$Table,Observed.Overlap <= 20),
	show.expected.overlap=TRUE,expected.overlap.style="hatchedBox",
	color.expected.overlap='red')
#dev.off()
```
<img src="examples/ex1.png" width="600" alt="sample output" />

As of version 1.0.7, we can change the order of the intersection bars in a customized way through option `sort.by`.
For example, let us switch the orders of the fourth and fifth bars in the above figure.
```
#First extract the intersection barcodes ordered by size
order1=names(sort(Result$overlap.sizes,decreasing=TRUE))
#Then switch the orders of the fourth and fifth element
order2=order1[c(1:3,5,4,6:length(order1))]
print(order1)
print(order2)
#Now plot with the new order
#png('examples/ex2.png',width=2000,height=2000,res=300)
plot(Result, Layout="landscape", sort.by=order2, keep=FALSE,
        bar.split=c(70,180), show.elements=TRUE, elements.cex=0.7,
        elements.list=subset(summary(Result)$Table,Observed.Overlap <= 20),
        show.expected.overlap=TRUE,expected.overlap.style="hatchedBox",
        color.expected.overlap='red')
#dev.off()
```
<img src="examples/ex2.png" width="600" alt="sample output" />
