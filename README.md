## SuperExactTest [![CRAN](http://www.r-pkg.org/badges/version/SuperExactTest)](https://cran.r-project.org/package=SuperExactTest) [![Downloads](http://cranlogs.r-pkg.org/badges/SuperExactTest?color=brightgreen)](http://www.r-pkg.org/pkg/SuperExactTest)

#### Current version 1.0.6

### Description
`SuperExactTest` is an R package for statistical testing and visualization of mult-set intersections.

### Installation
`SuperExactTest` is available from `CRAN` so the simplest way to install in `R` is by running `install.packages("SuperExactTest")`.

However, the one on `CRAN` may be some versions behind the current development. To install the latest update from here in `github`, run `devtools::install_github("mw201608/SuperExactTest")` in `R`.


### Reference
[Minghui Wang, Yongzhong Zhao, and Bin Zhang (2015) Efficient Test and Visualization of Multi-Set Intersections. *Scientific Reports* 5: 16923.](https://www.nature.com/articles/srep16923)

### Vignette
Detailed description about the package and sample analysis code is available from `vignette("set_html",package="SuperExactTest")` ([link](examples/set_html.Md)) after installation.

### Example
```
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
	elements.list=subset(summary(Result)$Table,Observed.Overlap <= 20))
#dev.off()
```
<img src="examples/ex1.png" width="600" alt="sample output" />
