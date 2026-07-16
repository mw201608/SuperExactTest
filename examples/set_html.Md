Introduction
------------

`SuperExactTest` provides statistical methods and visualization tools
for analyzing intersections among multiple sets. This R package
implements an exact probability framework for evaluating whether
observed multi-set intersections are larger or smaller than expected by
chance (Wang et al., 2015).

Unlike pairwise overlap tests, `SuperExactTest` computes the exact
distribution of intersections involving an arbitrary number of sets
using a forward algorithm whose computational complexity increases
linearly with the number of sets. The package also includes scalable
visualization methods for exploring intersection sizes and their
statistical significance.

Citation
--------

If you use `SuperExactTest` in published work, please cite:

> Minghui Wang, Yongzhong Zhao, and Bin Zhang (2015) Efficient Test and
> Visualization of Multi-Set Intersections. *Scientific Reports* 5:
> 16923.

Case study
----------

This vignette demonstrates the main functionality of `SuperExactTest`
using expression quantitative trait loci (eQTLs) identified from four
human brain regions.

eQTLs are genomic loci whose genetic variants influence gene expression.
Because gene regulation varies across tissues, cell types, and
developmental stages, comparing eQTLs across tissues can help identify
shared and tissue-specific regulatory mechanisms.

The example dataset consists of genes with cis-eQTLs reported by Gibbs
et al. (2010). These data were originally obtained from the NCBI eQTL
Browser (now discontinued) and are included in `SuperExactTest` as a
built-in example dataset.

Load the package and example data:

    library("SuperExactTest")
    data("eqtls")

The object `cis.eqtls` is a named list containing four vectors of gene
symbols, corresponding to the cerebellum (CB), frontal cortex (FC),
temporal cortex (TC), and pons (PONS).

    str(cis.eqtls)

    ## List of 4
    ##  $ CB  : chr [1:147] "KCTD10" "CHURC1" "C10orf85" "AMFR" ...
    ##  $ FC  : chr [1:164] "CHURC1" "KCTD10" "AMFR" "SCG3" ...
    ##  $ TC  : chr [1:137] "CHURC1" "SCG3" "AMFR" "KCTD10" ...
    ##  $ PONS: chr [1:101] "CHURC1" "KCTD10" "PEX6" "HMBOX1" ...

These cis-eQTL gene sets were identified from genome-wide expression
profiling of 18,196 unique genes (Gibbs et al., 2010), which serves as
the background population throughout this vignette.

The length of the *cis*-eQTL gene sets varies from 101 to 164:

    (length.gene.sets=sapply(cis.eqtls,length))

    ##   CB   FC   TC PONS 
    ##  147  164  137  101

Assuming the *cis*-eQTL gene sets were independently and randomly
sampled from the population of 18,196 unique genes profiled in the eQTL
study, the probability of the number of common genes shared by the four
*cis*-eQTL gene sets can be computed using function `dpsets` which
implements the exact probability calculation of multi-set intersection
developed in this study.

Before testing the observed overlap, we first calculate the expected
intersection size under the null hypothesis that the four gene sets were
independently sampled from the background population.

    total=18196
    (num.expcted.overlap=total*do.call(prod,as.list(length.gene.sets/total)))

    ## [1] 5.53701e-05

Due to the large background population gene size whereas small gene set
sizes, the expected intersection size is close to 0. It is obvious that
the possible number of genes shared among the four *cis*-eQTL gene sets
is from 0 to 101. We can compute the probability density distribution of
the possible intersection sizes using the `dpsets` function:

    (p = sapply(0:101, function(i) dpsets(i, length.gene.sets, n=total)))

    ##   [1]  9.999446e-01  5.536713e-05  1.487462e-09  2.584596e-14  3.266929e-19
    ##   [6]  3.203379e-24  2.537574e-29  1.669915e-34  9.317026e-40  4.476026e-45
    ##  [11]  1.874211e-50  6.907188e-56  2.258521e-61  6.596148e-67  1.730423e-72
    ##  [16]  4.097358e-78  8.793254e-84  1.716552e-89  3.057708e-95 4.983934e-101
    ##  [21] 7.451607e-107 1.024173e-112 1.296529e-118 1.514351e-124 1.634467e-130
    ##  [26] 1.632415e-136 1.510523e-142 1.296427e-148 1.033055e-154 7.649616e-161
    ##  [31] 5.267943e-167 3.376250e-173 2.015080e-179 1.120610e-185 5.809401e-192
    ##  [36] 2.808707e-198 1.266884e-204 5.332802e-211 2.095428e-217 7.687359e-224
    ##  [41] 2.633520e-230 8.425576e-237 2.517649e-243 7.026430e-250 1.831520e-256
    ##  [46] 4.458637e-263 1.013598e-269 2.151522e-276 4.263524e-283 7.885797e-290
    ##  [51] 1.361041e-296 2.191414e-303 3.290550e-310 4.606280e-317 4.940656e-324
    ##  [56]  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00
    ##  [61]  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00
    ##  [66]  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00
    ##  [71]  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00
    ##  [76]  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00
    ##  [81]  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00
    ##  [86]  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00
    ##  [91]  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00
    ##  [96]  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00
    ## [101]  0.000000e+00  0.000000e+00

In the function call of `dpsets`, the first argument is the intersection
size, the second argument is a vector of the set sizes, and option *n*
specifies the size of the background gene population from which the gene
sets are collected. As expected, the probability density is maximized at
intersection size 0 and decreases as the intersection size increases.
When the intersection size is larger than 54, the p values become
extremely low to 0 due to computational precision. To recover the
extremely low p values, we can use the `log.p` option to return the the
p values in the logarithm space.

    (p.log = sapply(0:101, function(i) dpsets(i, length.gene.sets, n=total, log.p=TRUE)))

    ##   [1] -5.537016e-05 -9.801524e+00 -2.032619e+01 -3.128662e+01 -4.256527e+01
    ##   [6] -5.409784e+01 -6.584376e+01 -7.777512e+01 -8.987156e+01 -1.021176e+02
    ##  [11] -1.145011e+02 -1.270122e+02 -1.396430e+02 -1.523867e+02 -1.652378e+02
    ##  [16] -1.781913e+02 -1.912432e+02 -2.043898e+02 -2.176279e+02 -2.309549e+02
    ##  [21] -2.443682e+02 -2.578656e+02 -2.714454e+02 -2.851056e+02 -2.988447e+02
    ##  [26] -3.126615e+02 -3.265546e+02 -3.405230e+02 -3.545656e+02 -3.686815e+02
    ##  [31] -3.828701e+02 -3.971305e+02 -4.114621e+02 -4.258644e+02 -4.403369e+02
    ##  [36] -4.548791e+02 -4.694908e+02 -4.841716e+02 -4.989212e+02 -5.137395e+02
    ##  [41] -5.286262e+02 -5.435814e+02 -5.586049e+02 -5.736966e+02 -5.888566e+02
    ##  [46] -6.040850e+02 -6.193819e+02 -6.347473e+02 -6.501815e+02 -6.656846e+02
    ##  [51] -6.812569e+02 -6.968987e+02 -7.126103e+02 -7.283921e+02 -7.442443e+02
    ##  [56] -7.601675e+02 -7.761622e+02 -7.922288e+02 -8.083678e+02 -8.245799e+02
    ##  [61] -8.408657e+02 -8.572259e+02 -8.736611e+02 -8.901722e+02 -9.067600e+02
    ##  [66] -9.234253e+02 -9.401690e+02 -9.569922e+02 -9.738960e+02 -9.908813e+02
    ##  [71] -1.007949e+03 -1.025102e+03 -1.042339e+03 -1.059664e+03 -1.077077e+03
    ##  [76] -1.094579e+03 -1.112174e+03 -1.129863e+03 -1.147647e+03 -1.165529e+03
    ##  [81] -1.183511e+03 -1.201596e+03 -1.219786e+03 -1.238086e+03 -1.256497e+03
    ##  [86] -1.275024e+03 -1.293671e+03 -1.312442e+03 -1.331342e+03 -1.350378e+03
    ##  [91] -1.369556e+03 -1.388884e+03 -1.408371e+03 -1.428027e+03 -1.447866e+03
    ##  [96] -1.467906e+03 -1.488166e+03 -1.508678e+03 -1.529482e+03 -1.550644e+03
    ## [101] -1.572283e+03 -1.594689e+03

Next, let us compute the observed intersection among the four gene sets
and then the corresponding fold enrichment (FE) by:

    common.genes=intersect(cis.eqtls[[1]], cis.eqtls[[2]], cis.eqtls[[3]],
     cis.eqtls[[4]])
    (num.observed.overlap=length(common.genes))

    ## [1] 56

    (FE=num.observed.overlap/num.expcted.overlap)

    ## [1] 1011376

Note that we extend the built-in `R` function `intersect` to make it
capable of handling more than two input vectors.

The probability of observing exactly this intersection size is obtained
with `dpsets()`.

    dpsets(num.observed.overlap, length.gene.sets, n=total, log.p=TRUE)

    ## [1] -776.1622

Again, we use the `log.p` option to recover the extremely low p value in
the logarithm space.

The probability of observing 56 or more intersection genes can be
calculated using the cumulative probability function `cpsets`:

    cpsets(num.observed.overlap-1, length.gene.sets, n=total, lower.tail=FALSE, log.p=TRUE)

    ## [1] -776.1622

Like function `dpsets`, `cpsets` takes similar arguments as input, with
an additional argument `lower.tail` specifying which of the one-tail
probabilities to be returned. `num.observed.overlap - 1` because the
probability is *P*(*X* ≤ *x*) when `lower.tail = TRUE` while
*P*(*X* &gt; *x*) when `lower.tail = FALSE`.

The extremly low one-tail probability of the observed intersection size
underlies highly significant overlap of common *cis* regulators across
four brain regions in the present *cis*-eQTL dataset, rejecting the null
hypothesis that the *cis*-eQTL gene sets were independent random samples
from the population of 18,196 unique genes profiled.

As a convenience, the `MSET()` function performs both the intersection
calculation and the statistical test in a single step.

    fit = MSET(cis.eqtls, n=total, lower.tail=FALSE, log.p=TRUE)
    fit

    ## $intersects
    ##  [1] "CHURC1"   "KCTD10"   "PEX6"     "HMBOX1"   "TIMM10"   "GOLGB1"  
    ##  [7] "MYOM2"    "C17orf75" "INPP5E"   "NAPRT1"   "AMFR"     "ABHD12"  
    ## [13] "ATPIF1"   "NSUN2"    "WDR41"    "BRWD1"    "WDR48"    "RAB3IP"  
    ## [19] "CPEB4"    "RPS23"    "SCG3"     "SURF1"    "NSFL1C"   "CBS"     
    ## [25] "NR1D2"    "CCDC23"   "PPIL3"    "ZNF589"   "TCFL5"    "VAMP4"   
    ## [31] "SMG7"     "EIF5A"    "SEZ6L2"   "WDR55"    "CADM1"    "MTRR"    
    ## [37] "KCNK1"    "SLC2A8"   "C9orf156" "GSTM3"    "ZNF323"   "N4BP2L2" 
    ## [43] "RNF167"   "CCBL2"    "FECH"     "SQSTM1"   "VRK3"     "DGCR8"   
    ## [49] "C9orf95"  "TPCN2"    "MAP3K2"   "FN3KRP"   "FAM119B"  "CNTNAP4" 
    ## [55] "TMEM80"   "NPM1"    
    ## 
    ## $FE
    ## [1] 1011376
    ## 
    ## $p.value
    ## [1] -776.1622

### Analyzing all possible intersections among four *cis*-eQTLs gene sets

In the above, we analyzed one specific intersection that is shared by
all four gene sets. In practice, we may be also interested in the
intersections among any combinations of the gene sets, such as overlap
among any two or three gene sets in this case.

In many applications, all possible intersections—not only the
intersection shared by every set—are of interest. The `supertest()`
function enumerates all 2<sup>*n*</sup> − 1 possible intersections for
`n` input sets and performs the corresponding statistical tests
automatically.

    res = supertest(cis.eqtls, n=total)

The returned variable `res` is an object of new `S3` class `"msets"`,
which is a specifically designed list holding the analysis results and
can be processed by generic functions including `plot` and `summary`.
For example, to visualize the analysis result, we can plot the analysis
results in a circular layout as shown in the following figure:

    plot(res, sort.by="size", margin=c(2,2,2,2), color.scale.pos=c(0.85,1), legend.pos=c(0.9,0.15))

![A circular plot visualizing all possible intersections and the
corresponding statistics amongst *cis*-eQTL gene sets. ](figure/fig1-1.svg)

The circular plot summarizes all possible intersections among the four gene sets.
The four tracks in the middle represent the four gene sets, with
individual blocks showing “presence” (green) or “absence” (grey) of the
gene sets in each intersection. To denote the four gene sets with
different colors, set argument `color.on` to NULL or provide it with a
vector of user-defined colors. The height of the bars in the outer layer
is proportional to the intersection sizes, as indicated by the numbers
on the top of the bars. The color intensity of the bars represents the P
value significance of the intersections. For convenience, the minimum p value is capped at `1.0E-320`.

As the option name suggests, `sort.by="size"` instructs the
intersections to be sorted by size. Users are flexible to use different
color schemes or sort intersections by P value, set, size or degree. The
complete control options can be obtained from the help documentation by
command `help("plot.msets")`. For example, we can visualize the results
in a landscape (matrix) layout as shown in Figure which includes only
intersections among 2 to 4 sets by option `degree = 2:4`:

    plot(res, Layout="landscape", degree=2:4, sort.by="size", margin=c(0.5,5,1,2))

![A bar chart illustrating all possible intersections among *cis*-eQTL
gene sets in a matrix
layout.](figure/fig2-1.svg)

The matrix of solid and empty circles at the bottom illustrates the
“presence” (solid green) or “absence” (empty) of the gene sets in each
intersection. The numbers to the right of the matrix are set sizes. The
colored bars on the top of the matrix represent the intersection sizes
with the color intensity showing the P value significance.

The generic `summary` function can be used to summarize the analysis
results in details:

    summary(res)

    ## A msets object with 4 sets: CB FC TC PONS 
    ## Background size: 18196 
    ## Summary of intersections:
    ##            Intersections Degree Observed.Overlap Expected.Overlap           FE
    ## 0001                PONS      1              101               NA           NA
    ## 0010                  TC      1              137               NA           NA
    ## 0011           TC & PONS      2               72     0.7604418554 9.468180e+01
    ## 0100                  FC      1              164               NA           NA
    ## 0101           FC & PONS      2               74     0.9103099582 8.129099e+01
    ## 0110             FC & TC      2              108     1.2347768740 8.746519e+01
    ## 0111      FC & TC & PONS      3               68     0.0068538395 9.921446e+03
    ## 1000                  CB      1              147               NA           NA
    ## 1001           CB & PONS      2               62     0.8159485601 7.598518e+01
    ## 1010             CB & TC      2               72     1.1067817103 6.505348e+01
    ## 1011      CB & TC & PONS      3               56     0.0061433806 9.115502e+03
    ## 1100             CB & FC      2               76     1.3249065729 5.736254e+01
    ## 1101      CB & FC & PONS      3               60     0.0073541198 8.158692e+03
    ## 1110        CB & FC & TC      3               67     0.0099753902 6.716529e+03
    ## 1111 CB & FC & TC & PONS      4               56     0.0000553701 1.011376e+06
    ##            P.value                 Elements
    ## 0001            NA AATF, ABHD12, ACCS,  ...
    ## 0010            NA ABHD12, ACCS, ACP6,  ...
    ## 0011 2.102344e-138 ABHD12, ACCS, ACP6,  ...
    ## 0100            NA ABHD12, ADAD2, ADAL, ...
    ## 0101 3.399219e-136 ABHD12, AMFR, ATPIF1 ...
    ## 0110 2.081929e-212 ABHD12, ADAL, AKAP8, ...
    ## 0111 2.400407e-273 ABHD12, AMFR, ATPIF1 ...
    ## 1000            NA ABHD10, ABHD12, ACO1 ...
    ## 1001 6.348636e-109 ABHD12, AMFR, ATPIF1 ...
    ## 1010 1.165928e-120 ABHD12, ADAL, AMFR,  ...
    ## 1011 1.105986e-218 ABHD12, AMFR, ATPIF1 ...
    ## 1100 2.768833e-122 ABHD12, ADAL, AMFR,  ...
    ## 1101 6.827755e-232 ABHD12, AMFR, ATPIF1 ...
    ## 1110 1.754835e-252 ABHD12, ADAL, AMFR,  ...
    ## 1111  0.000000e+00 ABHD12, AMFR, ATPIF1 ...

The main output from the `summary` function is a data.frame, with each
row representing an intersection and its corresponding test statistics.
Details about the `summary` function output are available from the help
documentation:

    ?summary.msets

To tabulate the `summary` result into a file, use command:

    write.csv(summary(res)$Table, file="summary.table.csv", row.names=FALSE)

References
==========
> Minghui Wang, Yongzhong Zhao, and Bin Zhang (2015) Efficient Test and Visualization of Multi-Set Intersections. Scientific Reports 5: 16923.

> Gibbs et al. (2010) Abundant Quantitative Trait Loci Exist for DNA Methylation and Gene Expression in Human Brain. PLoS Genetics 6:e1000952.

