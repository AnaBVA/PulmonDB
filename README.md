Introduction to PulmonDB
================
true

Background
==========

PulmonDB is a gene expression database with Chronic Obstructive Pulmonary Diseases (COPD) and Idiopathic Pulmonary Disease (IPF) experiments. It has homogenized values using individual contrast and manual curated annotation that can be download or accessed by <http://pulmondb.liigh.unam.mx>.

Setups
======

``` r
library(devtools)
library(RMySQL)
library(dplyr)
library(tidyr)
library(SummarizedExperiment)
library(ontologyIndex)
```

Installation
============

The package of PulmonDB can be found in Github and intalled by using `install_github()`

``` r
install_github("AnaBVA/PulmonDB")
#> Skipping install of 'PulmonDB' from a github remote, the SHA1 (2935765f) has not changed since last install.
#>   Use `force = TRUE` to force installation
library(PulmonDB)
```

Download data
=============

genesPulmonDB()
---------------

The aim of this package is to download the data from PulmonDB in R environment for further analysis. The main function is `genesPulmonDB` , it will give you the homogenized sample contrast values and the annotation as `SummarizedExperiment` [object](https://bioconductor.org/packages/3.9/bioc/vignettes/SummarizedExperiment/inst/doc/SummarizedExperiment.html).

``` r

genesPulmonDB(c("MMP1","JUND"),c("GSE1122"))
#> class: SummarizedExperiment 
#> dim: 2 14 
#> metadata(0):
#> assays(1): values
#> rownames(2): JUND MMP1
#> rowData names(0):
#> colnames(14): GSM18404.ch1-vs-GSM18403.ch1
#>   GSM18405.ch1-vs-GSM18403.ch1 ... GSM18416.ch1-vs-GSM18403.ch1
#>   GSM18417.ch1-vs-GSM18403.ch1
#> colData names(3): SAMPLE_TYPE DISEASE_STATUS STUDIED_DISEASE
```

``` r
# End
sessionInfo()
#> R version 3.6.0 (2019-04-26)
#> Platform: x86_64-apple-darwin15.6.0 (64-bit)
#> Running under: macOS Mojave 10.14.6
#> 
#> Matrix products: default
#> BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
#> LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> attached base packages:
#> [1] parallel  stats4    stats     graphics  grDevices utils     datasets 
#> [8] methods   base     
#> 
#> other attached packages:
#>  [1] PulmonDB_0.1.0              ontologyIndex_2.5          
#>  [3] SummarizedExperiment_1.14.0 DelayedArray_0.10.0        
#>  [5] BiocParallel_1.18.0         matrixStats_0.54.0         
#>  [7] Biobase_2.44.0              GenomicRanges_1.36.0       
#>  [9] GenomeInfoDb_1.20.0         IRanges_2.18.1             
#> [11] S4Vectors_0.22.0            BiocGenerics_0.30.0        
#> [13] tidyr_0.8.3                 dplyr_0.8.3                
#> [15] RMySQL_0.10.17              DBI_1.0.0                  
#> [17] devtools_2.1.0              usethis_1.5.1              
#> 
#> loaded via a namespace (and not attached):
#>  [1] tidyselect_0.2.5       xfun_0.8               remotes_2.1.0         
#>  [4] purrr_0.3.2            lattice_0.20-38        testthat_2.1.1        
#>  [7] htmltools_0.3.6        yaml_2.2.0             rlang_0.4.0           
#> [10] pkgbuild_1.0.3         pillar_1.4.2           glue_1.3.1            
#> [13] withr_2.1.2            sessioninfo_1.1.1      plyr_1.8.4            
#> [16] GenomeInfoDbData_1.2.1 stringr_1.4.0          zlibbioc_1.30.0       
#> [19] memoise_1.1.0          evaluate_0.14          knitr_1.23            
#> [22] callr_3.3.0            ps_1.3.0               curl_3.3              
#> [25] Rcpp_1.0.1             backports_1.1.4        desc_1.2.0            
#> [28] pkgload_1.0.2          XVector_0.24.0         fs_1.3.1              
#> [31] digest_0.6.20          stringi_1.4.3          processx_3.4.0        
#> [34] grid_3.6.0             rprojroot_1.3-2        cli_1.1.0             
#> [37] tools_3.6.0            bitops_1.0-6           magrittr_1.5          
#> [40] RCurl_1.95-4.12        tibble_2.1.3           crayon_1.3.4          
#> [43] pkgconfig_2.0.2        Matrix_1.2-17          prettyunits_1.0.2     
#> [46] assertthat_0.2.1       rmarkdown_1.14         R6_2.4.0              
#> [49] compiler_3.6.0
```
