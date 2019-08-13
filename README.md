---
title: "Introduction to PulmonDB"
author: 
- name: Ana Beatriz Villasenor-Altamirano
  affiliation:
    - Laboratorio Internacional de Investigacion sobre el Genoma Humano (LIIGH), UNAM
- name: Oscar Aldana Assad
  affiliation:
    - Laboratorio Internacional de Investigacion sobre el Genoma Humano (LIIGH), UNAM
- name: Alejandra Medina-Rivera
  affiliation:
    - Laboratorio Internacional de Investigacion sobre el Genoma Humano (LIIGH), UNAM
 

output:   md_document:
    variant: markdown_github
vignette: >
  %\VignetteIndexEntry{intro-PulmonDB}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```



# Background

PulmonDB is a gene expression database with Chronic Obstructive Pulmonary Diseases (COPD) and Idiopathic Pulmonary Disease (IPF) experiments. It has homogenized values using individual contrast and manual curated annotation that can be download or accessed by http://pulmondb.liigh.unam.mx. 

![](./Figures/pulmondb.jpg)

# About the package 

This package has been created to download the information available in PulmonDB. It uses MySQL queries to access the database by [RMySQL library](https://cran.r-project.org/web/packages/RMySQL/index.html). Then, the data is manipulated with [dplyr](https://dplyr.tidyverse.org/) package from [tidyverse](https://www.tidyverse.org/) to create a `SummarizedExperiment` [object](https://bioconductor.org/packages/3.9/bioc/vignettes/SummarizedExperiment/inst/doc/SummarizedExperiment.html) that will contain values and/or curated annotation. 


# Setups

```{r message=FALSE}
library(ggplot2)
library(SummarizedExperiment)
```

# Installation

The package of PulmonDB can be found in Git hub and installed using ` install_github()`

```{r}
# If needed, use install.packages("remotes")
# remotes::install_github("AnaBVA/PulmonDB")
library(PulmonDB)
```

# Download data 
 
## genesPulmonDB()

The aim of this package is to download the data from PulmonDB in R environment for further analysis. The main function is `genesPulmonDB` , it will give you the homogenized sample contrast values and the annotation as `SummarizedExperiment` [object](https://bioconductor.org/packages/3.9/bioc/vignettes/SummarizedExperiment/inst/doc/SummarizedExperiment.html). 

The first argument must be genes, can be one or a vector (gene names are based on Gencode v25). The second argument is the GSE id from [GEO](https://www.ncbi.nlm.nih.gov/geo/browse/). The available GSEs can be found in the [supplementary table 2](https://www.biorxiv.org/content/10.1101/726745v1.full).

```{r}

genesPulmonDB(c("MMP1","JUND"),c("GSE1122"))

```
With `genesPulmonDB` function, all homogenazed values can be downloaded using *'all'* instead of genes.

```{r}

genesPulmonDB("all",c("GSE475"))

```

`SummarizedExperiment` objects generated from PulmonDB hold homogenized values that can be access using:

```{r}

GSE1122 <- genesPulmonDB(c("MMP1","JUND"),c("GSE1122"))

assay(GSE1122)

#Other opption
#assays(GSE1122)$values

```

The annotation of `SummarizedExperiment` object can be accessed by using:


```{r}

colData(GSE1122)

```

## annotationPulmonDB

Another function provided in this package is `annotationPulmonDB` that will return the annotation manually curated for [PulmonDB](http://pulmondb.liigh.unam.mx/), further details can be found in the [paper](https://www.biorxiv.org/content/10.1101/726745v1.full). 

The annotation by default will return a `data.frame` in which columns are the annotated features and rows are sample contrast (Test vs Reference). The annotation by contrast will be  separated by "_vs_" (EMPHYSEMA_vs_HEALTHY/CONTROL), the first annotaion corresponds to the test and the second one to the reference sample.

```{r}

a_GSE52463 <-annotationPulmonDB("GSE52463")
#a_GSE52463 <-annotationPulmonDB("GSE52463","contrast")

head(a_GSE52463)

```

Alternatively, `annotationPulmonDB` can return the annotation by sample usign "sample" as a second parameter into the function. Columns are annotated features and rows samples using GSM (GEO id for samples).

```{r}

as_GSE52463 <- annotationPulmonDB("GSE52463","sample")

head(as_GSE52463)

```


## FOSB

Using FOSB as an example, we can access the gene expression of multiple experiments that were obtained from lung tissue.


```{r}
g <- "FOSB"
gse <- c("GSE52463","GSE63073","GSE1122","GSE72073","GSE24206","GSE29133","GSE37768")

b <- genesPulmonDB(g,gse)

```

Then, we process the data and plot it using `ggplot2`. The boxplot is filled by DISEASE (COPD, IPF or CONTROL). FOSB was shown to be differentially expressed between the two conditions in the Figure 4 of the [paper](https://www.biorxiv.org/content/10.1101/726745v1.full)

```{r}
# Transform the data to have 
# rows as contrast samples and columns as genes
v <- data.frame(t(assay(b)))
colnames(v) <- "FOSB"

# Select annotation from "DISEASE_STATUS"
d <- data.frame(colData(b)[,"DISEASE_STATUS"])
rownames(d) <- rownames(colData(b))
colnames(d) <- "DISEASE_STATUS"

# Merge the data.frames
h <- merge(v,d, by = "row.names")

# Rownames of new object "h"
rownames(h) <- h[,1]

# Remove rownames as a column
h <- data.frame(h[,-1])

# Adding labels of "IPF" or "COPD" to plot colours
h$DISEASE <- "CONTROL"
h$DISEASE[grep("AAD|COPD|EMPHYSEMA",h$DISEASE_STATUS)] <- "COPD"
h$DISEASE[grep("IPF",h$DISEASE_STATUS)] <- "IPF"
h$DISEASE <- factor(h$DISEASE)

# Plot the results as a boxplot
ggplot(h,aes(x = reorder(DISEASE_STATUS,FOSB,FUN = median),y = FOSB,fill= DISEASE)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#ffffff","#9e1919","#06bd9b")) +
  geom_jitter(position=position_jitter(0.2),alpha = 0.4) +
  theme_classic(base_size = 14) +
  labs(x = "DISEASE_STATUS") +
  coord_flip() 

```

```{r}
# End
sessionInfo()

options(width = 120)
sessioninfo::session_info()
```

