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
 

output: rmarkdown::github_document
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



# Setups

```{r message=FALSE}
library(devtools)
library(RMySQL)
library(dplyr)
library(tidyr)
library(SummarizedExperiment)
library(ontologyIndex)
```

# Installation

The package of PulmonDB can be found in Github and intalled by using ` install_github()`

```{r}
install_github("AnaBVA/PulmonDB")
library(PulmonDB)
```

# Download data 
 
## genesPulmonDB()

The aim of this package is to download the data from PulmonDB in R environment for further analysis. The main function is `genesPulmonDB` , it will give you the homogenized sample contrast values and the annotation as `SummarizedExperiment` [object](https://bioconductor.org/packages/3.9/bioc/vignettes/SummarizedExperiment/inst/doc/SummarizedExperiment.html).


```{r}

genesPulmonDB(c("MMP1","JUND"),c("GSE1122"))

```




```{r}
# End
sessionInfo()
```
