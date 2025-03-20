---
title: "XXXXX"
author: "Jason Hunter and Ryan Greer"
date: "2025-03-19"
output: 
  html_document:
    keep_md: yes
---



## Installing Packages(only once)

``` r
# every single install.packages() command we ran on fiji (may not be exhaustive)
options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("tidyverse")
install.packages("dplyr")
install.packages("IRanges")
install.packages("ggplot2")
install.packages("purrr")
install.packages("readr")
install.packages("tibble")
install.packages("tidyr")
install.packages("matrixStats")
install.packages("broom")
install.packages("reshape")
install.packages("reshape2")


# Install BiocManager
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.20")

BiocManager::install("DESeq2")
BiocManager::install("apeglm")
```

## Loading Required Libraries

``` r
# loading in every library we used over the semester
library(tidyverse)
library(readr)
library(DESeq2)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(IRanges)
library(purrr)
library(pheatmap)
library(textshape)
library(Rcpp)
library(tibble)
library(matrixStats)
library(broom)
library(reshape)
```

## Introduction

This document entails our project.

## Importing counts and TPM values

``` r
# loading in the genes that significantly chagned
data <- read.table("results/sig_4fold_genes_counts.tsv", header = TRUE, sep = "\t")
```

## After loading in the data, we can see that the genes that significantly changed are:

``` r
print(data$gene_name)
```

```
##  [1] "Gm16429"       "Gm13694"       "Gm45234"       "Gm45216"      
##  [5] "Gm48419"       "Aoc3"          "Abcc2"         "Lhx5"         
##  [9] "Nlrp3"         "Khdc1c"        "Krt13"         "Gm9923"       
## [13] "Apol8"         "Pgk1-rs7"      "Ppp1r3c"       "Rps12-ps9"    
## [17] "Gm13339"       "Gm14046"       "Gm13657"       "Cphx3"        
## [21] "Gm16429"       "Gm13694"       "Mir6236"       "Gm45216"      
## [25] "Gm49388"       "Gm8723"        "Gm48419"       "H19"          
## [29] "Kng1"          "Spink1"        "Khdc1c"        "Klf17"        
## [33] "Ankrd34a"      "Spn"           "Gm9923"        "Pgk1-rs7"     
## [37] "Gm13192"       "Rps12-ps9"     "Gm2897"        "Gm4852"       
## [41] "Gm7206"        "Gm14046"       "Gm4750"        "1700028K03Rik"
## [45] "Cphx3"         "Obox4-ps18"    "Gm16429"       "Gm13694"      
## [49] "Rpl31-ps15"    "Gm28439"       "Gm28438"       "Gm7558"       
## [53] "D030062O11Rik" "4930512J16Rik" "Gm4045"        "Gm45216"      
## [57] "Gm19810"       "Gm8723"        "Gm48419"       "Cyp1a1"       
## [61] "Gm16429"       "Gm13694"       "Gm49388"       "Gm48419"
```

## From this list, after some manual testing in IGV, we decided to focus on the expression of the following gene:

![Rps12-ps9](figures/Rps12-ps9.png)

``` r
df <- read.table("results/sig_4fold_genes_counts.tsv",
                 header = TRUE,
                 sep = "\t",
                 stringsAsFactors = FALSE)

# subset rows for gene_name == "Rps12-ps9"
rps12_data <- df[df$gene_name == "Rps12-ps9", ]

# inspect the resulting subset
rps12_data
```

```
##                        gene_name WT_0_1 WT_0_2 WT_0_3 WT_12_1 WT_12_2 WT_12_3
## ENSMUSG00000069862.6   Rps12-ps9  1.386   3.56  8.131   6.424  11.054  12.292
## ENSMUSG00000069862.6.1 Rps12-ps9  1.386   3.56  8.131   6.424  11.054  12.292
##                        WT_24_1 WT_24_2 WT_24_3 WT_48_1 WT_48_2 WT_48_3 WT_96_1
## ENSMUSG00000069862.6    14.614  23.266   7.787  24.525  17.151   9.021   4.598
## ENSMUSG00000069862.6.1  14.614  23.266   7.787  24.525  17.151   9.021   4.598
##                        WT_96_2 WT_96_3
## ENSMUSG00000069862.6     8.126   11.41
## ENSMUSG00000069862.6.1   8.126   11.41
```



``` r
# now we reshape data for time course analysis
rps12_long <- reshape2::melt(rps12_data,
                             id.vars = c("gene_name"),
                             variable.name = "sample",
                             value.name = "count")

## looks like there's some duplicates. Let's remove them
rps12_long <- rps12_long[!duplicated(rps12_long), ]
rps12_long
```

```
##    gene_name  sample  count
## 1  Rps12-ps9  WT_0_1  1.386
## 3  Rps12-ps9  WT_0_2  3.560
## 5  Rps12-ps9  WT_0_3  8.131
## 7  Rps12-ps9 WT_12_1  6.424
## 9  Rps12-ps9 WT_12_2 11.054
## 11 Rps12-ps9 WT_12_3 12.292
## 13 Rps12-ps9 WT_24_1 14.614
## 15 Rps12-ps9 WT_24_2 23.266
## 17 Rps12-ps9 WT_24_3  7.787
## 19 Rps12-ps9 WT_48_1 24.525
## 21 Rps12-ps9 WT_48_2 17.151
## 23 Rps12-ps9 WT_48_3  9.021
## 25 Rps12-ps9 WT_96_1  4.598
## 27 Rps12-ps9 WT_96_2  8.126
## 29 Rps12-ps9 WT_96_3 11.410
```


``` r
## Now we can extract the time point and replicate number from the sample column
rps12_long$timepoint <- gsub("WT_([0-9]+)_[0-9]+", "\\1", rps12_long$sample)
rps12_long$replicate <- gsub("WT_[0-9]+_([0-9]+)", "\\1", rps12_long$sample)
rps12_long$timepoint <- factor(rps12_long$timepoint, levels = c("0", "12", "24", "48", "96"))
```


## Calculating the mean and standard error for each time point

``` r
rps12_summary <- rps12_long %>%
  group_by(timepoint) %>%
  summarise(
            mean = mean(count),
            se = sd(count) / sqrt(n()),
            sd = sd(count),
            .groups = "drop")
View(rps12_summary)
```

## Plotting the mean and standard error for each time point

``` r
expression <- ggplot(rps12_summary, aes(x = timepoint, y = mean, group = 1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(
    title = "Rps12-ps9 Expression Across Time",
    y = "Mean Count",
    x = "Time (hours)",
    caption = "Error bars represent standard error of the mean"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
  )
expression
```

![](project_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## And a statistical analysis of expression changes
## We compare each time point to the 0 hour time point

``` r
timepoints <- c("12", "24", "48", "96")
stat_results <- data.frame()

for (tp in timepoints) {
  # Extract data for current timepoint and baseline
  tp_data <- rps12_long %>% filter(timepoint %in% c("0", tp))

  # Perform t-test
  t_test <- t.test(count ~ timepoint, data = tp_data)

  # Add results to dataframe
  stat_results <- rbind(stat_results,
                        data.frame(comparison = paste0("0 vs ", tp),
                                   p_value = t_test$p.value,
                                   fold_change = mean(tp_data$count[tp_data$timepoint == tp]) / # nolint
                                                 mean(tp_data$count[tp_data$timepoint == "0"]))) # nolint
}


print(stat_results)
```

```
##   comparison    p_value fold_change
## 1    0 vs 12 0.10652547    2.276516
## 2    0 vs 24 0.12114828    3.492162
## 3    0 vs 48 0.09049991    3.876807
## 4    0 vs 96 0.25791074    1.845530
```

``` r
# we could also compare it the other ribosomal pseudogenes
ribosomal_pseudogenes <- df[grepl("Rps.*-ps", df$gene_name), ]
```

## Heatmap Visualization

``` r
if (nrow(rps12_data) > 0) {
  sample_cols <- grep("WT_", colnames(rps12_data))
  heatmap_data <- as.matrix(rps12_data[, sample_cols])
  rownames(heatmap_data) <- rps12_data$gene_name

  pheatmap(heatmap_data,
           main = "Rps12-ps9 Expression Across Samples",
           cluster_rows = FALSE)
}
```

![](project_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
