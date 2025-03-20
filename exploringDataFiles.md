---
title: "Exploring Data Files"
author: "Jason Hunter and Ryan Greer"
date: "2025-03-19"
output: 
  html_document:
    keep_md: yes
---



## Introduction

This document explores the data files in the `results` and `data` folders.

## Loading Required Libraries

``` r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
## ✔ purrr     1.0.4     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(readr)
```


![image](igvimage.png)

#### Read in rlog_counts_all.rds


``` r
rlog_counts_all <- readRDS("data/rlog_counts_all.rds")
head(rlog_counts_all)
```

```
##                          WT_0_1    WT_0_2    WT_0_3   WT_12_1   WT_12_2
## ENSMUSG00000000001.4  12.088138 12.044057 12.091983 12.099517 12.119256
## ENSMUSG00000000003.15  0.000000  0.000000  0.000000  0.000000  0.000000
## ENSMUSG00000000028.15 11.197783 11.164766 11.199549 11.089259 11.161860
## ENSMUSG00000000031.16  7.946821  7.949234  7.839281  7.936920  7.827947
## ENSMUSG00000000037.17  6.134868  6.192025  6.261958  6.194548  6.205843
## ENSMUSG00000000049.11 -1.489179 -1.483911 -1.489815 -1.489875 -1.488933
##                         WT_12_3   WT_24_1   WT_24_2   WT_24_3   WT_48_1
## ENSMUSG00000000001.4  12.115069 12.067230 12.091940 12.067092 12.082349
## ENSMUSG00000000003.15  0.000000  0.000000  0.000000  0.000000  0.000000
## ENSMUSG00000000028.15 11.195988 11.135160 11.179804 11.167671 11.226624
## ENSMUSG00000000031.16  7.927084  8.273941  8.386485  8.372208  8.995030
## ENSMUSG00000000037.17  6.243509  6.008827  6.156208  6.082642  6.031924
## ENSMUSG00000000049.11 -1.488526 -1.488755 -1.483091 -1.488349 -1.489117
##                         WT_48_2   WT_48_3   WT_96_1   WT_96_2   WT_96_3
## ENSMUSG00000000001.4  12.074756 12.128427 12.080032 12.142432 12.125406
## ENSMUSG00000000003.15  0.000000  0.000000  0.000000  0.000000  0.000000
## ENSMUSG00000000028.15 11.187295 11.213695 11.216637 11.236676 11.232707
## ENSMUSG00000000031.16  9.100390  8.839909  8.939234  8.863632  8.761878
## ENSMUSG00000000037.17  6.096918  6.061240  6.035706  6.083772  6.142024
## ENSMUSG00000000049.11 -1.488591 -1.471382 -1.489624 -1.488506 -1.489479
```

``` r
summary(rlog_counts_all)
```

```
##      WT_0_1           WT_0_2           WT_0_3          WT_12_1      
##  Min.   :-2.257   Min.   :-2.257   Min.   :-2.257   Min.   :-2.257  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.000   Median : 0.000   Median : 0.000   Median : 0.000  
##  Mean   : 2.296   Mean   : 2.296   Mean   : 2.296   Mean   : 2.297  
##  3rd Qu.: 4.606   3rd Qu.: 4.602   3rd Qu.: 4.608   3rd Qu.: 4.616  
##  Max.   :18.760   Max.   :18.709   Max.   :18.773   Max.   :18.530  
##     WT_12_2          WT_12_3          WT_24_1          WT_24_2      
##  Min.   :-2.257   Min.   :-2.257   Min.   :-2.257   Min.   :-2.257  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.000   Median : 0.000   Median : 0.000   Median : 0.000  
##  Mean   : 2.297   Mean   : 2.297   Mean   : 2.298   Mean   : 2.299  
##  3rd Qu.: 4.617   3rd Qu.: 4.617   3rd Qu.: 4.591   3rd Qu.: 4.605  
##  Max.   :18.573   Max.   :18.596   Max.   :18.641   Max.   :18.630  
##     WT_24_3          WT_48_1          WT_48_2          WT_48_3      
##  Min.   :-2.257   Min.   :-2.257   Min.   :-2.257   Min.   :-2.257  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.000   Median : 0.000   Median : 0.000   Median : 0.000  
##  Mean   : 2.299   Mean   : 2.298   Mean   : 2.299   Mean   : 2.299  
##  3rd Qu.: 4.594   3rd Qu.: 4.598   3rd Qu.: 4.595   3rd Qu.: 4.595  
##  Max.   :18.809   Max.   :18.844   Max.   :18.887   Max.   :18.913  
##     WT_96_1          WT_96_2          WT_96_3      
##  Min.   :-2.257   Min.   :-2.257   Min.   :-2.257  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.000   Median : 0.000   Median : 0.000  
##  Mean   : 2.298   Mean   : 2.298   Mean   : 2.297  
##  3rd Qu.: 4.596   3rd Qu.: 4.592   3rd Qu.: 4.592  
##  Max.   :18.754   Max.   :18.825   Max.   :18.817
```

#### Read in rlog_counts_mouse_dox_long.rds

``` r
rlog_counts_mouse_dox_long <- readRDS("data/rlog_counts_mouse_dox_long.rds")
head(rlog_counts_mouse_dox_long)
```

```
##                          WT_0_1    WT_0_2    WT_0_3   WT_12_1   WT_12_2
## ENSMUSG00000000001.4  12.088138 12.044057 12.091983 12.099517 12.119256
## ENSMUSG00000000028.15 11.197783 11.164766 11.199549 11.089259 11.161860
## ENSMUSG00000000031.16  7.946821  7.949234  7.839281  7.936920  7.827947
## ENSMUSG00000000037.17  6.134868  6.192025  6.261958  6.194548  6.205843
## ENSMUSG00000000049.11 -1.489179 -1.483911 -1.489815 -1.489875 -1.488933
## ENSMUSG00000000056.7   9.211715  9.204328  9.230460  9.184674  9.198976
##                         WT_12_3   WT_24_1   WT_24_2   WT_24_3   WT_48_1
## ENSMUSG00000000001.4  12.115069 12.067230 12.091940 12.067092 12.082349
## ENSMUSG00000000028.15 11.195988 11.135160 11.179804 11.167671 11.226624
## ENSMUSG00000000031.16  7.927084  8.273941  8.386485  8.372208  8.995030
## ENSMUSG00000000037.17  6.243509  6.008827  6.156208  6.082642  6.031924
## ENSMUSG00000000049.11 -1.488526 -1.488755 -1.483091 -1.488349 -1.489117
## ENSMUSG00000000056.7   9.203715  9.097634  9.109582  9.107368  9.165726
##                         WT_48_2   WT_48_3   WT_96_1   WT_96_2   WT_96_3
## ENSMUSG00000000001.4  12.074756 12.128427 12.080032 12.142432 12.125406
## ENSMUSG00000000028.15 11.187295 11.213695 11.216637 11.236676 11.232707
## ENSMUSG00000000031.16  9.100390  8.839909  8.939234  8.863632  8.761878
## ENSMUSG00000000037.17  6.096918  6.061240  6.035706  6.083772  6.142024
## ENSMUSG00000000049.11 -1.488591 -1.471382 -1.489624 -1.488506 -1.489479
## ENSMUSG00000000056.7   9.212129  9.150540  9.073101  9.086099  9.045839
```

``` r
summary(rlog_counts_mouse_dox_long)
```

```
##      WT_0_1             WT_0_2              WT_0_3            WT_12_1        
##  Min.   :-2.06472   Min.   :-2.064961   Min.   :-2.06507   Min.   :-2.06510  
##  1st Qu.: 0.01049   1st Qu.: 0.009834   1st Qu.: 0.01387   1st Qu.: 0.01206  
##  Median : 3.89481   Median : 3.885049   Median : 3.87634   Median : 3.89189  
##  Mean   : 4.46116   Mean   : 4.461037   Mean   : 4.46042   Mean   : 4.46235  
##  3rd Qu.: 8.93421   3rd Qu.: 8.932073   3rd Qu.: 8.93455   3rd Qu.: 8.93075  
##  Max.   :18.75974   Max.   :18.709301   Max.   :18.77326   Max.   :18.52959  
##     WT_12_2            WT_12_3            WT_24_1            WT_24_2        
##  Min.   :-2.06458   Min.   :-2.06435   Min.   :-2.06448   Min.   :-2.06451  
##  1st Qu.: 0.01505   1st Qu.: 0.01237   1st Qu.: 0.01066   1st Qu.: 0.01436  
##  Median : 3.88447   Median : 3.88537   Median : 3.88294   Median : 3.88499  
##  Mean   : 4.46176   Mean   : 4.46259   Mean   : 4.46427   Mean   : 4.46562  
##  3rd Qu.: 8.92931   3rd Qu.: 8.92993   3rd Qu.: 8.93545   3rd Qu.: 8.93919  
##  Max.   :18.57329   Max.   :18.59596   Max.   :18.64124   Max.   :18.63001  
##     WT_24_3            WT_48_1            WT_48_2            WT_48_3        
##  Min.   :-2.06425   Min.   :-2.06468   Min.   :-2.06439   Min.   :-2.06434  
##  1st Qu.: 0.01111   1st Qu.: 0.00898   1st Qu.: 0.01569   1st Qu.: 0.01196  
##  Median : 3.88550   Median : 3.88174   Median : 3.88731   Median : 3.88735  
##  Mean   : 4.46586   Mean   : 4.46491   Mean   : 4.46579   Mean   : 4.46548  
##  3rd Qu.: 8.93677   3rd Qu.: 8.94558   3rd Qu.: 8.94509   3rd Qu.: 8.94285  
##  Max.   :18.80877   Max.   :18.84403   Max.   :18.88697   Max.   :18.91329  
##     WT_96_1             WT_96_2            WT_96_3        
##  Min.   :-2.064965   Min.   :-2.06434   Min.   :-2.06488  
##  1st Qu.: 0.009829   1st Qu.: 0.01137   1st Qu.: 0.01288  
##  Median : 3.877085   Median : 3.87687   Median : 3.87874  
##  Mean   : 4.464491   Mean   : 4.46437   Mean   : 4.46272  
##  3rd Qu.: 8.933543   3rd Qu.: 8.93577   3rd Qu.: 8.93112  
##  Max.   :18.753650   Max.   :18.82499   Max.   :18.81652
```

#### Read in count_files.RData

``` r
count_files <- load("data/count_files.RData")
count_files
```

```
## [1] "counts_filtered" "counts_matrix"   "g2s"
```

``` r
## this loaded in the following objects:
# counts_filtered
# counts_matrix
# g2s
```

#### Checking out the counts_filtered object

``` r
head(counts_filtered)
```

```
##                       WT_0_1 WT_0_2 WT_0_3 WT_12_1 WT_12_2 WT_12_3 WT_24_1
## ENSMUSG00000000001.4    4574   5101   5738    5906    4331    3740    3859
## ENSMUSG00000000028.15   2482   2797   3107    2828    2193    1971    2003
## ENSMUSG00000000031.16    174    208    183     225     123     128     256
## ENSMUSG00000000037.17     75    102    129     112      84      81      41
## ENSMUSG00000000049.11      0      1      0       0       0       0       0
## ENSMUSG00000000056.7     644    744    822     795     582     508     482
##                       WT_24_2 WT_24_3 WT_48_1 WT_48_2 WT_48_3 WT_96_1 WT_96_2
## ENSMUSG00000000001.4     4019    3347    4450    3672    3763    5302    3818
## ENSMUSG00000000028.15    2132    1798    2503    1998    1993    2958    2042
## ENSMUSG00000000031.16     306     256     722     681     493     804     507
## ENSMUSG00000000037.17      70      47      52      54      46      64      50
## ENSMUSG00000000049.11       1       0       0       0       3       0       0
## ENSMUSG00000000056.7      497     423     596     525     472     635     435
##                       WT_96_3
## ENSMUSG00000000001.4     5277
## ENSMUSG00000000028.15    2859
## ENSMUSG00000000031.16     624
## ENSMUSG00000000037.17      85
## ENSMUSG00000000049.11       0
## ENSMUSG00000000056.7      583
```

``` r
summary(counts_filtered)
```

```
##      WT_0_1             WT_0_2           WT_0_3          WT_12_1      
##  Min.   :     0.0   Min.   :     0   Min.   :     0   Min.   :     0  
##  1st Qu.:     1.0   1st Qu.:     1   1st Qu.:     1   1st Qu.:     1  
##  Median :    16.0   Median :    18   Median :    19   Median :    21  
##  Mean   :   971.9   Mean   :  1129   Mean   :  1220   Mean   :  1218  
##  3rd Qu.:   513.5   3rd Qu.:   597   3rd Qu.:   639   3rd Qu.:   655  
##  Max.   :471029.0   Max.   :522800   Max.   :596451   Max.   :478504  
##     WT_12_2            WT_12_3            WT_24_1            WT_24_2        
##  Min.   :     0.0   Min.   :     0.0   Min.   :     0.0   Min.   :     0.0  
##  1st Qu.:     1.0   1st Qu.:     1.0   1st Qu.:     1.0   1st Qu.:     1.0  
##  Median :    15.0   Median :    13.0   Median :    13.0   Median :    14.0  
##  Mean   :   880.1   Mean   :   767.8   Mean   :   856.9   Mean   :   864.7  
##  3rd Qu.:   471.5   3rd Qu.:   409.0   3rd Qu.:   444.5   3rd Qu.:   452.5  
##  Max.   :359297.0   Max.   :318700.0   Max.   :361164.0   Max.   :362709.0  
##     WT_24_3            WT_48_1            WT_48_2            WT_48_3        
##  Min.   :     0.0   Min.   :     0.0   Min.   :     0.0   Min.   :     0.0  
##  1st Qu.:     1.0   1st Qu.:     1.0   1st Qu.:     1.0   1st Qu.:     1.0  
##  Median :    12.0   Median :    15.0   Median :    12.0   Median :    12.0  
##  Mean   :   774.3   Mean   :   988.4   Mean   :   827.5   Mean   :   806.2  
##  3rd Qu.:   387.0   3rd Qu.:   508.0   3rd Qu.:   425.0   3rd Qu.:   414.0  
##  Max.   :713810.0   Max.   :500595.0   Max.   :434073.0   Max.   :432070.0  
##     WT_96_1            WT_96_2            WT_96_3      
##  Min.   :     0.0   Min.   :     0.0   Min.   :     0  
##  1st Qu.:     1.0   1st Qu.:     1.0   1st Qu.:     1  
##  Median :    17.0   Median :    12.0   Median :    17  
##  Mean   :  1167.5   Mean   :   796.8   Mean   :  1113  
##  3rd Qu.:   604.5   3rd Qu.:   410.0   3rd Qu.:   571  
##  Max.   :547223.0   Max.   :396618.0   Max.   :553204
```

#### Checking out the counts_matrix object

``` r
head(counts_matrix)
```

```
##                         WT_0_1   WT_0_2   WT_0_3  WT_12_1  WT_12_2 WT_12_3
## ENSMUSG00000000001.4  4574.000 5101.000 5738.000 5906.000 4331.000    3740
## ENSMUSG00000000003.15    0.000    0.000    0.000    0.000    0.000       0
## ENSMUSG00000000028.15 2482.001 2796.999 3107.000 2828.000 2193.000    1971
## ENSMUSG00000000031.16  174.000  208.000  183.000  225.000  123.000     128
## ENSMUSG00000000037.17   75.000  102.000  128.999  111.999   83.999      81
## ENSMUSG00000000049.11    0.000    1.000    0.000    0.000    0.000       0
##                        WT_24_1  WT_24_2  WT_24_3 WT_48_1  WT_48_2 WT_48_3
## ENSMUSG00000000001.4  3859.000 4019.000 3347.000    4450 3672.000    3763
## ENSMUSG00000000003.15    0.000    0.000    0.000       0    0.000       0
## ENSMUSG00000000028.15 2002.999 2132.000 1798.000    2503 1998.000    1993
## ENSMUSG00000000031.16  256.000  305.999  256.001     722  680.999     493
## ENSMUSG00000000037.17   41.000   70.000   47.000      52   54.000      46
## ENSMUSG00000000049.11    0.000    1.000    0.000       0    0.000       3
##                       WT_96_1  WT_96_2 WT_96_3
## ENSMUSG00000000001.4     5302 3818.000    5277
## ENSMUSG00000000003.15       0    0.000       0
## ENSMUSG00000000028.15    2958 2042.000    2859
## ENSMUSG00000000031.16     804  506.999     624
## ENSMUSG00000000037.17      64   50.000      85
## ENSMUSG00000000049.11       0    0.000       0
```

``` r
summary(counts_matrix)
```

```
##      WT_0_1             WT_0_2             WT_0_3            WT_12_1        
##  Min.   :     0.0   Min.   :     0.0   Min.   :     0.0   Min.   :     0.0  
##  1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0  
##  Median :     0.0   Median :     0.0   Median :     0.0   Median :     0.0  
##  Mean   :   526.7   Mean   :   611.9   Mean   :   661.2   Mean   :   659.8  
##  3rd Qu.:    26.0   3rd Qu.:    29.0   3rd Qu.:    31.8   3rd Qu.:    35.0  
##  Max.   :471028.9   Max.   :522799.8   Max.   :596450.7   Max.   :478504.2  
##     WT_12_2            WT_12_3            WT_24_1            WT_24_2        
##  Min.   :     0.0   Min.   :     0.0   Min.   :     0.0   Min.   :     0.0  
##  1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0  
##  Median :     0.0   Median :     0.0   Median :     0.0   Median :     0.0  
##  Mean   :   476.9   Mean   :   416.0   Mean   :   464.3   Mean   :   468.5  
##  3rd Qu.:    24.0   3rd Qu.:    21.3   3rd Qu.:    21.0   3rd Qu.:    22.9  
##  Max.   :359297.4   Max.   :318699.9   Max.   :361164.2   Max.   :362709.0  
##     WT_24_3            WT_48_1            WT_48_2            WT_48_3        
##  Min.   :     0.0   Min.   :     0.0   Min.   :     0.0   Min.   :     0.0  
##  1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0  
##  Median :     0.0   Median :     0.0   Median :     0.0   Median :     0.0  
##  Mean   :   419.5   Mean   :   535.6   Mean   :   448.4   Mean   :   436.8  
##  3rd Qu.:    19.0   3rd Qu.:    24.7   3rd Qu.:    20.4   3rd Qu.:    20.0  
##  Max.   :713810.4   Max.   :500595.2   Max.   :434072.8   Max.   :432069.8  
##     WT_96_1            WT_96_2            WT_96_3        
##  Min.   :     0.0   Min.   :     0.0   Min.   :     0.0  
##  1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0  
##  Median :     0.0   Median :     0.0   Median :     0.0  
##  Mean   :   632.6   Mean   :   431.8   Mean   :   603.3  
##  3rd Qu.:    29.0   3rd Qu.:    19.4   3rd Qu.:    27.0  
##  Max.   :547222.9   Max.   :396618.3   Max.   :553203.6
```

#### Checking out the g2s object

``` r
head(g2s)
```

```
##                 gene_id gene_symbol
## 1  ENSMUSG00000000001.4       Gnai3
## 2 ENSMUSG00000000003.15        Pbsn
## 3 ENSMUSG00000000028.15       Cdc45
## 4 ENSMUSG00000000031.16         H19
## 5 ENSMUSG00000000037.17       Scml2
## 6 ENSMUSG00000000049.11        Apoh
```

``` r
summary(g2s)
```

```
##    gene_id          gene_symbol       
##  Length:55401       Length:55401      
##  Class :character   Class :character  
##  Mode  :character   Mode  :character
```

``` r
## save this object as a csv file
write.csv(g2s, "data/csvs/g2s.csv")
```

#### Reading in dds_time_point_mouse_dox_long.RData

``` r
dds_time_point_mouse_dox_long <- load("data/dds_time_point_mouse_long.RData")
dds_time_point_mouse_dox_long
```

```
## [1] "dds_time_point"
```

``` r
## this loaded in the dds_time_point object
## let's see whats in it
head(dds_time_point)
```

```
## class: DESeqDataSet 
## dim: 6 15 
## metadata(1): version
## assays(4): counts mu H cooks
## rownames(6): ENSMUSG00000000001.4 ENSMUSG00000000003.15 ...
##   ENSMUSG00000000037.17 ENSMUSG00000000049.11
## rowData names(34): baseMean baseVar ... deviance maxCooks
## colnames(15): WT_0_1 WT_0_2 ... WT_96_2 WT_96_3
## colData names(4): sample_id time_point replicate sizeFactor
```

``` r
summary(dds_time_point)
```

```
## [1] "DESeqDataSet object of length 55401 with 34 metadata columns"
```

#### Reading in deseq_samples.RData

``` r
deseq_samples <- load("data/deseq_samples.RData")
deseq_samples
```

```
## [1] "deseq_samples" "counts_matrix"
```

``` r
head(deseq_samples)
```

```
## [1] "deseq_samples" "counts_matrix"
```

``` r
summary(deseq_samples)
```

```
##    Length     Class      Mode 
##         2 character character
```

#### Lastly from the data folder, lets read in the time_point_res_df.RData file

``` r
time_point_res_df <- load("data/time_point_res_df.RData")
time_point_res_df
```

```
## [1] "res_df"
```

``` r
## this loaded in the res_df object
head(res_df)
```

```
##                 gene_id     baseMean log2FoldChange      lfcSE       stat
## 1  ENSMUSG00000000001.4 4374.7441434     0.05289369 0.03396958  1.5570899
## 2 ENSMUSG00000000003.15    0.0000000             NA         NA         NA
## 3 ENSMUSG00000000028.15 2333.1201830    -0.06050840 0.04299435 -1.4073570
## 4 ENSMUSG00000000031.16  383.1058765    -0.07147966 0.14077602 -0.5077545
## 5 ENSMUSG00000000037.17   70.2663334     0.07091747 0.21208680  0.3343794
## 6 ENSMUSG00000000049.11    0.3672462    -0.74656791 3.98743621 -0.1872301
##      pvalue      padj gene_name        result_name
## 1 0.1194491 0.5167905     Gnai3 time_point_12_vs_0
## 2        NA        NA      Pbsn time_point_12_vs_0
## 3 0.1593215 0.5844163     Cdc45 time_point_12_vs_0
## 4 0.6116255 0.9128884       H19 time_point_12_vs_0
## 5 0.7380932 0.9483573     Scml2 time_point_12_vs_0
## 6 0.8514803        NA      Apoh time_point_12_vs_0
```

``` r
summary(res_df)
```

```
##    gene_id             baseMean        log2FoldChange       lfcSE      
##  Length:221604      Min.   :     0.0   Min.   :-23.66   Min.   :0.02   
##  Class :character   1st Qu.:     0.0   1st Qu.: -0.30   1st Qu.:0.10   
##  Mode  :character   Median :     0.2   Median :  0.03   Median :0.65   
##                     Mean   :   510.3   Mean   :  0.03   Mean   :1.59   
##                     3rd Qu.:    24.5   3rd Qu.:  0.35   3rd Qu.:2.89   
##                     Max.   :437827.1   Max.   : 23.49   Max.   :4.86   
##                                        NA's   :89636    NA's   :89636  
##       stat            pvalue           padj         gene_name        
##  Min.   :-27.35   Min.   :0.00    Min.   :0.00     Length:221604     
##  1st Qu.: -0.58   1st Qu.:0.18    1st Qu.:0.07     Class :character  
##  Median :  0.04   Median :0.57    Median :0.46     Mode  :character  
##  Mean   :  0.06   Mean   :0.52    Mean   :0.46                       
##  3rd Qu.:  0.56   3rd Qu.:0.85    3rd Qu.:0.81                       
##  Max.   : 20.68   Max.   :1.00    Max.   :1.00                       
##  NA's   :89636    NA's   :89760   NA's   :157576                     
##  result_name       
##  Length:221604     
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
## 
```

#### There's also a results folder with some files in it.

``` r
gene_counts <- read_tsv("results/salmon.merged.gene_counts.tsv")
```

```
## Rows: 55401 Columns: 17
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: "\t"
## chr  (2): gene_id, gene_name
## dbl (15): WT_0_1, WT_0_2, WT_0_3, WT_12_1, WT_12_2, WT_12_3, WT_24_1, WT_24_...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
head(gene_counts)
```

```
## # A tibble: 6 × 17
##   gene_id gene_name WT_0_1 WT_0_2 WT_0_3 WT_12_1 WT_12_2 WT_12_3 WT_24_1 WT_24_2
##   <chr>   <chr>      <dbl>  <dbl>  <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
## 1 ENSMUS… Gnai3      4574   5101   5738    5906   4331      3740   3859    4019 
## 2 ENSMUS… Pbsn          0      0      0       0      0         0      0       0 
## 3 ENSMUS… Cdc45      2482.  2797.  3107    2828   2193      1971   2003.   2132 
## 4 ENSMUS… H19         174    208    183     225    123       128    256     306.
## 5 ENSMUS… Scml2        75    102    129.    112.    84.0      81     41      70 
## 6 ENSMUS… Apoh          0      1      0       0      0         0      0       1 
## # ℹ 7 more variables: WT_24_3 <dbl>, WT_48_1 <dbl>, WT_48_2 <dbl>,
## #   WT_48_3 <dbl>, WT_96_1 <dbl>, WT_96_2 <dbl>, WT_96_3 <dbl>
```

``` r
summary(gene_counts)
```

```
##    gene_id           gene_name             WT_0_1             WT_0_2        
##  Length:55401       Length:55401       Min.   :     0.0   Min.   :     0.0  
##  Class :character   Class :character   1st Qu.:     0.0   1st Qu.:     0.0  
##  Mode  :character   Mode  :character   Median :     0.0   Median :     0.0  
##                                        Mean   :   526.7   Mean   :   611.9  
##                                        3rd Qu.:    26.0   3rd Qu.:    29.0  
##                                        Max.   :471028.9   Max.   :522799.8  
##      WT_0_3            WT_12_1            WT_12_2            WT_12_3        
##  Min.   :     0.0   Min.   :     0.0   Min.   :     0.0   Min.   :     0.0  
##  1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0  
##  Median :     0.0   Median :     0.0   Median :     0.0   Median :     0.0  
##  Mean   :   661.2   Mean   :   659.8   Mean   :   476.9   Mean   :   416.0  
##  3rd Qu.:    31.8   3rd Qu.:    35.0   3rd Qu.:    24.0   3rd Qu.:    21.3  
##  Max.   :596450.7   Max.   :478504.2   Max.   :359297.4   Max.   :318699.9  
##     WT_24_1            WT_24_2            WT_24_3            WT_48_1        
##  Min.   :     0.0   Min.   :     0.0   Min.   :     0.0   Min.   :     0.0  
##  1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0  
##  Median :     0.0   Median :     0.0   Median :     0.0   Median :     0.0  
##  Mean   :   464.3   Mean   :   468.5   Mean   :   419.5   Mean   :   535.6  
##  3rd Qu.:    21.0   3rd Qu.:    22.9   3rd Qu.:    19.0   3rd Qu.:    24.7  
##  Max.   :361164.2   Max.   :362709.0   Max.   :713810.4   Max.   :500595.2  
##     WT_48_2            WT_48_3            WT_96_1            WT_96_2        
##  Min.   :     0.0   Min.   :     0.0   Min.   :     0.0   Min.   :     0.0  
##  1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0  
##  Median :     0.0   Median :     0.0   Median :     0.0   Median :     0.0  
##  Mean   :   448.4   Mean   :   436.8   Mean   :   632.6   Mean   :   431.8  
##  3rd Qu.:    20.4   3rd Qu.:    20.0   3rd Qu.:    29.0   3rd Qu.:    19.4  
##  Max.   :434072.8   Max.   :432069.8   Max.   :547222.9   Max.   :396618.3  
##     WT_96_3        
##  Min.   :     0.0  
##  1st Qu.:     0.0  
##  Median :     0.0  
##  Mean   :   603.3  
##  3rd Qu.:    27.0  
##  Max.   :553203.6
```


``` r
gene_tpm <- read_tsv("results/salmon.merged.gene_counts.tsv")
```

```
## Rows: 55401 Columns: 17
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: "\t"
## chr  (2): gene_id, gene_name
## dbl (15): WT_0_1, WT_0_2, WT_0_3, WT_12_1, WT_12_2, WT_12_3, WT_24_1, WT_24_...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
head(gene_tpm)
```

```
## # A tibble: 6 × 17
##   gene_id gene_name WT_0_1 WT_0_2 WT_0_3 WT_12_1 WT_12_2 WT_12_3 WT_24_1 WT_24_2
##   <chr>   <chr>      <dbl>  <dbl>  <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
## 1 ENSMUS… Gnai3      4574   5101   5738    5906   4331      3740   3859    4019 
## 2 ENSMUS… Pbsn          0      0      0       0      0         0      0       0 
## 3 ENSMUS… Cdc45      2482.  2797.  3107    2828   2193      1971   2003.   2132 
## 4 ENSMUS… H19         174    208    183     225    123       128    256     306.
## 5 ENSMUS… Scml2        75    102    129.    112.    84.0      81     41      70 
## 6 ENSMUS… Apoh          0      1      0       0      0         0      0       1 
## # ℹ 7 more variables: WT_24_3 <dbl>, WT_48_1 <dbl>, WT_48_2 <dbl>,
## #   WT_48_3 <dbl>, WT_96_1 <dbl>, WT_96_2 <dbl>, WT_96_3 <dbl>
```

``` r
summary(gene_tpm)
```

```
##    gene_id           gene_name             WT_0_1             WT_0_2        
##  Length:55401       Length:55401       Min.   :     0.0   Min.   :     0.0  
##  Class :character   Class :character   1st Qu.:     0.0   1st Qu.:     0.0  
##  Mode  :character   Mode  :character   Median :     0.0   Median :     0.0  
##                                        Mean   :   526.7   Mean   :   611.9  
##                                        3rd Qu.:    26.0   3rd Qu.:    29.0  
##                                        Max.   :471028.9   Max.   :522799.8  
##      WT_0_3            WT_12_1            WT_12_2            WT_12_3        
##  Min.   :     0.0   Min.   :     0.0   Min.   :     0.0   Min.   :     0.0  
##  1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0  
##  Median :     0.0   Median :     0.0   Median :     0.0   Median :     0.0  
##  Mean   :   661.2   Mean   :   659.8   Mean   :   476.9   Mean   :   416.0  
##  3rd Qu.:    31.8   3rd Qu.:    35.0   3rd Qu.:    24.0   3rd Qu.:    21.3  
##  Max.   :596450.7   Max.   :478504.2   Max.   :359297.4   Max.   :318699.9  
##     WT_24_1            WT_24_2            WT_24_3            WT_48_1        
##  Min.   :     0.0   Min.   :     0.0   Min.   :     0.0   Min.   :     0.0  
##  1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0  
##  Median :     0.0   Median :     0.0   Median :     0.0   Median :     0.0  
##  Mean   :   464.3   Mean   :   468.5   Mean   :   419.5   Mean   :   535.6  
##  3rd Qu.:    21.0   3rd Qu.:    22.9   3rd Qu.:    19.0   3rd Qu.:    24.7  
##  Max.   :361164.2   Max.   :362709.0   Max.   :713810.4   Max.   :500595.2  
##     WT_48_2            WT_48_3            WT_96_1            WT_96_2        
##  Min.   :     0.0   Min.   :     0.0   Min.   :     0.0   Min.   :     0.0  
##  1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0   1st Qu.:     0.0  
##  Median :     0.0   Median :     0.0   Median :     0.0   Median :     0.0  
##  Mean   :   448.4   Mean   :   436.8   Mean   :   632.6   Mean   :   431.8  
##  3rd Qu.:    20.4   3rd Qu.:    20.0   3rd Qu.:    29.0   3rd Qu.:    19.4  
##  Max.   :434072.8   Max.   :432069.8   Max.   :547222.9   Max.   :396618.3  
##     WT_96_3        
##  Min.   :     0.0  
##  1st Qu.:     0.0  
##  Median :     0.0  
##  Mean   :   603.3  
##  3rd Qu.:    27.0  
##  Max.   :553203.6
```


``` r
deseq_results <- load("results/DESEQ_results.rdata")
view(deseq_results)
```


``` r
tpm_results <- load("results/TPM_results.rdata")
view(tpm_results)
```


``` r
# If you want to cross-reference those gene IDs with the DE results:
sig_4fold  <- read.table("results/sig_4fold_genes_counts.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
gene_names <- read_csv("data/csvs/g2s.csv")
```

```
## New names:
## Rows: 55401 Columns: 3
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (2): gene_id, gene_symbol dbl (1): ...1
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## • `` -> `...1`
```

``` r
# sig_4fold_annot <- sig_4fold %>%
#   left_join(gene_names, by = "gene_symbol")  # or by gene_id, if that column matches

# # Inspect
# head(sig_4fold_annot)
```


