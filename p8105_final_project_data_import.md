P8105 Final Project
================

``` r
library(tidyverse)
options(scipen = 100)
knitr::opts_chunk$set(echo = TRUE)
```

## Read in the data

``` r
resp_2019 = read.table("data/atusresp_2019.dat", header = TRUE, sep = ",")
resp_2020 = read.table("data/atusresp_2020.dat", header = TRUE, sep = ",")
roster_2019 = read.table("data/atusrost_2019.dat", header = TRUE, sep = ",")
roster_2020 = read.table("data/atusrost_2020.dat", header = TRUE, sep = ",")
roster_2019 = read.table("data/atusrost_2019.dat", header = TRUE, sep = ",")
roster_2020 = read.table("data/atusrost_2020.dat", header = TRUE, sep = ",")
who_2019 = read.table("data/atuswho_2019.dat", header = TRUE, sep = ",")
who_2020 = read.table("data/atuswho_2020.dat", header = TRUE, sep = ",")
activity_2019 = read.table("data/atusact_2019.dat", header = TRUE, sep = ",")
activity_2020 = read.table("data/atusact_2020.dat", header = TRUE, sep = ",")
activity_summary_2019 = read.table("data/atussum_2019.dat", header = TRUE, sep = ",")
activity_summary_2020 = read.table("data/atussum_2020.dat", header = TRUE, sep = ",")
```

``` r
head(activity_2019)
```

    ##         TUCASEID TUACTIVITY_N TEWHERE TRTCCTOT_LN TRTCC_LN TRTCOC_LN TRTEC_LN
    ## 1 20190101190022            1      -1           0       -1         0       -1
    ## 2 20190101190022            2      -1           0       -1         0       -1
    ## 3 20190101190022            3      12           0       -1         0       -1
    ## 4 20190101190022            4       5           0       -1         0       -1
    ## 5 20190101190022            5      12           0       -1         0       -1
    ## 6 20190101190022            6       1           0       -1         0       -1
    ##   TRTHH_LN TRTNOHH_LN TRTOHH_LN TRTONHH_LN TRTO_LN TUACTDUR TUACTDUR24 TUCC5
    ## 1       -1         -1        -1         -1      -1      180        180     0
    ## 2       -1         -1        -1         -1      -1       30         30     0
    ## 3       -1         -1        -1         -1      -1       30         30     0
    ## 4       -1         -1        -1         -1      -1      240        240     0
    ## 5       -1         -1        -1         -1      -1       30         30     0
    ## 6       -1         -1        -1         -1      -1       30         30     0
    ##   TUCC5B TUCC7 TUCC8 TUCUMDUR TUCUMDUR24 TUDURSTOP TUEC24 TUSTARTTIM TUSTOPTIME
    ## 1      0     0    97      180        180         2     -1   04:00:00   07:00:00
    ## 2      0     0     0      210        210         1     -1   07:00:00   07:30:00
    ## 3      0     0     0      240        240         1     -1   07:30:00   08:00:00
    ## 4      0     0     0      480        480         2     -1   08:00:00   12:00:00
    ## 5      0     0     0      510        510         1     -1   12:00:00   12:30:00
    ## 6      0     0     0      540        540         1     -1   12:30:00   13:00:00
    ##   TUTIER1CODE TUTIER2CODE TUTIER3CODE TRCODE TRTIER2 TXWHERE
    ## 1           1           1           1  10101     101       0
    ## 2           1           2           1  10201     102       0
    ## 3          18          14           1 181401    1814       0
    ## 4          14           1           1 140101    1401       0
    ## 5          18          14           1 181401    1814       0
    ## 6          11           1           1 110101    1101       0
