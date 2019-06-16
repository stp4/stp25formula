
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stp25formula

Interne Funktionen um Formel und Daten für Evaluationen vorzubereiten.
Die daten werden dabei ins tibble-Format konvertiert und angeforderte
Transformationen ausgeführt.

## Overview

### Usage

``` r
dat <- data.frame(
  sex = 1:2,
  treatment = 1:2,
  m1 = 1:2,  m2 = 1:2,  m3 = 1:2,  m4 = 1:2,  m5 = 1:2,  m6 = 1:2
)
x <- stp25formula::prepare_data2( ~ m1 + m2 + m3 + m4, dat)
str(x)
#> List of 14
#>  $ data           :Classes 'tbl_df', 'tbl' and 'data.frame': 2 obs. of  4 variables:
#>   ..$ m1: int [1:2] 1 2
#>   ..$ m2: int [1:2] 1 2
#>   ..$ m3: int [1:2] 1 2
#>   ..$ m4: int [1:2] 1 2
#>   ..- attr(*, "terms")=Classes 'terms', 'formula'  language ~m1 + m2 + m3 + m4
#>   .. .. ..- attr(*, "variables")= language list(m1, m2, m3, m4)
#>   .. .. ..- attr(*, "factors")= int [1:4, 1:4] 1 0 0 0 0 1 0 0 0 0 ...
#>   .. .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. .. ..$ : chr [1:4] "m1" "m2" "m3" "m4"
#>   .. .. .. .. ..$ : chr [1:4] "m1" "m2" "m3" "m4"
#>   .. .. ..- attr(*, "term.labels")= chr [1:4] "m1" "m2" "m3" "m4"
#>   .. .. ..- attr(*, "order")= int [1:4] 1 1 1 1
#>   .. .. ..- attr(*, "intercept")= int 1
#>   .. .. ..- attr(*, "response")= int 0
#>   .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
#>   .. .. ..- attr(*, "predvars")= language list(m1, m2, m3, m4)
#>   .. .. ..- attr(*, "dataClasses")= Named chr [1:4] "numeric" "numeric" "numeric" "numeric"
#>   .. .. .. ..- attr(*, "names")= chr [1:4] "m1" "m2" "m3" "m4"
#>  $ measure.vars   : chr [1:4] "m1" "m2" "m3" "m4"
#>  $ group.vars     : NULL
#>  $ condition.vars : NULL
#>  $ formula        :Class 'formula'  language ~m1 + m2 + m3 + m4
#>   .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
#>  $ by             : chr "1"
#>  $ measure        : Named chr [1:4] "integer" "integer" "integer" "integer"
#>   ..- attr(*, "names")= chr [1:4] "m1" "m2" "m3" "m4"
#>  $ row_name       : Named chr [1:4] "m1" "m2" "m3" "m4"
#>   ..- attr(*, "names")= chr [1:4] "m1" "m2" "m3" "m4"
#>  $ col_name       : NULL
#>  $ measure.class  : Named chr [1:4] "integer" "integer" "integer" "integer"
#>   ..- attr(*, "names")= chr [1:4] "m1" "m2" "m3" "m4"
#>  $ group.class    : NULL
#>  $ condition.class: NULL
#>  $ digits         : Named num [1:4] 2 2 2 2
#>   ..- attr(*, "names")= chr [1:4] "m1" "m2" "m3" "m4"
#>  $ N              : int 2
#>  - attr(*, "class")= chr [1:2] "stp25data" "list"
```

``` r
prepare_data2(dat, 2:5)
#> [1] "treatment" "m1"        "m2"        "m3"
#> 
#> formula: ~treatment + m1 + m2 + m3
#> <environment: 0x0000000015199150>
#> 
#> measure.vars:  treatment, m1, m2, m3
#> measure:  integer, integer, integer, integer
#> measure.class:  integer, integer, integer, integer
#> digits:  2, 2, 2, 2
#> row_name:  treatment, m1, m2, m3
#> by: [1] "1"
#> 
#> group.vars:   
#> # A tibble: 2 x 4
#>   treatment    m1    m2    m3
#>       <int> <int> <int> <int>
#> 1         1     1     1     1
#> 2         2     2     2     2
```

``` r
prepare_data2(dat, m1, m2, m3)
#>                
#> "m1" "m2" "m3"
#> 
#> formula: ~m1 + m2 + m3
#> <environment: 0x00000000179fc938>
#> 
#> measure.vars:  m1, m2, m3
#> measure:  integer, integer, integer
#> measure.class:  integer, integer, integer
#> digits:  2, 2, 2
#> row_name:  m1, m2, m3
#> by: [1] "1"
#> 
#> group.vars:   
#> # A tibble: 2 x 3
#>      m1    m2    m3
#>   <int> <int> <int>
#> 1     1     1     1
#> 2     2     2     2
```

Lokal transformieren. Interne Evaluation mit stats::model.frame die
Variablen Namen bleiben aber so erhalten wie sie im orinalem data.frame
waren.

``` r
 prepare_data2(~ log(m1) + m2 + m3 + m4, dat)
#> 
#> formula: ~log(m1) + m2 + m3 + m4
#> 
#> measure.vars:  m1, m2, m3, m4
#> measure:  integer, integer, integer, integer
#> measure.class:  integer, integer, integer, integer
#> digits:  2, 2, 2, 2
#> row_name:  m1, m2, m3, m4
#> by: [1] "1"
#> 
#> group.vars:   
#> # A tibble: 2 x 4
#>      m1    m2    m3    m4
#>   <dbl> <int> <int> <int>
#> 1 0         1     1     1
#> 2 0.693     2     2     2
```

Metainformation wie digits und Berechnungsmethoden bereitstellen

``` r
prepare_data2( ~ m1[1] + m2 + m3[4, median] + m4, dat)
#> 
#> formula: ~m1 + m2 + m3 + m4
#> <environment: 0x0000000019730160>
#> 
#> measure.vars:  m1, m2, m3, m4
#> measure:  integer, integer, median, integer
#> measure.class:  integer, integer, integer, integer
#> digits:  1, 2, 4, 2
#> row_name:  m1, m2, m3, m4
#> by: [1] "1"
#> 
#> group.vars:   
#> # A tibble: 2 x 4
#>      m1    m2    m3    m4
#>   <int> <int> <int> <int>
#> 1     1     1     1     1
#> 2     2     2     2     2

prepare_data2(dat, m1[1, freq], m2[2, mean], m3[3, median])
#>                                                 
#>   "m1[1, freq]"   "m2[2, mean]" "m3[3, median]"
#> 
#> formula: ~m1 + m2 + m3
#> <environment: 0x000000001a0993f0>
#> 
#> measure.vars:  m1, m2, m3
#> measure:  freq, mean, median
#> measure.class:  integer, integer, integer
#> digits:  1, 2, 3
#> row_name:  m1, m2, m3
#> by: [1] "1"
#> 
#> group.vars:   
#> # A tibble: 2 x 3
#>      m1    m2    m3
#>   <int> <int> <int>
#> 1     1     1     1
#> 2     2     2     2
```

Gruppierung

``` r

prepare_data2(m1 ~ sex, dat)
#> 
#> formula: m1 ~ sex
#> 
#> measure.vars:  m1
#> measure:  integer
#> measure.class:  integer
#> digits:  2
#> row_name:  m1
#> by: ~sex
#> <environment: 0x000000001aa27258>
#> 
#> group.vars:  sex 
#> # A tibble: 2 x 2
#>      m1   sex
#>   <int> <int>
#> 1     1     1
#> 2     2     2

prepare_data2(dat, m1, by = ~ sex, )
#>      
#> "m1"
#> 
#> formula: m1 ~ sex
#> <environment: 0x0000000014bcdab8>
#> 
#> measure.vars:  m1
#> measure:  integer
#> measure.class:  integer
#> digits:  2
#> row_name:  m1
#> by: ~sex
#> <environment: 0x0000000014baf640>
#> 
#> group.vars:  sex 
#> # A tibble: 2 x 2
#>      m1   sex
#>   <int> <int>
#> 1     1     1
#> 2     2     2
x <- prepare_data2(m1 ~ sex | treatment, dat)
x
#> 
#> formula: m1 ~ sex
#> 
#> measure.vars:  m1
#> measure:  integer
#> measure.class:  integer
#> digits:  2
#> row_name:  m1
#> by: ~sex
#> <environment: 0x0000000017af2290>
#> 
#> group.vars:  sex 
#> # A tibble: 2 x 3
#>      m1   sex treatment
#>   <int> <int>     <int>
#> 1     1     1         1
#> 2     2     2         2
x$condition.vars
#> [1] "treatment"
```
