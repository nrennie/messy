<!-- badges: start -->
  [![R-CMD-check](https://github.com/nrennie/messy/workflows/R-CMD-check/badge.svg)](https://github.com/nrennie/messy/actions)
<!-- badges: end -->

# messy <img src="man/figures/logo.png" align="right" width="120" />

When teaching examples using R, instructors often using *nice* datasets - but these aren't very realistic, and aren't what students will later encounter in the real world. Real datasets have typos, missing values encoded in strange ways, and weird spaces. The {messy} R package takes a *clean* dataset, and randomly adds these things in - giving students the opportunity to practice their data cleaning and wrangling skills without having to change all of your examples.

## Installation

Install from CRAN using:

```r
install.packages("messy")
```

Install development version from GitHub using:

```r
remotes::install_github("nrennie/messy")
```

## Usage

For more in-depth usage instructions, see the package documentation at [nrennie.rbind.io/messy](https://nrennie.rbind.io/messy/) which has examples of each function.

The simplest way to use the {messy} package is applying the `messy()` function:

```r
set.seed(1234)
messy(ToothGrowth[1:10,])
```

```r
    len supp dose
1   4.2   VC  0.5
2  11.5 <NA> <NA>
3  7.3    VC  0.5
4   5.8  (VC  0.5
5   6.4   VC <NA>
6    10   VC  0.5
7  11.2 <NA>  0.5
8  11.2   VC  0.5
9  5.2    VC  0.5
10    7   VC 0.5 
```

You can vary the amount of *messiness* for each function, and chain together different functions to create customised messy data:

```r
set.seed(1234)
ToothGrowth[1:10,] |> 
  make_missing(cols = "supp", missing = " ") |> 
  make_missing(cols = c("len", "dose"), missing = c(NA, 999)) |> 
  add_whitespace(cols = "supp", messiness = 0.5) |> 
  add_special_chars(cols = "supp")
```

```r
    len supp dose
1   4.2   VC  0.5
2  11.5  VC    NA
3   7.3   VC  0.5
4   5.8 *VC   0.5
5   6.4  VC   0.5
6  10.0   VC  0.5
7  11.2       0.5
8  11.2  V#C   NA
9   5.2  !VC  0.5
10  7.0 VC*   0.5
```
