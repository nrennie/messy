<!-- badges: start -->
  [![R-CMD-check](https://github.com/nrennie/messy/workflows/R-CMD-check/badge.svg)](https://github.com/nrennie/messy/actions)
<!-- badges: end -->

# messy <img src="man/figures/logo.png" align="right" width="120" />

When teaching examples using R, instructors often using *nice* datasets - but these aren't very realistic, and aren't what students will later encounter in the real world. Real datasets have typos, missing values encoded in strange ways, and weird spaces. The {messy} R package takes a *clean* dataset, and randomly adds these things in - giving students the opportunity to practice their data cleaning and wrangling skills without having to change all of your examples.

## Installation

Install from GitHub using:

```r
remotes::install_github("nrennie/messy")
```

## Usage

### `messy()`

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

Increase how *messy* the data is:

```r
set.seed(1234)
messy(ToothGrowth[1:10,], messiness = 0.7)
```

```r
     len  supp dose
1   <NA>  <NA> <NA>
2  11.5   <NA> <NA>
3   <NA>  <NA> <NA>
4   5.8   <NA> <NA>
5   <NA> .v*c  <NA>
6   <NA>  <NA> <NA>
7   <NA>  <NA> <NA>
8   <NA>  <NA> 0.5 
9   <NA>  v@c  <NA>
10  <NA>  <NA> <NA>
```

### `add_whitespace()`

Randomly adds a whitespace to the ends of some values, meaning that numeric columns may be converted to characters:

```r
set.seed(1234)
add_whitespace(ToothGrowth[1:10,])
```

```r
     len supp dose
1    4.2   VC  0.5
2   11.5   VC  0.5
3    7.3   VC  0.5
4    5.8   VC 0.5 
5    6.4   VC  0.5
6     10   VC  0.5
7  11.2    VC  0.5
8   11.2   VC  0.5
9    5.2   VC  0.5
10     7   VC 0.5 
```

Apply to only some columns:

```r
set.seed(1234)
add_whitespace(ToothGrowth[1:10,], cols = "supp")
```

```r
    len supp dose
1   4.2   VC  0.5
2  11.5   VC  0.5
3   7.3   VC  0.5
4   5.8   VC  0.5
5   6.4   VC  0.5
6  10.0   VC  0.5
7  11.2  VC   0.5
8  11.2   VC  0.5
9   5.2   VC  0.5
10  7.0   VC  0.5
```

### `change_case()`

Randomly switches the case between upper case, lower case, and no change of character or factor columns:

```r
set.seed(1234)
change_case(ToothGrowth[1:10,], messiness = 0.5)
```

```r
    len supp dose
1   4.2   vc  0.5
2  11.5   VC  0.5
3   7.3   VC  0.5
4   5.8   VC  0.5
5   6.4   VC  0.5
6  10.0   VC  0.5
7  11.2   vc  0.5
8  11.2   vc  0.5
9   5.2   VC  0.5
10  7.0   VC  0.5
```

### `add_special_chars()`

Randomly add special characters to character strings:

```r
set.seed(1234)
add_special_chars(ToothGrowth[1:10,])
```

```r
    len supp dose
1   4.2   VC  0.5
2  11.5   VC  0.5
3   7.3   VC  0.5
4   5.8  (VC  0.5
5   6.4   VC  0.5
6  10.0   VC  0.5
7  11.2   VC  0.5
8  11.2   VC  0.5
9   5.2   VC  0.5
10  7.0   VC  0.5
```

### `make_missing()`

Randomly make some values missing using `NA`:

```r
set.seed(1234)
make_missing(ToothGrowth[1:10,])
```

```r
    len supp dose
1   4.2   VC  0.5
2  11.5   VC   NA
3   7.3   VC  0.5
4   5.8   VC  0.5
5   6.4   VC  0.5
6  10.0   VC  0.5
7    NA   VC  0.5
8  11.2   VC   NA
9   5.2   VC  0.5
10  7.0   VC  0.5
```

Add a different missing value representation for some columns:

```r
set.seed(1234)
make_missing(ToothGrowth[1:10,], cols = "supp", missing = "999")
```

```r
    len supp dose
1   4.2   VC  0.5
2  11.5   VC  0.5
3   7.3   VC  0.5
4   5.8   VC  0.5
5   6.4   VC  0.5
6  10.0   VC  0.5
7  11.2  999  0.5
8  11.2   VC  0.5
9   5.2   VC  0.5
10  7.0   VC  0.5
```

### `messy_colnames()`

Create messy column names:

```r
set.seed(1234)
messy_colnames(ToothGrowth[1:10,])
```

```r
   )len s(upp  dose
1   4.2     VC  0.5
2  11.5     VC  0.5
3   7.3     VC  0.5
4   5.8     VC  0.5
5   6.4     VC  0.5
6  10.0     VC  0.5
7  11.2     VC  0.5
8  11.2     VC  0.5
9   5.2     VC  0.5
10  7.0     VC  0.5
```

### Combining functions

You can pipe together multiple functions to create custom messy transformations:

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
