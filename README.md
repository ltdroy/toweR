
<!-- README.md is generated from README.Rmd. Please edit that file -->

# toweR

<!-- badges: start -->

<!-- badges: end -->

STILL IN DEVELOPMENT - NOT FOR USE

The goal of toweR is to make the job of loading and vertically combining
multiple rectangular data-files more efficient (less user time
consumed/wasted), easier (less manual effort from the user) and safer
(reduced error risk of user error/unknown data problems). It provides
specialized tools for the situation where the data files to be combined
vertically may have an inconsistent structure.

A vertical merge is different to a join, in a *join* we are usually
combining two datasets with different types (and/or) levels of
measurement (e.g. the demographic characteristics of UK households, and
the labour-market characteristics of UK regions), on a shared key
(e.g. UK regional location).

In a *vertical merge* (also called vertical bind or row-bind), we are
taking two or more data-sets with (in principle) the same measurements,
that cover *different cases* (or perhaps the same cases at different
times) and we are then stacking them on top of each other (hence the
‘tower’ metaphor).

A very simple (and familiar to most users) example of vertical merger in
`R` is a call to `rbind()`.

``` r

df1 <- tibble::tribble(
  ~ x, ~y, 
  1, "a",
  2, "b",
  3, "c",
  4, "d",
  5, "e"
)

df2 <- tibble::tribble(
  ~ x, ~y, 
  6, "f",
  7, "g",
  8, "h",
  9, "i",
  10, "j"
)

rbind(df1, df2)
#> # A tibble: 10 x 2
#>        x y    
#>    <dbl> <chr>
#>  1     1 a    
#>  2     2 b    
#>  3     3 c    
#>  4     4 d    
#>  5     5 e    
#>  6     6 f    
#>  7     7 g    
#>  8     8 h    
#>  9     9 i    
#> 10    10 j
```

This simple example doesn’t require additional tooling, and indeed
`dplyr::bind_rows()` provided a very useful extension of `rbind()` that
accepts a list of data-frames and binds them vertically by column name.
So the case of `n > 2` doesn’t *necessarily* pose a problem (but see
below).

``` r

df1 <- tibble::tribble(
  ~ x, ~y, 
  1, "a",
  2, "b",
  3, "c",
  4, "d",
  5, "e"
)

df2 <- tibble::tribble(
  ~ x, ~y, 
  6, "f",
  7, "g",
  8, "h",
  9, "i",
  10, "j"
)

dplyr::bind_rows(list(df1, df2, df2))
#> # A tibble: 15 x 2
#>        x y    
#>    <dbl> <chr>
#>  1     1 a    
#>  2     2 b    
#>  3     3 c    
#>  4     4 d    
#>  5     5 e    
#>  6     6 f    
#>  7     7 g    
#>  8     8 h    
#>  9     9 i    
#> 10    10 j    
#> 11     6 f    
#> 12     7 g    
#> 13     8 h    
#> 14     9 i    
#> 15    10 j
```

`toweR` comes into its own when it comes to dealing with multiple
dataframes (i.e. any more than 3) that may have *inconsistent
structure*:

1.  Different column names, or missing/inconsistent columns
2.  Different column types (e.g. date value in one dataset, character in
    another)
3.  Unexpected missingness patterns in some dataframes

These inconsistencies can create significant problems when seeking to
combine the data for analysis:

**Example 1:** Inconsistently named or missing columns can create
duplicated columns after a vertical merge, or lead to columns being
silently (and potentially unexpectedly) padded with missing values for
sets of cases that didn’t include that column.

``` r

df1 <- tibble::tribble(
  ~ x, ~y, 
  1, "a",
  2, "b",
  3, "c",
  4, "d",
  5, "e"
)

df2 <- tibble::tribble(
  ~ x, ~Y, 
  6, "f",
  7, "g",
  8, "h",
  9, "i",
  10, "j"
)

dplyr::bind_rows(list(df1, df2))
#> # A tibble: 10 x 3
#>        x y     Y    
#>    <dbl> <chr> <chr>
#>  1     1 a     <NA> 
#>  2     2 b     <NA> 
#>  3     3 c     <NA> 
#>  4     4 d     <NA> 
#>  5     5 e     <NA> 
#>  6     6 <NA>  f    
#>  7     7 <NA>  g    
#>  8     8 <NA>  h    
#>  9     9 <NA>  i    
#> 10    10 <NA>  j
```

**Example 2:** Inconsistent column types (i.e. numeric, character, etc.)
can lead to unexpected coercion of columns into undesirable formats.

``` r

df1 <- tibble::tribble(
  ~ x, ~y, 
  "1", "a",
  "2", "b",
  "3", "c",
  "4", "d",
  "5", "e"
)

df2 <- tibble::tribble(
  ~ x, ~y, 
  6, "f",
  7, "g",
  8, "h",
  9, "i",
  10, "j"
)

combined_df <- rbind(df1, df2)

combined_df
#> # A tibble: 10 x 2
#>    x     y    
#>    <chr> <chr>
#>  1 1     a    
#>  2 2     b    
#>  3 3     c    
#>  4 4     d    
#>  5 5     e    
#>  6 6     f    
#>  7 7     g    
#>  8 8     h    
#>  9 9     i    
#> 10 10    j

mean(combined_df$x)
#> Warning in mean.default(combined_df$x): argument is not numeric or logical:
#> returning NA
#> [1] NA
```

**Example 3:** Missing data patterns can mask the absence of information
in certain files. For example, column ‘x’ might be present in files
1-34, but the user may not be aware in files 4 and 19 it is filled with
only missing data. Once the data is merged together the absence of this
information in certain files may be conflated with the general presence
of missing data in ‘x’. This, in turn may lead to inappropriate
analytical decisions and/or unexpected results (like attempting to take
the mean of x for groups in which it was not measured)

``` r

df1 <- tibble::tribble(
  ~ x, ~y, ~z,
  1, "a", 1,
  2, "b", 1,
  3, "c", 1,
  4, "d", 1,
  5, "e", 1
)

df2 <- tibble::tribble(
  ~ x, ~y, 
  6, "f",
  7, "g",
  8, "h",
  9, "i",
  10, "j"
)

df3 <- tibble::tribble(
  ~ x, ~Y, 
  NA, "f",
  NA, "g",
  NA, "h",
  NA, "i",
  NA, "j"
)

dplyr::bind_rows(list(df1, df2))
#> # A tibble: 10 x 3
#>        x y         z
#>    <dbl> <chr> <dbl>
#>  1     1 a         1
#>  2     2 b         1
#>  3     3 c         1
#>  4     4 d         1
#>  5     5 e         1
#>  6     6 f        NA
#>  7     7 g        NA
#>  8     8 h        NA
#>  9     9 i        NA
#> 10    10 j        NA
```

The `toweR` package provides a set of specialized tools for managing
these issues (and others, see below).

Without such tools, analysts may end up needing to revert to trying to
manually check for these issues and correct them. With just two small
data-frames these (as in the example above) these kinds of issues are
easy to spot and correct. But, imagine a real-world realistic scenario
in which there are hundreds of variables and dozens of dataframes (or
even hundreds). Manual checks quickly become untenable at these scales
and come with the increasing likelihood of human error.

This approach can result in lots of wasted effort (checking files that
don’t have issues) and it scales very poorly (e.g. if there are 300
files to check).

This is a frequent occurrence in analysis engineering. The cases that
need to be analysed are spread across a set of files that may have been
created at different times (e.g. monthly snapshot data), or covering
different groups of cases. Anytime data is spread over multiple files,
especially the

## Installation

You can install the development version of toweR like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
#library(toweR)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
