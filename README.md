
<!-- README.md is generated from README.Rmd. Please edit that file -->

# toweR

<!-- badges: start -->

[![R-CMD-check](https://github.com/ltdroy/toweR/workflows/R-CMD-check/badge.svg)](https://github.com/ltdroy/toweR/actions)
[![test-coverage](https://github.com/ltdroy/toweR/workflows/test-coverage/badge.svg)](https://github.com/ltdroy/toweR/actions)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg)](https://choosealicense.com/licenses/mit/)
[![codecov](https://codecov.io/gh/ltdroy/toweR/branch/master/graph/badge.svg)](https://codecov.io/gh/ltdroy/toweR)
<!-- badges: end -->

The goal of toweR is to make the job of loading and vertically combining
multiple rectangular data-files more efficient, easier and cleaner.

Most importantly, it provides specialized tools for dealing with
*inconsistencies in the structure of the data files* (e.g. different
column names).

## Quick-Start: Tower Workflow

The basic `toweR` workflow is:

1.  Build a ‘file scaffold’ (a special df/tibble) containing the
    metadata about your files
2.  Use this metadata to read each file into your scaffold
3.  Harmonise these files prior to combining them
4.  Combine them vertically (a.k.a row-bind/vertical-merge) into a
    single dataframe
5.  Check the diagnostics for this vertical merge
6.  (As needed) repeat steps (3-5) to refine the harmonisation

### Build the file scaffold

-   `build_scaffolding_from_dir()`

### Add metadata about your files

-   `metadata_col_from_regex()`
-   `metadata_col_from_lookup()`
-   `add_linestarts()`
-   `modify_linestarts()`
-   `add_sheet_selections()`
-   `modify_sheet_selections()`

### Check the file coverage using the metadata

-   `check_metadata_coverage()`

### Load the raw data into your scaffold

-   `load_materials_simply()`

### Harmonise the files prior to vertical merge

-   `harmonise_by_explicit()`

### Vertically combine the the datasets (with optional diagnostics)

-   `vertical_merge()`

### Extract detailed diagnostics from the combined dataframe

-   `get_tower_diagnostics()`

## Installation

You can install the development version of toweR like so:

``` r
devtools::install_github("ltdroy/toweR")
```

## Background

A *vertical merge* (a.k.a a row-bind) is different to a *join*, in a
*join* we are usually combining two datasets with different types
(and/or) levels of measurement (e.g. the demographic characteristics of
UK households, and the labor-market characteristics of UK regions),
horizontally on a shared key (e.g. UK regional location).

In a vertical merge (also called vertical bind or row-bind), we are
taking two or more data-sets with (in principle) the same measurement
structure, that cover *different cases* (and/or the same cases measured
at different times) and we are then stacking them on top of each other
(hence the ‘tower’ metaphor).

A trivial (and familiar to most users) example of vertical merger in `R`
is a call to `rbind()`.

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

This simple example doesn’t require any additional tooling.

Further more, `dplyr::bind_rows()` provides a useful extension of
`rbind()` that accepts a list of data-frames and binds them vertically
by column name. So the case of `n > 2` doesn’t *necessarily* pose a
problem for vertical merges (but see below).

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

The `toweR` package comes into its own when it comes to dealing with
multiple datasets (i.e. any more than 3) that may have *inconsistent
structure*:

1.  Different column names, or missing/inconsistent columns
2.  Different column types (e.g. date value in one dataset, character in
    another)
3.  Unexpected missing-data patterns in some dataframes
4.  Data is stored in different parts of text/excel-files

These inconsistencies can create significant problems when seeking to
combine the data for scientific analyses:

**Example 1:** Inconsistently named, or missing, columns can create
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
in certain files. For example, a column named `x` might be present in
all files in a set, but the user might not be aware that in files 4 and
19, the column `x` is filled with only missing data.

In such an example, once the data is merged together, the absence of
this information in certain files may be mistaken for a general
‘low-level’ presence of missing observations in the variable in question
(rather than complete absence of the measure for some datasets).

This, in turn may lead to inappropriate analytic decisions (like
treating `x` as missing at random, as opposed to missing from particular
datasets) and/or produce unexpected results (like attempting to
calculate the mean of `x` for groups in which it was not measured at
all).

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
these issues (and others, such as inconsistent location of the data in
the source files, see below).

Without specialized tools to deal with complex vertical merges, analysts
and researchers may be forced to manually check for these issues and
correct them. In the worst case scenario they may try to do this by
hand, via ‘copy and paste’ between files and spreadsheets, with
significant risk of manual error.

In real-world scenarios, datasets may contain hundreds of variables and
there may be tens or hundreds of datasets to combine. Manual combination
quickly becomes untenable at these scales and comes with an increasing
likelihood of human error.

## Vertical join scenarios

1.  A trial has been conducted in a large number of
    labs/hospitals/establishments across the country, and the results
    for each participant have been recorded in an individual spreadsheet
    for each establishment and then emailed to a central analysis
    office. Although similar, the structure of these spreadsheets has
    not been strongly enforced.
2.  A long-term longitudinal survey has been repeated over multiple
    waves, with the administrators of the survey (and therefore the
    format of the collated data) changing subtly over time.
3.  A researcher is scraping quarterly published data from the last ten
    years from the website of public body, they want to combine the 40
    published datasets, but the format of this data has changed over
    time.
4.  *Any R user that has data spread across multiple datasets or
    data-frames that they want to combine together quickly, easily and
    safely*
