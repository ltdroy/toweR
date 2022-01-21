
<!-- README.md is generated from README.Rmd. Please edit that file -->

# toweR

<!-- badges: start -->

<!-- badges: end -->

STILL IN DEVELOPMENT - NOT FOR USE

The goal of toweR is to make the job of loading and vertically combining
multiple rectangular data-files more efficient (less user time
consumed/wasted), easier (less manual effort from the user) and
safer/cleaner (reduced risk of user error/unknown data problems).

It provides specialized tools for the situation where the data files to
be loaded and then combined vertically may have an inconsistent
structure.

A *vertical merge* is different to a *join*, in a *join* we are usually
combining two datasets with different types (and/or) levels of
measurement (e.g. the demographic characteristics of UK households, and
the labor-market characteristics of UK regions), horizontally on a
shared key (e.g. UK regional location).

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
‘low-level’ presence of missing observations in the variable in
question (rather than complete absence of the measure for some
datasets).

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

# Vertical join scenarios

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

## Installation

You can install the development version of toweR like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

# Tooling

The following provides an overview of the tooling for vertical merges
provided by `toweR` and shows the workflow we envision for the package.

We organize the concepts around a metaphor of the steps needed to
construct a tower (the final vertically merged datasets).

## Scaffolding & Materials (organised loading of the data)

### Construct the scaffold

The first step in our vertical merge process is to load the data in an
organized way, into an appropriately structured R object.

We begin by building the *scaffolding* for our tower. This *scaffolding*
is a meta-data tibble/dataframe (a tibble/dataframe with information
about the raw files we’re about to load), containing columns `filename`
and `file_path`, as well as additional meta-data that will help us
organize the raw ‘materials’ (i.e. datasets) as we load them into R. A
common example of additional metadata that we might want is a
*timestamp* which records the measurement date of each dataset.

The `toweR` package provides a series of helper functions to construct
the initial *scaffolding* automatically, and to then add metadata. The
following example builds a *scaffolding* dataframe from the contents of
a directory on a local computer.

``` r

data_scaffold <- build_scaffolding_from_dir("R:/Data/Quartely-Datasets/")
```

Other options are available for other storage types:

2.  `build_scaffolding_from_aws()`
3.  `build_scaffolding_from_*()`

In this example, the datasets correspond to different measurement
time-points, `toweR` provides two different options for recording this
information:

``` r

# We can use a regex applied to the filename
data_scaffold <- build_scaffolding_from_dir("R:/Data/Quartely-Datasets/") %>%
  metadata_col_from_regex(list("timestamp" = "[some-expression]"))

# This will create a metadata column timestamp containing all parts of each filename
# that match the supplied regex. 

# The use can supply a full lookup table that will be joined into the dataframe
data_scaffold <- build_scaffolding_from_dir("R:/Data/Quartely-Datasets/") %>%
  metadata_col_from_lookup(tibble::tribble(
    ~filename, ~timestamp,
    "file1", "Q1 2020",
    "file2", "Q2 2020", 
    "file3a", "Q3 2020",
    "file3b", "Q3 2020"
  ))
```

The metadata that we create at this stage can later be used for three
main tasks:

1.  **Coverage checks** - Once we have created our scaffolding, `toweR`
    helps us to perform *coverage checks*, these are checks that our
    collection of datasets covers all expected groups, time-periods,
    etc. and that we haven’t missed any datasets
2.  **Data augmentation** - When we come to vertically merge our data,
    we will have the option to attach the metadata to the raw files
    prior to merging, so that the source file (and relevant associated
    information) for each observation can be tracked.
3.  **Tailored data loading** - When loading the data, parameters
    related to the starting position of the data (etc.) can be
    controlled using metadata, including filename, but also using groups
    defined in the metadata.

### Coverage checks

The `toweR` package provides a set of helper functions to allow you use
metadata to check the coverage of your files:

1.  `check_data_range(scaffold_df, start, end, time_unit, metadata_col)`
    you supply the start and end dates for your data and the function
    checks that there are no missing dates (e.g. months) among your data
    files.
2.  `check_numeric_range(scaffold_df, start, end, interval,
    metadata_col)`
3.  `check_explicit(scaffold_df, expected_values, metadata_col)` check
    that the set of values in the chosen metadata column is the same as
    `expected_values`.

All these functions return their inputs, or fail with an error message
if the checks fail.

### Data Augmentation

This happens at the final data merging stage. See *Building the Tower*
below.

### Collecting the materials (loading the data)

Now that we’ve build our scaffolding df, we will load our data. The data
is loaded as a list-column of the scaffolding (one dataframe per-row).

``` r

data_scaffold <- build_scaffolding_from_dir("R:/Data/Quartely-Datasets/") %>%
  metadata_col_from_lookup(tibble::tribble(
    ~filename, ~timestamp,
    "file1", "Q1 2020",
    "file2", "Q2 2020", 
    "file3a", "Q3 2020",
    "file3b", "Q3 2020"
  )) %>%
  load_data_into_scaffold()
```

#### Setting the data-locations

##### Controlling Linestart

``` r

data_scaffold <- build_scaffolding_from_dir("R:/Data/Quartely-Datasets/") %>%
  metadata_col_from_lookup(tibble::tribble(
    ~filename, ~timestamp,
    "file1", "Q1 2020",
    "file2", "Q2 2020", 
    "file3a", "Q3 2020",
    "file3b", "Q3 2020"
  )) %>%
  add_linestarts(default = 2) %>%
  load_data_into_scaffold()
```

``` r

data_scaffold <- build_scaffolding_from_dir("R:/Data/Quartely-Datasets/") %>%
  metadata_col_from_lookup(tibble::tribble(
    ~filename, ~timestamp,
    "file1", "Q1 2020",
    "file2", "Q2 2020", 
    "file3a", "Q3 2020",
    "file3b", "Q3 2020"
  )) %>%
  add_linestarts(default = 1) %>%
  modify_linestarts(meta_col = "filename", meta_values = c("file1", "file2"), new_linestart = 3)
  load_data_into_scaffold()
```

##### Controlling Sheet Selection

``` r

data_scaffold <- build_scaffolding_from_dir("R:/Data/Quartely-Datasets/") %>%
  metadata_col_from_lookup(tibble::tribble(
    ~filename, ~timestamp,
    "file1", "Q1 2020",
    "file2", "Q2 2020", 
    "file3a", "Q3 2020",
    "file3b", "Q3 2020"
  )) %>%
  add_linestarts(default = 1) %>%
  modify_linestarts(meta_col = "filename", meta_values = c("file1", "file2"), new_linestart = 3) %>%
  add_sheet_selections(default = "data") %>%
  modify_sheetstarts(meta_col = "sheet_name", meta_values = c("file1", "file2"), new_sheet = "Data") %>%
  load_data_into_scaffold()
```

## Preparation (harmonising the raw data)

### Harmonise Column Names

``` r

data_scaffold %>%
  # renames all columns (in all datasets) that match expression with the output name
  harmonise_matching_to_colname(., pattern = "age", shared_colname = "age_of_participant") %>%
  # renames a specific column name (in all datasets) to a specific alternative
  harmonise_colname(., old = "Age (18-45)", shared_colname = "age_of_participant") %>%
  # Custom renaming pattern (2D)
  # Supply a dataframe mapping old and new column-names and apply to a custom selection of dataframes
  harmonise_custom2D_colname(.,
                     custom_colname_mapping = tibble(old, new),
                     eligiblility_metacol = "filename",
                     eligible_values = c("file1", "file2")) %>%
  # Custom renaming pattern (3D)
  # Supply a dataframe mapping old and new column-names for specific dataframes (can vary by df)
  harmonise_custom3D_colname(.,
                     custom_colname_mapping = tibble(old, new, eligible_value),
                     eligiblility_metacol = "filename")
  
```

### Harmonise Column Types

``` r

data_scaffold %>%
  # Safely as true throws an error if any values are coerced to NA, and 
  # provides information on the location of the issue
  harmonise_type_as_character(., cols = c("a", "b", ...), safely = TRUE) %>%
  harmonise_type_as_numeric(., cols = c("a", "b", ...), safely = TRUE) %>%
  # Custom harmonization 2D allow for different 
  # recoding functions to be applied to different copies of the column in different
  # datasets (e.g. for the situation where dates are encoded different in different files)
  # It will throw an error if any coercion to NA occurs, or if the output types of the variables
  # do not match. 
  harmonise_type_as_custom2D(., scheme = list(
    "file1" = as.Date(),
    "file2" = lubridate::dmy(),
    ...
  ), safely = TRUE)
```

## Building the Tower (vertical join)

``` r

final_df <- data_scaffold %>%
  vertical_merge(
    .,
    selections = TRUE,
    # Diagnostics can either be a logical (TRUE/FALSE)
    # or it can be a character vector listing the diagnostics to be performed:
    # TRUE - perform all diagnostics
    # FALSE - perform no diagnostics
    #  Character vector:
    # 1. 'names' - provides warnings about missing/inconsistent column names,
    # and fails if explicit 'selections' are provided and one of these selections is
    # missing from all datasets
    # 2. 'types' - provides a warning about any inconsistent types on shared variable names,
    # this consistentencies are always resolved by converting the variable to character in the final dataset
    # 3. 'missingness' - provides a warning about unusual patterns of missingness of selected variables in one or more files
    # 4. 'coverage' - (only applied if the coverage parameter is supplied), checks that the metadata in the `tower_blocks` object
    # provides full coverage of the expected values (see coverage).
    diagnostics = TRUE,
    # Should diagnostic summary information be stored as an attribute of the output dataset
    keep_diagnostics = FALSE,
    # Add all `tower_blocks` metadata as columns in the output dataset.
    # Character vector can be supplied to add only some metadata
    add_metadata = TRUE,
    # A list of named character vectors, this list should follow
    # a pattern of:
    # list("metadata_column" = c(expected_values))
    # Essentially this facilitates a diagnostic check that the metadata column
    # contains all the values listed in expected values.
    # A helper function `generate_time_sequence()` can be used to
    # build a time or time-date vector to check coverage of time-stamped files.
    coverage = NULL)
```

## Adjust and Repeat

A key part of the logic of the `toweR` work-flow is that the user doesnt
have to get all of the steps above right initially\! Have a go and learn
about your data, and then use the diagnostics (both at the final step,
and in the intermediary operations) to iteratively refine your approach
until you have a clean pipeline that produces a well-harmonized
vertically joined dataset.
