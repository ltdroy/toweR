#' Vertically merge a set of data bricks
#'
#' @param data_bricks Set of data bricks:
#' A tibble with a list column called `data_brick` which contains
#' dataframes to be merged vertically. Typically this tibble
#' is the output of a call to 'build_tower()'.
#' @param diagnostics This can either be a logical (TRUE/FALSE)
#' or it can be a character vector listing the diagnostics to be performed:
#' TRUE - perform all diagnostics
#' FALSE - perform no diagnostics
#' Character vector:
#' 1. 'names' - provides warnings about missing/inconsistent column names,
#' and fails if explicit 'selections' are provided and one of these selections is
#' missing from all datasets
#' 2. 'types' - provides a warning about any inconsistent types on shared variable names,
#' this consistentencies are always resolved by converting the variable to character in the final dataset
#' 3. 'missingness' - provides a warning about unusual patterns of missingness of selected variables in one or more files
#' 4. 'coverage' - (only applied if the coverage parameter is supplied), checks that the metadata in the `tower_blocks` object
#' provides full coverage of the expected values (see coverage).
#' @param keep_diagnostics TRUE/FALSE. Should diagnostic summary information be stored as an attribute of the output dataset,
#' which can be viewed, or extracted wholesale, using `get_tower_diagnostics()`
#' @param add_metadata TRUE/FALSE/character vector:
#' 1. TRUE - Add all `tower_blocks` metadata as columns in the output dataset.
#' 2. FALSE - Don't add `tower_blocks` metadata to the output dataset.
#' 3. character vector - add the named columns to the output dataset.
#' @param selections TRUE/character vector:
#' 1. TRUE - Select all columns in all data bricks
#' 2. character vector - take the named columns from each data brick (where available)
#' @param coverage A list of named character vectors, this list should follow
#' a pattern of:
#' list("metadata_column" = c(expected_values))
#' Essentially this faccilitates a diagnostic check that the metadata column
#' contains all the values listed in expected values.
#' A helper function `generate_time_sequence()` can be used to
#' build a time or time-date vector to check coverage of time-stamped files.
#' @return
#' A tibble/dataframe that contains a vertical merger of all of the dataframes in the supplied data_brick
#' object. Optionally, it contains attributes:
#' 1. "name_diagnostics" - A dataframe containing rows for each variable and columns containing information about any NA-padding that occured (i.e. due to the variable being missing)
#' 2. "type_diagnostics" - A dataframe containig rows for each variable and columns containing information about the type of the variable and any conversions that took place.
#' 3. "missingness" - A dataframe containing rows for each variable, and missingn diagnostic information for that variable.
#' 4. "coverage" - A dataframe with diagnostic information about any missing coverage in the source files.
#' @export
#'
#' @examples
vertical_merge <- function(
  data_bricks,
  selections = TRUE,
  diagnostics = TRUE,
  keep_diagnostics = FALSE,
  add_metadata = TRUE,
  coverage = NULL) {



}
