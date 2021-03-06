% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vertical_merge.R
\name{vertical_merge}
\alias{vertical_merge}
\title{Vertically merge a set of data bricks}
\usage{
vertical_merge(
  data_bricks,
  selections = TRUE,
  diagnostics = TRUE,
  keep_diagnostics = FALSE,
  add_metadata = TRUE,
  coverage = NULL
)
}
\arguments{
\item{data_bricks}{Set of data bricks:
A tibble with a list column called \code{data_brick} which contains
dataframes to be merged vertically. Typically this tibble
is the output of a call to 'build_tower()'.}

\item{selections}{TRUE/character vector:
\enumerate{
\item TRUE - Select all columns in all data bricks
\item character vector - take the named columns from each data brick (where available)
}}

\item{diagnostics}{This can either be a logical (TRUE/FALSE)
or it can be a character vector listing the diagnostics to be performed:
TRUE - perform all diagnostics
FALSE - perform no diagnostics
Character vector:
\enumerate{
\item 'names' - provides warnings about missing/inconsistent column names,
and fails if explicit 'selections' are provided and one of these selections is
missing from all datasets
\item 'types' - provides a warning about any inconsistent types on shared variable names,
this consistentencies are always resolved by converting the variable to character in the final dataset
\item 'missingness' - provides a warning about unusual patterns of missingness of selected variables in one or more files
\item 'coverage' - (only applied if the coverage parameter is supplied), checks that the metadata in the \code{tower_blocks} object
provides full coverage of the expected values (see coverage).
}}

\item{keep_diagnostics}{TRUE/FALSE. Should diagnostic summary information be stored as an attribute of the output dataset,
which can be viewed, or extracted wholesale, using \code{get_tower_diagnostics()}}

\item{add_metadata}{TRUE/FALSE/character vector:
\enumerate{
\item TRUE - Add all \code{tower_blocks} metadata as columns in the output dataset.
\item FALSE - Don't add \code{tower_blocks} metadata to the output dataset.
\item character vector - add the named columns to the output dataset.
}}

\item{coverage}{A list of named character vectors, this list should follow
a pattern of:
list("metadata_column" = c(expected_values))
Essentially this faccilitates a diagnostic check that the metadata column
contains all the values listed in expected values.
A helper function \code{generate_time_sequence()} can be used to
build a time or time-date vector to check coverage of time-stamped files.}
}
\value{
A tibble/dataframe that contains a vertical merger of all of the dataframes in the supplied data_brick
object. Optionally, it contains attributes:
\enumerate{
\item "name_diagnostics" - A dataframe containing rows for each variable and columns containing information about any NA-padding that occured (i.e. due to the variable being missing)
\item "type_diagnostics" - A dataframe containig rows for each variable and columns containing information about the type of the variable and any conversions that took place.
\item "missingness" - A dataframe containing rows for each variable, and missingn diagnostic information for that variable.
\item "coverage" - A dataframe with diagnostic information about any missing coverage in the source files.
}
}
\description{
Vertically merge a set of data bricks
}
