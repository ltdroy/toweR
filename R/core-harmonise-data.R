#' Harmonise column names across a list of dataframes
#' using an explicit list of column name groups to
#' align equivalent columns
#'
#' @inheritParams metadata_col_from_regex
#' @param capture_list A named list of character vectors, where the name of each
#' list item is a harmonised column name to use, and values in each character
#' vector are existing column names to convert to this name.
#' @param verbose Report changes made in the console? (default: TRUE)
#'
#' @return `scaffold_df` with the column names of the dataframes in the `datasets`
#' harmonised according to `capture_list`
#' @export
#'
#' @examples
#'
#'\dontrun{
#'  #' library(magrittr)
#'
#' tibble::tibble(
#'   filename = c("a_101.csv", "a_201.csv", "b_201.csv"),
#'   filepath = c("data/a_101.csv", "data/a_201.csv", "data/b_201.csv"),
#'   file_ext = c("csv", "csv", "csv")
#' ) %>%
#'  metadata_col_from_regex(
#'    scaffold_df = .,
#'    regex_list = list(
#'      "seminar_series" = "\\d\\d\\d"
#'    )
#'  ) %>%
#'  add_sheet_selections(default = "results1") %>%
#'  modify_linestarts(
#'    meta_col = "seminar_series",
#'    meta_values = "101",
#'    # common situation - slight inconsistency in sheet names
#'    new_sheets_id = "Results1"
#'  )
#'  add_linestarts(default = 1) %>%
#'  modify_linestarts(
#'    meta_col = "seminar_series",
#'    meta_values = "201",
#'    new_linestart = 2
#'  ) %>%
#'  load_materials_simple() %>%
#'  harmonise_by_explicit(., capture_list = list(
#'    "results" = c("results", "ResultsA", "ResultsB")
#'  ))
#'}
#'
harmonise_by_explicit <- function(scaffold_df, capture_list, verbose = TRUE){

  assertthat::assert_that(
    is_file_scaffold(scaffold_df),
    is.list(capture_list),
    length(names(capture_list)) == length(capture_list),
    "datasets" %in% names(scaffold_df)
  )

  scaffold_df[["datasets"]] <- purrr::map2(
    scaffold_df[["datasets"]],
    scaffold_df[["filename"]],
    rename_cols_by_capture_list,
    capture_list = capture_list,
    verbose = verbose
  )

  return(scaffold_df)

}

#' rename_cols_by_capture_list
#'
#' @inheritParams harmonise_by_explicit
#' @param df A dataframe
#' @param df_name string, a label for the dataframe
#' @param verbose report changes to the console?
#'
#' @return `df` with renamed column names according to `capture_list`
#'
#' @examples
rename_cols_by_capture_list <- function(df, df_name, capture_list, verbose){

  for(group_name in names(capture_list)){

    # Bool, is column name in capture group
    capture_bool <- names(df) %in% capture_list[[group_name]]

    # Check that 0 or 1 column names are in capture group
    assertthat::assert_that(
      sum(capture_bool) %in% c(0,1),
      msg = paste0(
        "Multiple columns captured in the same capture group:\n",
        paste0(names(df)[capture_bool], collapse = ",\n"))
    )

    if(isTRUE(verbose) && sum(capture_bool) == 1){
      message("Column: '",
              names(df)[capture_bool],
              "' renamed as '",
              group_name,
              "' in file:\n",
              df_name
              )
    }

    # Rename the captured column (if any) with the group name
    names(df)[capture_bool] <- group_name

  }

  return(df)

}
