#' Add metadata columns to a scaffold using regex to
#' extract information from the filename
#'
#' @param scaffold_df A tibble/data.frame containing columns:
#'
#' 1. filename - Name of the file
#' 2. filepath - Path to the file
#' 3. file_ext - File extension (e.g. .csv)
#'
#' And any other columns required by the function.
#'
#' @param regex_list A named list of strings, with the names corresponding
#' to the new columns to be created, and the strings representing
#' regex patterns used to extract information from the source column.
#' @param source_column A string of length 1 (default: 'filename'), the name of a
#' column in the dataset from which values are extracted using the supplied regex.
#'
#' @return `scaffold_df` with additional columns created by applying `regex_list`
#' @export
#'
#' @examples
#'
#' library(magrittr)
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
#'  )
#'
metadata_col_from_regex <- function(scaffold_df,
                                    regex_list,
                                    source_column = "filename"){

  # Input checking

  assertthat::assert_that(
    is_file_scaffold(scaffold_df),
    is.list(regex_list),
    length(names(regex_list)) == length(regex_list),
    source_column %in% names(scaffold_df)
    )

  list_contents_valid <- purrr::map_lgl(
    regex_list, function(x){is.character(x) & length(x) == 1}
  )

  assertthat::assert_that(
    isTRUE(
      all(
        list_contents_valid
      )
    ),
    msg = paste0(
      "Items: ",
      paste0(seq_len(length(regex_list))[list_contents_valid], collapse = ", "),
      "\n... are not character vectors of length 1."
    )
  )

  list_names_invalid <- names(regex_list) %in% c("filename", "filepath", "file_ext")

  assertthat::assert_that(
    isTRUE(
      !any(list_names_invalid)
    ),
    msg =
      paste0(
        "Cannot use metadata_col_from_regex() to override the core file scaffold columns:\n",
        paste0(regex_list[list_names_invalid], collapse = ", ")
      )

  )

  if(length(intersect(
    names(regex_list),
    names(scaffold_df)
  )) > 0){

    warning("Columns:\n",
            paste0(
              intersect(
                names(regex_list),
                names(scaffold_df)
              ),
              collapse = ",\n"
            ),
            "\n of the scaffold_df will be over-ridden by this operation."
            )

  }

  # Substantive Code

  new_metadata_df <- purrr::map_dfc(
    regex_list,
    function(user_pattern){
      stringr::str_extract(scaffold_df[[source_column]], pattern = user_pattern)
    }
  )

  scaffold_df_final <- dplyr::bind_cols(scaffold_df, new_metadata_df)

  # Output checks

  assertthat::assert_that(
    is.data.frame(scaffold_df),
    nrow(scaffold_df_final) == nrow(scaffold_df),
    ncol(scaffold_df_final) == ncol(scaffold_df) + ncol(new_metadata_df),
    msg = "Error in constructing updated file scaffold"
  )

  # Return

  return(scaffold_df_final)

}

#' Add metadata columns to a scaffold using lookups that cover each filename
#'
#' @inheritParams metadata_col_from_regex
#' @param lookup_list A list of df/tibbles, all containing a column 'filename',
#'   with each of the (unique) values of the filename column in `scaffold_df`,
#'   and additional columns containing metadata corresponding to the files.
#'   These lookups are joined into the scaffold_df by a left-join.
#'
#' @return `scaffold_df` with additional columns created by joining-in the lookups
#' contained in `lookup_list`
#' @export
#'
#' @examples
#'
#' library(magrittr)
#'
#' # Metadata that you prepared in a spreadsheet (perhaps)
#' series_lookup = tibble::tibble(
#'                        filename = c("a_101.csv", "a_201.csv", "b_201.csv"),
#'                        seminar_series = c("101", "201", "201"))
#'
#' tibble::tibble(
#'   filename = c("a_101.csv", "a_201.csv", "b_201.csv"),
#'   filepath = c("data/a_101.csv", "data/a_201.csv", "data/b_201.csv"),
#'   file_ext = c("csv", "csv", "csv")
#' ) %>%
#'  metadata_col_from_lookup(
#'    scaffold_df = .,
#'    lookup_list = list(
#'      series_lookup
#'    )
#'  )
#'
metadata_col_from_lookup <- function(scaffold_df, lookup_list){

  assertthat::assert_that(
    is_file_scaffold(scaffold_df),
    is.list(lookup_list)
  )

  list_contents_are_df <- purrr::map_lgl(
    lookup_list, ~ is.data.frame(.x)
  )

  assertthat::assert_that(
    isTRUE(
      all(
        list_contents_are_df
      )
    ),
    msg = paste0(
      "These components of lookup_list are not dfs:",
      paste0(seq_len(length(lookup_list))[list_contents_are_df],
             collapse = ", "
             )
    )
  )

  list_contents_contain_fn_col <- purrr::map_lgl(
    lookup_list, ~ "filename" %in% names(.x)
  )

  assertthat::assert_that(
    isTRUE(
      all(
        list_contents_contain_fn_col
      )
    ),
    msg = paste0(
      "These components of lookup_list do not have a filename columnb
      to use as a joining key:",
      paste0(seq_len(length(lookup_list))[list_contents_contain_fn_col],
             collapse = ", "
             )
    )
  )

  assertthat::assert_that(
    isTRUE((purrr::map(
          lookup_list, ~ names(.x)
        ) %>%
        purrr::reduce(intersect)) == "filename") | length(lookup_list) == 1,
    msg = "Only the column 'filename' can be repeated across multiple dfs in the lookup_list"
  )

  list_contents_fn_is_unique <-  purrr::map_lgl(
    lookup_list, ~ length(unique(.x[["filename"]])) == nrow(.x)
  )

  assertthat::assert_that(
    isTRUE(
      all(
        list_contents_fn_is_unique
      )
    ),
    msg = paste0(
      "One (or more) of the lookups contains a duplicate filename.",
      "\nSee items: ",
      paste0(seq_len(length(lookup_list))[list_contents_fn_is_unique],
             collapse = ", "
      )
    )
  )

  assertthat::assert_that(
    isTRUE(
      all(
        purrr::map_lgl(
          lookup_list, ~ nrow(scaffold_df) == nrow(.x)
        )
      )
    ),
    msg = "One (or more) of the lookups has a different number of rows to the file scaffold"
  )

  assertthat::assert_that(
    isTRUE(
      all(
        purrr::map_lgl(
          lookup_list, ~ setequal(scaffold_df[["filename"]], .x[["filename"]])
        )
      )
    ),
    msg = "The filename key is invalid (wrong filename, or missing a filename)\n
           in one of the lookups."
  )

  # Check if any of the look-ups will over-ride existing columns, in which case,
  # warn the user.

  overriding_lookups <- purrr::map_lgl(lookup_list,
                                       ~ length(intersect(names(.x),
                                                          names(scaffold_df)
                                                          )
                                                ) > 1
                                       )

  if(any(overriding_lookups)){

    warning(
      "The following existing columns in the file-scaffold will be over-ridden:\n",
      paste0(
        purrr::map(lookup_list[overriding_lookups],
                       ~ names(.x)[names(.x) != "filename"]) %>%
          purrr::reduce(., union) %>%
          intersect(., names(scaffold_df)),
        collapse = ",\n"
      )
    )

  }

  if(length(lookup_list) > 1){

    consolidated_lookup <- lookup_list %>%
      purrr::reduce(., safe_left_join,
                    by = "filename"
                    )

  } else {

    consolidated_lookup <- lookup_list[[1]]

  }

  assertthat::assert_that(
    nrow(consolidated_lookup) == nrow(scaffold_df),
    msg = "Error in consolidating lookups"
  )

  safe_left_join(
    scaffold_df,
    consolidated_lookup,
    by = "filename"
  ) %>%
    return()

}

#' Add a default line-start value to the scaffold data-frame
#' in preparation for loading the data into R
#'
#' @inheritParams metadata_col_from_regex
#' @param default Default value to use (added to all rows)
#'
#' @return `scaffold_df` with an additional column indicating
#' the line to start reading the data from
#' @export
#'
#' @examples
#'
#' library(magrittr)
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
#'  add_linestarts(default = 1)
add_linestarts <- function(scaffold_df, default = 1){

  assertthat::assert_that(
    is_file_scaffold(scaffold_df),
    is.numeric(default),
    length(default) == 1
  )

  # Add line-starts as a list column
  scaffold_df[["linestart"]] <- default

  return(scaffold_df)

}

#' Modify the default linestart values for specific files
#'
#' @inheritParams metadata_col_from_regex
#' @param meta_col A string, the name of the meta column to use to identify
#' files whose default metadata will be changed
#' @param meta_values Rows matching these values will replace their linestart/sheet_selection value with the new value`
#' @param new_linestart A number, the index of the row to start reading the data from
#'
#' @return
#' @export
#'
#' @examples
#'
#' library(magrittr)
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
#'  add_linestarts(default = 1) %>%
#'  modify_linestarts(
#'    meta_col = "seminar_series",
#'    meta_values = "201",
#'    new_linestart = 2
#'  )
modify_linestarts <- function(scaffold_df,
                              meta_col = "filename",
                              meta_values,
                              new_linestart){

  assertthat::assert_that(
    is_file_scaffold(scaffold_df)
  )

  assertthat::assert_that(
    is.character(meta_col),
    length(meta_col) == 1,
    length(new_linestart) == 1,
    is.numeric(new_linestart)
  )

  assertthat::assert_that(
    meta_col %in% names(scaffold_df),
    "linestart" %in% names(scaffold_df)
  )

  assertthat::assert_that(
    isTRUE(all(meta_values %in% scaffold_df[[meta_col]])),
    msg = paste("One or more meta_values were not found the", meta_col, "column")
  )

  scaffold_df[["linestart"]][ scaffold_df[[meta_col]] %in% meta_values ] <- new_linestart

  return(scaffold_df)

}

#' Add a default name or index of the sheet to read for each file
#'
#' Note: this only affects the reading of excel/spreadsheet files (e.g. workbooks
#' containing multiple sheets)
#' @inheritParams add_linestarts
#'
#' @return `scaffold_df` with a `sheet_selection` column added, containing
#' the default value supplied.
#' @export
#'
#' @examples
#'
#' library(magrittr)
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
#'  add_sheet_selections(default = "QA_responses A)
add_sheet_selections <- function(scaffold_df, default = 1){

  assertthat::assert_that(
    is_file_scaffold(scaffold_df)
  )

  assertthat::assert_that(
    is.numeric(default) | is.character(default),
    length(default) == 1
  )

  scaffold_df[["sheet_selection"]] <- default

  return(scaffold_df)

}

#' Modify the default linestart values for specific files
#'
#' @inheritParams modify_linestarts
#' @param new_sheets_id A string or number, name or index of the sheet to
#' read the data from
#'
#' @return `scaffold_df`, with the `sheet_selection` value for the selected
#' rows modified to be `new_sheets_id`
#' @export
#'
#' @examples
#'
#' library(magrittr)
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
#'  modify_sheet_selections(
#'    meta_col = "seminar_series",
#'    meta_values = "101",
#'    # common situation - slight inconsistency in sheet names
#'    new_sheets_id = "Results1"
#'  )
modify_sheet_selections <- function(scaffold_df,
                                    meta_col = "filename",
                                    meta_values,
                                    new_sheets_id){

  assertthat::assert_that(
    is_file_scaffold(scaffold_df)
  )

  assertthat::assert_that(
    is.character(meta_col),
    length(meta_col) == 1,
    length(new_sheets_id) == 1
  )

  assertthat::assert_that(
    meta_col %in% names(scaffold_df),
    "sheet_selection" %in% names(scaffold_df)
  )

  assertthat::assert_that(
    isTRUE(all(meta_values %in% scaffold_df[[meta_col]])),
    msg = paste("One or more meta_values are not the", meta_col, "column")
  )

  scaffold_df[["sheet_selection"]][ scaffold_df[[meta_col]] %in% meta_values ] <- new_sheets_id

  return(scaffold_df)

}

