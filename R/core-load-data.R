#' (Simple) automatically load the files using the supplied metadata
#'
#' Assumes that there is one rectangular dataset to read per-file
#' and that each file is either csv or xlsx.
#'
#' In the case of csv the linestart is used to guide file reading.
#'
#' In the case of xlsx the linestart and sheet_selection are
#' used to guide file reading.
#'
#' @inheritParams metadata_col_from_regex
#'
#' @return `scaffold_df` with a list column `datasets` added
#' that contains the raw files (loaded using metadata in scaffold_df)
#' @export
#'
#' @examples
#'
#' \dontrun{
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
#'  load_materials_simple()
#'
#' }
#'
load_materials_simple <- function(scaffold_df){

  # Runtime assertions

  assertthat::assert_that(
    is_file_scaffold(scaffold_df)
  )

  # Check that all files exist before loading

  file_exist_bool <- purrr::map_lgl(
    scaffold_df[["filepath"]],
    ~ fs::file_exists(.x)
  )

  assertthat::assert_that(
    isTRUE(
      all(
        file_exist_bool
      )
    ),
    msg = paste0(
      "Error, the following files were not found at the supplied paths:\n\n",
      paste0(scaffold_df[["filename"]][file_exist_bool], collapse = ",\n"),
      "\ninspect the filepath column of the scaffold df."
    )
  )

  scaffold_df[["datasets"]] <- purrr::pmap(
    scaffold_df,
    ~ tibble::tibble(...) %>%
      simple_read_switch
  )

  return(scaffold_df)

}

#' simple_read_switch
#'
#' @param scaffold_row A row of a `scaffold_df` dataframe
#'
#' @return A dataframe loaded via read_helper_csv
#' or read_helper_xlsx
#'
#' @examples
simple_read_switch <- function(scaffold_row){

  if(scaffold_row[["file_ext"]] == "csv"){
    read_helper_csv(scaffold_row) %>% return()
  } else {
    if(scaffold_row[["file_ext"]] == "xlsx"){
      read_helper_xlsx(scaffold_row) %>% return()
    } else {
      stop(
        "File extenion: ", scaffold_row[["file_ext"]],
        " is not covered by load_materials_simple().\n"
      )
    }
  }

}

#' read_helper_csv
#'
#' @inheritParams simple_read_switch
#'
#' @return A dataframe
#'
#' @examples
read_helper_csv <- function(scaffold_row){

  if(!("linestart") %in% names(scaffold_row)){
    warning("linestart was not supplied, defaulting to line 1")

    linestart_num <- 1

  } else {

    linestart_num <- scaffold_row[["linestart"]]
  }

  assertthat::assert_that(
    !is.na(as.integer(linestart_num)),
    msg = paste(
      "Invalid linstart value for file:",
      scaffold_row[["filename"]]
    )
  )

  readr::read_csv(
    file = scaffold_row[["filepath"]],
    col_types = readr::cols(.default = "c"),
    skip = (as.integer(linestart_num) - 1),
    progress = FALSE,
    name_repair = "unique"
  )

}

#' read_helper_xlsx
#'
#' @inheritParams simple_read_switch
#'
#' @return A dataframe
#'
#' @examples
read_helper_xlsx <- function(scaffold_row){

  if(!("linestart") %in% names(scaffold_row)){
    warning("linestart was not supplied, defaulting to line 1")

    linestart_num <- 1

  } else {

    linestart_num <- scaffold_row[["linestart"]]
  }

  assertthat::assert_that(
    !is.na(as.integer(linestart_num)),
    msg = paste(
      "Invalid linstart value for file:",
      scaffold_row[["filename"]]
    )
  )

  if(!("sheet_selection") %in% names(scaffold_row)){
    warning("sheet selection was not supplied, defaulting to first sheet")

    selected_sheet <- 1

  } else {

    selected_sheet <- scaffold_row[["sheet_selection"]]
  }

  readxl::read_xlsx(
    file = scaffold_row[["filepath"]],
    sheet = selected_sheet,
    col_types = "text",
    skip = (as.integer(linestart_num) - 1),
    progress = FALSE
  )

}
