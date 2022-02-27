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

read_helper_csv <- function(scaffold_row){

  if(!("linestart") %in% names(scaffold_row)){
    warning("linestart was not supplied, defaulting to line 1")

    linestart_num <- 1

  } else {

    linestart_num <- scaffold_row[["linestart"]]
  }

  readr::read_csv(
    file = scaffold_row[["filepath"]],
    col_types = readr::cols(.default = "c"),
    skip = (linestart_num - 1),
    progress = FALSE,
    name_repair = "unique"
  )

}

read_helper_xlsx <- function(scaffold_row){

  if(!("linestart") %in% names(scaffold_row)){
    warning("linestart was not supplied, defaulting to line 1")

    linestart_num <- 1

  } else {

    linestart_num <- scaffold_row[["linestart"]]
  }

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
    skip = (linestart_num - 1),
    progress = FALSE
  )

}
