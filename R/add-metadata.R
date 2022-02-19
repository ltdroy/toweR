#' Add metadata columns to a scaffold using regex to
#' extract information from the filename
#'
#' @param scaffold_df A tibble/data.frame containing columns:
#'
#' 1. filename - Name of the file
#' 2. filepath - Path to the file
#' 3. file_ext - File extension (e.g. .csv)
#'
#' @param regex_list A named list of strings, with the names corresponding
#' to the new columns to be created, and the strings representing
#' regex patterns used to extract information from the filename column.
#'
#' @return `scaffold_df` with additional columns created by applying `regex_list`
#' @export
#'
#' @examples
metadata_col_from_regex <- function(scaffold_df, regex_list){

  assertthat::assert_that(
    is_file_scaffold(scaffold_df)
  )

  assertthat::assert_that(
    is.list(regex_list),
    msg = "regex_list was expected to be a list"
  )

  assertthat::assert_that(
    length(names(regex_list)) == length(regex_list),
    msg = "all regex_list items must be named"
  )

  assertthat::assert_that(
    isTRUE(
      all(
        purrr::map_lgl(
          regex_list, function(x){is.character(x) & length(x) == 1}
        )
      )
    ),
    msg = "all regex_list items must be character vectors of length 1"
  )

  assertthat::assert_that(
    isTRUE(
      !any(names(regex_list) %in% c("filename", "filepath", "file_ext"))
    ),
    msg = "Cannot use metadata_col_from_regex() to override the core file scaffold columns:\n filename, filepath, file_ext"
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

  newcolumn_df <- purrr::map_dfc(
    regex_list,
    function(user_pattern){
      stringr::str_extract(scaffold_df[["filename"]], pattern = user_pattern)
    }
  )

  scaffold_df_final <- dplyr::bind_cols(scaffold_df, newcolumn_df)

  assertthat::assert_that(
    is.data.frame(scaffold_df),
    nrow(scaffold_df_final) == nrow(scaffold_df),
    ncol(scaffold_df_final) == ncol(scaffold_df) + ncol(newcolumn_df),
    msg = "Error in constructing updated file scaffold"
  )

  return(scaffold_df_final)

}

#' Add metadata columns to a scaffold using lookups that cover each filename
#'
#' @param scaffold_df A tibble/data.frame containing columns:
#'
#' 1. filename - Name of the file
#' 2. filepath - Path to the file
#' 3. file_ext - File extension (e.g. .csv)
#'
#' @param lookup_list A list of df/tibbles, all containing a column 'filename',
#'   covering each of the filnames in `scaffold_df`, and additional columns
#'   containing metadata corresponding to the files.
#'   These lookups are joined into the scaffold_df by a left-join.
#'
#' @return `scaffold_df` with additional columns created by joining the lookups
#' contained in `lookup_list`
#' @export
#'
#' @examples
metadata_col_from_lookup <- function(scaffold_df, lookup_list){

  assertthat::assert_that(
    is_file_scaffold(scaffold_df)
  )

  assertthat::assert_that(
    is.list(lookup_list),
    msg = "lookup_list was expected to be a list of dataframes (place a single df in list())"
  )

  assertthat::assert_that(
    isTRUE(
      all(
        purrr::map_lgl(
          lookup_list, ~ is.data.frame(.x)
        )
      )
    ),
    msg = "Expected lookup_list to contain dataframes"
  )

  assertthat::assert_that(
    isTRUE(
      all(
        purrr::map_lgl(
          lookup_list, ~ "filename" %in% names(.x)
        )
      )
    ),
    msg = "All dataframes in lookup_list should have a filename column"
  )

  assertthat::assert_that(
    isTRUE((purrr::map(
          lookup_list, ~ names(.x)
        ) %>%
        purrr::reduce(intersect)) == "filename") | length(lookup_list) == 1,
    msg = "Only the column 'filename' can be repeated across multiple, filename lookups"
  )

  assertthat::assert_that(
    isTRUE(
      all(
        purrr::map_lgl(
          lookup_list, ~ length(unique(.x[["filename"]])) == nrow(.x)
        )
      )
    ),
    msg = "One (or more) of the lookups contains a duplicate filename"
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

#' Perform left join defensively
#'
#' Will fail if:
#'
#' 1. Neither of the dfs have a unique `by` key
#' 2. The rowcount of the output df is not equal to
#' the rowcount of the left-hand df
#'
#' @param df_l Left-hand df
#' @param df_r Right hand df
#' @param by String, name of the key column
#'
#' @return A df of `df_r` left joined into `df_l`
#'
#' @examples
safe_left_join <- function(df_l, df_r, by){

  assertthat::assert_that(
    length(unique(df_l[[by]])) == nrow(df_l) | length(unique(df_r[[by]])) == nrow(df_r)
  )

  df_output <- dplyr::left_join(
    df_l, df_r, by = by,
    na_matches = c("never")
  )

  assertthat::assert_that(
    nrow(df_output) == nrow(df_l)
  )

  return(df_output)

}

#' Add a default line-start value to the scaffold data-frame
#' in preparation for loading the data into R
#'
#' @param scaffold_df A tibble/data.frame containing columns:
#'
#' 1. filename - Name of the file
#' 2. filepath - Path to the file
#' 3. file_ext - File extension (e.g. .csv)
#'
#' @param default A number or string to use as the default line-start
#' value for each file
#'
#' @return
#' @export
#'
#' @examples
add_linestarts <- function(scaffold_df, default = 1){

  assertthat::assert_that(
    is_file_scaffold(scaffold_df)
  )

  assertthat::assert_that(
    is.numeric(default) | is.character(default),
    msg = "Expected default to be a character or numeric vector"
  )

  # Add line-starts as a list column
  scaffold_df[["linestart"]] <- default %>% purrr::map(., ~ .x)

  return(scaffold_df)

}

#' Add a default line-start value to the scaffold data-frame
#' in preparation for loading the data into R
#'
#' @param scaffold_df A tibble/data.frame containing columns:
#'
#' 1. filename - Name of the file
#' 2. filepath - Path to the file
#' 3. file_ext - File extension (e.g. .csv)
#' @param meta_col A string, the name of the meta column to use to identify
#' files whose meta-data should be changed
#' @param meta_values Rows matching these values will replace their linestart value with `new_linestart`
#' @param new_linestart A number or string
#'
#' @return
#' @export
#'
#' @examples
modify_linestarts <- function(scaffold_df, meta_col = "filename", meta_values, new_linestart){

  assertthat::assert_that(
    is_file_scaffold(scaffold_df)
  )

  assertthat::assert_that(
    is.character(meta_col),
    length(meta_col) == 1,
    msg = "Expected meta_col to be a character vector of length 1"
  )

  assertthat::assert_that(
    meta_col %in% names(scaffold_df),
    msg = paste(meta_col, "is not one of the columns of the scaffold_df")
  )

  assertthat::assert_that(
    isTRUE(all(meta_values %in% scaffold_df[[meta_col]])),
    msg = paste("One or more meta_values are not the", meta_col, "column")
  )

  scaffold_df[["linestart"]][ scaffold_df[[meta_col]] %in% meta_values ] <- new_linestart

  return(scaffold_df)

}

add_sheet_selections <- function(scaffold_df, default = 1, load_all_sheets = FALSE){

  assertthat::assert_that(
    is_file_scaffold(scaffold_df)
  )

  assertthat::assert_that(
    is.numeric(default) | is.character(default),
    msg = "Expected defailt to be a character vector or numeric vector"
  )

  if(isTRUE(load_all_sheets)){

    scaffold_df[["sheet_selection"]] <- NA

    return(scaffold_df)

  }


}
