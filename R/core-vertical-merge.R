#' Vertically merge a set of data bricks
#'
#' @param scaffold_df
#' @param diagnostics TRUE/FALSE
#' @param keep_diagnostics TRUE/FALSE. Should diagnostic summary information be stored as an attribute of the output dataset,
#' which can be viewed, or extracted wholesale, using `get_tower_diagnostics()`
#' @param add_metadata TRUE/FALSE/character vector:
#' 1. TRUE - Add all `tower_blocks` metadata as columns in the output dataset.
#' 2. FALSE - Don't add `tower_blocks` metadata to the output dataset.
#' 3. character vector - add the named columns to the output dataset.
#' @param selections TRUE/character vector:
#' 1. TRUE - Select all columns in all data bricks
#' 2. character vector - take the named columns from each data brick (where available)
#' @param quiet TRUE/FALSE
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
  scaffold_df,
  selections = TRUE,
  diagnostics = TRUE,
  keep_diagnostics = FALSE,
  add_metadata = TRUE,
  coverage = NULL,
  quiet = FALSE) {

  assertthat::assert_that(
    is_file_scaffold(scaffold_df),
    "datasets" %in% names(scaffold_df)
  )

  assertthat::assert_that(
    isTRUE(
      all(
        purrr::map_lgl(scaffold_df[["datasets"]],
                       ~ is.data.frame(.x)
                       )
      )
    ),
    msg = "At-least one of the dataset objects in scaffold_df is not a data.frame"
  )

  scaffold_df[["dfs_processed"]] <- purrr::map(
    scaffold_df[["datasets"]],
    ~ .x
  )


  scaffold_df <- apply_selections(scaffold_df, selections = selections)
  scaffold_df <- include_metadata(scaffold_df, add_metadata = add_metadata)

  tower_df <- dplyr::bind_rows(
    scaffold_df[["dfs_processed"]] %>%
      purrr::set_names(scaffold_df[["filename"]])
  )

  diagnostics_list <- run_diagnostics(
    scaffold_df,
    tower_df,
    diagnostics
  )

  if(!isTRUE(quiet)){
    message(
      build_diagnostic_quick_summary(
        diagnostics_list = diagnostics_list,
        tower_df= tower_df)
    )
  }

  if(isTRUE(keep_diagnostics)){

    attr(tower_df, "diagnostics") <- diagnostics_list

  }

  return(tower_df)

}

apply_selections <- function(scaffold_df,
                             selections){

  if(isTRUE(selections)){

    return(scaffold_df)

  } else {

    if(is.character(selections)){

      scaffold_df[["dfs_processed"]] <- purrr::map(
        scaffold_df[["dfs_processed"]],
        ~ .x[names(.x) %in% selections]
      )

      return(scaffold_df)

    } else {

      warning("Unrecognised value of 'selections', ",
              "should be TRUE or character vector.\n",
              "All columns retained.")

      return(scaffold_df)

    }

  }

}

include_metadata <- function(scaffold_df,
                             add_metadata) {

  # Add metadata if needed
  if(isTRUE(add_metadata)){

    scaffold_df[["dfs_processed"]] <- purrr::pmap(
      scaffold_df,
      function(...){
        scaffold_row <- tibble::tibble(...)
        cbind(
          scaffold_row[["dfs_processed"]],
          scaffold_row[
            purrr::map_lgl(
              scaffold_row, ~ !is.list(.x)
            )
          ]
        )
      }
    )

  } else {

    if(is.character(add_metadata)){

      assertthat::assert_that(
        all(add_metadata %in% names(scaffold_df)),
        !("dfs_processed" %in% add_metadata)
      )

      scaffold_df[["dfs_processed"]] <- purrr::pmap(
        scaffold_df,
        function(...){
          scaffold_row <- tibble::tibble(...)
          cbind(
            scaffold_row[["dfs_processed"]],
            scaffold_row[add_metadata]
          )
        }
      )

    } else {

      scaffold_df[["dfs_processed"]] <- scaffold_df[["dfs_processed"]]

      if(!isFALSE(add_metadata)){

        warning("Unrecognised value of add_metadata ",
                "(should be TRUE, FALSE, or vector of column names).\n",
                "No metadata added. ")

      }

    }

  }

  return(scaffold_df)

}
