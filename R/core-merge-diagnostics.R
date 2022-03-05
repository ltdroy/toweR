#' run_diagnostics
#'
#' @param scaffold_df
#' @param tower_df
#' @param diagnostics
#'
#' @return
#' @export
#'
#' @examples
run_diagnostics <- function(scaffold_df,
                            tower_df,
                            diagnostics){

  if(!isTRUE(diagnostics)){
    return(NULL)
  }

  list(
    colnames_diagnostics = run_colname_diagnostics(scaffold_df, tower_df),
    coltype_diagnostics = run_coltype_diagnostics(scaffold_df, tower_df),
    missingness_diagnostics = run_missingness_diagnostics(scaffold_df, tower_df),
    coverage_diagnostics = NULL
  )

}

#' build_diagnostic_quick_summary
#'
#' @param diagnostics_list
#' @param tower_df
#'
#' @return
#'
#' @examples
build_diagnostic_quick_summary <- function(diagnostics_list, tower_df){

  quick_class <- function(x){
    dplyr::case_when(
      x ~ "Pass",
      TRUE ~ "Potential Issues"
    )
  }

  paste0(
    "\n\n---MERGE DIAGNOSTICS---\n",
    "Column name diagnostics: ", quick_class(
      diagnostics_list[["colnames_diagnostics"]] %>%
        eval_colname_diagnostics(., tower_df)
      ), "\n",
    "Column type diagnostics: ", quick_class(
      diagnostics_list[["coltype_diagnostics"]] %>%
        eval_coltype_diagnostics(., tower_df)
      ), "\n",
    "Missingness diagnostics: ", quick_class(
      diagnostics_list[["missingness_diagnostics"]] %>%
        eval_missingness_diagnostics(., tower_df)
      ), "\n"
  )

}

#' eval_missingness_diagnostics
#'
#' @param diag_ob
#' @param tower_df
#'
#' @return
#' @export
#'
#' @examples
eval_missingness_diagnostics <- function(diag_ob, tower_df){

  diag_ob %>%
    dplyr::select(
        dplyr::all_of(names(tower_df))
    ) %>%
    purrr::map_lgl(
      .,
      function(col){

        col_vals <- unique(col)
        col_vals <- col_vals[!is.na(col_vals)]

        !(0 %in% col_vals)

      }
    ) %>%
    all()

}

#' eval_coltype_diagnostics
#'
#' @param diag_ob
#' @param tower_df
#'
#' @return
#' @export
#'
#' @examples
eval_coltype_diagnostics <- function(diag_ob, tower_df){

  diag_ob %>%
    dplyr::select(
        dplyr::all_of(names(tower_df))
    ) %>%
    purrr::map_lgl(
      .,
      function(col){

        col_types <- unique(col)
        col_types <- col_types[!is.na(col_types)]

        length(col_types) == 1

      }
    ) %>%
    all()

}

#' eval_colname_diagnostics
#'
#' @param diag_ob
#' @param tower_df
#'
#' @return
#' @export
#'
#' @examples
eval_colname_diagnostics <- function(diag_ob, tower_df){

  diag_ob %>%
    dplyr::select(
       dplyr::all_of(names(tower_df))
    ) %>%
    purrr::pmap_lgl(
      .,
      function(...){
        list(...) %>%
          purrr::map_lgl(
            .,
            ~ .x == 1
          ) %>%
          all()
      }
    ) %>%
    all()

}

#' run_missingness_diagnostics
#'
#' @param scaffold_df
#' @param tower_df
#'
#' @return
#'
#' @examples
run_missingness_diagnostics <- function(scaffold_df, tower_df){

  tidyr::expand_grid(
    files = scaffold_df[["filename"]],
    columns = names(tower_df)
  ) %>%
    dplyr::mutate(
      missing_pc = purrr::map2_dbl(
        files,
        columns,
        function(file, colname){

          missingness_col_basic(
            colname = colname,
            df = (scaffold_df %>%
                    dplyr::filter(filename == file) %>%
                    purrr::pluck("dfs_processed", 1))
          )

        }
      )
    ) %>%
    tidyr::pivot_wider(
      .,
      names_from = columns,
      values_from = missing_pc
    )

}

#' run_colname_diagnostics
#'
#' @param scaffold_df
#' @param tower_df
#'
#' @return
#'
#' @examples
run_colname_diagnostics <- function(scaffold_df, tower_df){

  tidyr::expand_grid(
    files = scaffold_df[["filename"]],
    columns = names(tower_df)
  ) %>%
    dplyr::mutate(
      present = purrr::map2_int(
        files,
        columns,
        function(file, colname){
          as.integer(colname %in%
                       names(scaffold_df %>%
                          dplyr::filter(filename == file) %>%
                          purrr::pluck("dfs_processed", 1))
                       )
        }
      )
    ) %>%
  tidyr::pivot_wider(
    .,
    names_from = columns,
    values_from = present
    )

}

#' run_coltype_diagnostics
#'
#' @param scaffold_df
#' @param tower_df
#'
#' @return
#'
#' @examples
run_coltype_diagnostics <- function(scaffold_df, tower_df){

  tidyr::expand_grid(
    files = scaffold_df[["filename"]],
    columns = names(tower_df)
  ) %>%
    dplyr::mutate(
      types = purrr::map2_chr(
        files,
        columns,
        function(file, colname){

          classify_column_basic(
            colname = colname,
            df = (scaffold_df %>%
                    dplyr::filter(filename == file) %>%
                    purrr::pluck("dfs_processed", 1))
          )

        }
      )
    ) %>%
    tidyr::pivot_wider(
      .,
      names_from = columns,
      values_from = types
    )

}

#' classify_column_basic
#'
#' @param colname
#' @param df
#'
#' @return
#'
#' @examples
classify_column_basic <- function(colname, df){

  dplyr::case_when(
    !(colname %in% names(df)) ~ NA_character_,
    is.character(df[[colname]]) ~ "ch",
    is.numeric(df[[colname]]) ~ "nm",
    lubridate::is.Date(df[[colname]]) ~ "dt",
    is.list(df[[colname]]) ~ "ls",
    TRUE ~ "ot"
  )

}

#' missingness_col_basic
#'
#' @param colname
#' @param df
#'
#' @return
#'
#' @examples
missingness_col_basic <- function(colname, df){

  dplyr::case_when(
    !(colname %in% names(df)) ~ NA_real_,
    TRUE ~ mean(is.na(df[[colname]]))
  )

}

#' get_tower_diagnostics
#'
#' Get the diagnostics from a dataframe constructed
#' using toweR::vertical_merge(scaffold_df, keep_diagnostics = TRUE)
#'
#' @param df A df produced by a call to toweR::vertical_merge(scaffold_df, keep_diagnostics = TRUE)
#'
#' @return The a list of dataframes providing diagnostic checks for the vertical
#' merge used to combine the different datasets.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  final_df <- vertical_merge(scaffold_df, keep_diagnostics = TRUE)
#'  full_merge_diagnostics <- get_tower_diagnostics(final_df)
#' }
#'
get_tower_diagnostics <- function(df){

  assertthat::assert_that(
    "diagnostics" %in% (attributes(df) %>% names())
  )

  attr(df, "diagnostics")

}


