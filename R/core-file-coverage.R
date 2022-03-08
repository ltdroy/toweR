#' Check the metadata for files represented in a scaffold_df, against
#' a set of expected values, to identify any coverage gaps in the
#' loaded data
#'
#' @inheritParams metadata_col_from_regex
#' @param meta_col name of column to check
#' @param expected_values values expected in column `meta_col`
#'
#' @return `scaffold_df` if coverage matches expected, error otherwise
#' @export
#'
#' @examples
check_metadata_coverage <- function(scaffold_df, meta_col, expected_values){

  assertthat::assert_that(
    is_file_scaffold(scaffold_df),
    meta_col %in% names(scaffold_df)
  )

  if(
    !isTRUE(
      all(expected_values %in% scaffold_df[[meta_col]])
    )
  ){

    missing_values <- setdiff(expected_values,
                              scaffold_df[[meta_col]])

    stop(
      "The following expected values were missing from metadata column: ",
      meta_col,
      "\n",
      paste0(missing_values, collapse = ",\n")
    )

  }

  return(scaffold_df)

}
