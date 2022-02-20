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

#' Remove any non-alphanumeric characters from a character vector
#'
#' @param x A character vector
#'
#' @return x with all instances of non-alphanumeric character replaced
#'  with an empty character
#'
#' @examples
string_alphanum_only <- function(x){

  assertthat::assert_that(
    is.character(x),
    msg = "Expected x to be a character vector"
  )


  y <- stringr::str_replace_all(
    x,
    pattern = "[^[:alnum:]]",
    replacement = ""
  )

  assertthat::assert_that(
    all(is.na(y) == is.na(x)),
    msg = "Unexpected NA mismatch created by string_alphanum_only()"
  )

  return(y)

}
