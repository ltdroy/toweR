#' Standardised runtime assertion: is file scaffold
#'
#' @param ob A candidate scaffold_df object
#'
#' @return TRUE
#'
is_file_scaffold <- function(ob){

  isTRUE(assertthat::assert_that(
    is.data.frame(ob),
    isTRUE(
      all(c("filename", "filepath", "file_ext") %in%
        names(ob))
    ),
    nrow(ob) > 0
  ))

}
