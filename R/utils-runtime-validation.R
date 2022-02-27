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
