#' Build a data scaffold (tibble) from the contents of a directory
#'
#' @param dir_path File directory path (string).
#' @param file_exts If NULL (default), all files in the directory are included in the scaffold,
#' otherwise, if a character vector is supplied, all files with matching extensions (e.g. 'csv')
#' are included. Any non-alphanumeric characters will be removed (e.g. '.csv' will become 'csv')
#' to match with the extracted file extension.
#' @param verbose If TRUE (default: FALSE), reports a list of the files added to the console.
#'
#' @return A data.frame/tibble with one row per-file in the directory, and columns with file metadata:
#'
#' 1. filename - Name of the file
#' 2. filepath - Path to the file
#' 3. file_ext - File extension (e.g. 'csv')
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  build_scaffolding_from_dir(dir_path = "raw_data/csv_files/")
#' }
build_scaffolding_from_dir <- function(dir_path,
                                       file_exts = NULL,
                                       verbose = FALSE){


  assertthat::assert_that(
    is.character(dir_path),
    length(dir_path) == 1,
    fs::dir_exists(dir_path),
    is.character(file_exts) | is.null(file_exts)
  )

  initial_scaffold <- fs::dir_info(dir_path)

  initial_scaffold[["file_ext"]] <- fs::path_ext(initial_scaffold[["path"]])

  if(!is.null(file_exts)){

    file_exts <- string_alphanum_only(file_exts)

    initial_scaffold <- initial_scaffold %>%
      dplyr::filter(
        file_ext %in% file_exts
      )

  }

  initial_scaffold[["filepath"]] <- initial_scaffold[["path"]]

  initial_scaffold[["filename"]] <- fs::path_file(initial_scaffold[["path"]])


  initial_scaffold <- initial_scaffold %>%
    dplyr::select(
      filename,
      filepath,
      file_ext
    )

  if(isTRUE(verbose)){

    message(
      paste0(
        "Metadata loaded for files:\n\n",
        paste0(
          seq_len(nrow(initial_scaffold)), ". ",
          initial_scaffold[["filename"]],
          collapse = ",\n"
        )
      )
    )

  }

  return(initial_scaffold)

}
