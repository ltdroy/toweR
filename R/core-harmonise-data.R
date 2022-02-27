harmonise_by_explicit <- function(scaffold_df, capture_list, verbose = TRUE){

  assertthat::assert_that(
    is_file_scaffold(scaffold_df),
    is.list(capture_list),
    length(names(capture_list)) == length(capture_list),
    "datasets" %in% names(scaffold_df)
  )

  scaffold_df[["datasets"]] <- purrr::map2(
    scaffold_df[["datasets"]],
    scaffold_df[["filename"]],
    rename_cols_by_capture_list,
    capture_list = capture_list,
    verbose = verbose
  )

  return(scaffold_df)

}

rename_cols_by_capture_list <- function(df, df_name, capture_list, verbose){

  for(group_name in names(capture_list)){

    # Bool, is column name in capture group
    capture_bool <- names(df) %in% capture_list[[group_name]]

    # Check that 0 or 1 column names are in capture group
    assertthat::assert_that(
      sum(capture_bool) %in% c(0,1),
      msg = paste0(
        "Multiple columns captured in the same capture group:\n",
        paste0(names(df)[capture_bool], collapse = ",\n"))
    )

    if(isTRUE(verbose) && sum(capture_bool) == 1){
      message("Column: '",
              names(df)[capture_bool],
              "' renamed as '",
              group_name,
              "' in file:\n",
              df_name
              )
    }

    # Rename the captured column (if any) with the group name
    names(df)[capture_bool] <- group_name

  }

  return(df)

}
