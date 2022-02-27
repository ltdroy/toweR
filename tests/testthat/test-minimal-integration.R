testthat::test_that(
  "Minimal integration",
  {

    tower_df_minimal <- build_scaffolding_from_dir(dir_path = "test-data",
                               file_exts = "csv",
                               verbose = TRUE) %>%
      metadata_col_from_regex(., list("data_index"="\\d")) %>%
      metadata_col_from_lookup(., lookup_list = list(
        tibble::tibble(
          filename = c("minimal-integration-testfile1.csv",
                       "minimal-integration-testfile2.csv",
                       "minimal-integration-testfile3.csv"),
          date_of_collection = c("2021-01-01", "2022-01-01", "2023-01-01") %>% lubridate::as_date()
        )
      )) %>%
      add_linestarts() %>%
      add_sheet_selections() %>%
      check_metadata_coverage(., meta_col = "data_index",
                              expected_values = c(1,2,3)) %>%
      load_materials_simple() %>%
      harmonise_by_explicit(
        .,
        capture_list = list(
          "a" = c("A"),
          "e" = c("letter e")
        )
      ) %>%
      vertical_merge(
        .,
        selections = TRUE,
        diagnostics = TRUE,
        keep_diagnostics = TRUE,
        add_metadata = TRUE
      )

    expected_output <- tibble::tribble(
                         ~a, ~b, ~c, ~d, ~e, ~f,                           ~filename,  ~filepath,                                    ~file_ext, ~data_index, ~date_of_collection, ~linestart, ~sheet_selection,
                         1L, 2L, 3L, 4L, 5L, 6L, "minimal-integration-testfile1.csv", "test-data/minimal-integration-testfile1.csv",     "csv",          1L,        "01/01/2021",         1L,               1L,
                         1L, 2L, 3L, 4L, 5L, 6L, "minimal-integration-testfile1.csv", "test-data/minimal-integration-testfile1.csv",     "csv",          1L,        "01/01/2021",         1L,               1L,
                         1L, 2L, 3L, 4L, 5L, 6L, "minimal-integration-testfile2.csv", "test-data/minimal-integration-testfile2.csv",     "csv",          2L,        "01/01/2022",         1L,               1L,
                         1L, 2L, 3L, 4L, 5L, 6L, "minimal-integration-testfile2.csv", "test-data/minimal-integration-testfile2.csv",     "csv",          2L,        "01/01/2022",         1L,               1L,
                         1L, 2L, 3L, 4L, 5L, NA, "minimal-integration-testfile3.csv", "test-data/minimal-integration-testfile3.csv",     "csv",          3L,        "01/01/2023",         1L,               1L,
                         1L, 2L, 3L, 4L, 5L, NA, "minimal-integration-testfile3.csv", "test-data/minimal-integration-testfile3.csv",     "csv",          3L,        "01/01/2023",         1L,               1L
                         ) %>%
      dplyr::mutate(
        dplyr::across(
          c(a,b,c,d,e,f, data_index),
          ~ as.character(.x)
        ),
        date_of_collection = lubridate::dmy(date_of_collection),
        filepath = fs::as_fs_path(filepath)
      )


    testthat::expect_equal(
      object = tower_df_minimal,
      expected = expected_output,
      ignore_attr = TRUE
    )

    testthat::expect_equal(
      sum(attr(tower_df_minimal, "diagnostics")[[1]] == 1),
      length(unique(tower_df_minimal$filename)) * ncol(tower_df_minimal)
    )

    testthat::expect_equal(
      sum(attr(tower_df_minimal, "diagnostics")[[3]] == 1),
      1
    )

    testthat::expect_true(
      eval_colname_diagnostics(attr(tower_df_minimal, "diagnostics")[[1]], tower_df = tower_df_minimal)
    )

    testthat::expect_true(
      eval_coltype_diagnostics(attr(tower_df_minimal, "diagnostics")[[2]], tower_df = tower_df_minimal)
    )

  }
)

