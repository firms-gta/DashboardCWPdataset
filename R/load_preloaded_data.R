load_preloaded_data <- function() {
  dataset1_path <- Sys.getenv(
    "DASHBOARD_DATASET_1",
    "data/dataset_1.qs"
  )
  
  dataset2_path <- Sys.getenv(
    "DASHBOARD_DATASET_2",
    "data/dataset_2.qs"
  )
  
  dataset1 <- if (file.exists(dataset1_path)) {
    qs::qread(dataset1_path)
  } else {
    NULL
  }
  
  dataset2 <- if (file.exists(dataset2_path)) {
    qs::qread(dataset2_path)
  } else {
    NULL
  }
  
  list(
    dataset1 = dataset1,
    dataset2 = dataset2,
    mode = dplyr::case_when(
      is.null(dataset1) && is.null(dataset2) ~ "upload",
      !is.null(dataset1) && is.null(dataset2) ~ "single",
      !is.null(dataset1) && !is.null(dataset2) ~ "comparison",
      TRUE ~ "upload"
    )
  )
}