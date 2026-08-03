load_default_data <- function(
    preload = Sys.getenv("SHINY_PRELOAD_DATA", "FALSE") == "TRUE",
    dataset1_path = Sys.getenv(
      "DASHBOARD_DATASET_1",
      "data/dataset_1.qs"
    ),
    dataset2_path = Sys.getenv(
      "DASHBOARD_DATASET_2",
      "data/dataset_2.qs"
    )
) {
  if (!preload) {
    message("Preloading disabled")
    return(NULL)
  }
  
  required_files <- c(dataset1_path, dataset2_path)
  
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0) {
    stop(
      "Missing preloaded datasets: ",
      paste(missing_files, collapse = ", ")
    )
  }
  
  dataset1 <- qs::qread(dataset1_path)
  dataset2 <- qs::qread(dataset2_path)
  
  validate_cwp_dataset(dataset1, "dataset 1")
  validate_cwp_dataset(dataset2, "dataset 2")
  
  list(
    dataset1 = prepare_cwp_dataset(dataset1),
    dataset2 = prepare_cwp_dataset(dataset2),
    parameters = load_dashboard_parameters()
  )
}