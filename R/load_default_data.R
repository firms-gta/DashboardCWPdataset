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
  
  # ---- Check files ----
  
  required_files <- c(dataset1_path, dataset2_path)
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0) {
    stop(
      "Missing preloaded datasets: ",
      paste(missing_files, collapse = ", ")
    )
  }
  
  # ---- Dataset preparation ----
  
  prepare_cwp_dataset <- function(x) {
    
    # Derived temporal columns
    if ("time_start" %in% names(x)) {
      
      if (!"year" %in% names(x)) {
        x$year <- as.integer(format(x$time_start, "%Y"))
      }
      
      if (!"month" %in% names(x)) {
        x$month <- as.integer(format(x$time_start, "%m"))
      }
      
      if (!"quarter" %in% names(x)) {
        x$quarter <- ((x$month - 1L) %/% 3L) + 1L
      }
    }
    
    # Harmonise categorical dimensions
    character_cols <- c(
      "source_authority",
      "species",
      "gear_type",
      "fishing_fleet",
      "fishing_mode",
      "geographic_identifier",
      "measurement",
      "measurement_type",
      "measurement_unit",
      "measurement_processing_level"
    )
    
    for (col in intersect(character_cols, names(x))) {
      x[[col]] <- as.character(x[[col]])
    }
    
    # Some updated datasets contain TRUE instead of "t"
    if ("measurement_unit" %in% names(x)) {
      
      x$measurement_unit <- as.character(x$measurement_unit)
      
      x$measurement_unit[
        !is.na(x$measurement_unit) &
          toupper(trimws(x$measurement_unit)) == "TRUE"
      ] <- "t"
    }
    
    x
  }
  
  # ---- Load datasets ----
  
  dataset1 <- qs::qread(dataset1_path)
  dataset2 <- qs::qread(dataset2_path)
  
  dataset1 <- prepare_cwp_dataset(dataset1)
  dataset2 <- prepare_cwp_dataset(dataset2)
  
  validate_cwp_dataset(dataset1, "dataset 1")
  validate_cwp_dataset(dataset2, "dataset 2")
  
  # ---- Parameters ----
  
  parameters_path <- Sys.getenv(
    "DASHBOARD_PARAMETERS_FILE",
    "data/default_parameters.csv"
  )
  
  parameters <- load_parameters_from_csv(parameters_path)
  
  # Docker image may override dataset titles
  docker_title1 <- Sys.getenv("DASHBOARD_TITLE_1", "")
  docker_title2 <- Sys.getenv("DASHBOARD_TITLE_2", "")
  
  if (nzchar(docker_title1)) {
    parameters$title1 <- docker_title1
  }
  
  if (nzchar(docker_title2)) {
    parameters$title2 <- docker_title2
  }
  
  # ---- Common columns ----
  
  common_columns <- intersect(
    names(dataset1),
    names(dataset2)
  )
  
  # Columns required by CWP.dataset
  mandatory_columns <- unique(c(
    "measurement_unit",
    "measurement_value",
    parameters$time_cols,
    parameters$geo_dim
  ))
  
  # Dimensions that may be included in the analysis
  dimension_columns <- setdiff(
    common_columns,
    c(
      mandatory_columns,
      "time_end",
      "year",
      "month",
      "quarter"
    )
  )
  
  # Columns available for interactive filtering.
  # time_start already has its dedicated range slider.
  # measurement_value is the fact value, not a categorical dimension.
  filter_columns <- setdiff(
    common_columns,
    c(
      "measurement_value",
      parameters$time_cols,
      "time_end"
    )
  )
  
  message(
    "Common columns: ",
    paste(common_columns, collapse = ", ")
  )
  
  message(
    "Selectable dimensions: ",
    paste(dimension_columns, collapse = ", ")
  )
  
  message(
    "Filterable columns: ",
    paste(filter_columns, collapse = ", ")
  )
  
  list(
    dataset1 = dataset1,
    dataset2 = dataset2,
    parameters = parameters,
    common_columns = common_columns,
    mandatory_columns = mandatory_columns,
    dimension_columns = dimension_columns,
    filter_columns = filter_columns
  )
}