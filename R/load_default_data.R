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
  
  parameters <- load_parameters_from_csv(parameters_path)
  
  parameters$title1 <- Sys.getenv(
    "DASHBOARD_TITLE_1",
    parameters$title1
  )
  
  parameters$title2 <- Sys.getenv(
    "DASHBOARD_TITLE_2",
    parameters$title2
  )
  
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
  
  add_time_columns_if_missing <- function(x) {
    
    if (!"time_start" %in% names(x)) {
      return(x)
    }
    
    if (!"year" %in% names(x)) {
      x$year <- as.integer(format(x$time_start, "%Y"))
    }
    
    if (!"month" %in% names(x)) {
      x$month <- as.integer(format(x$time_start, "%m"))
    }
    
    if (!"quarter" %in% names(x)) {
      x$quarter <- ((x$month - 1L) %/% 3L) + 1L
    }
    
    x
  }
  prepare_cwp_dataset <- function(x) {
    
    # Colonnes temporelles dérivées
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
    
    # Harmonisation des dimensions catégorielles
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
    if ("measurement_unit" %in% names(x)) {
      
      x$measurement_unit <- as.character(x$measurement_unit)
      
      x$measurement_unit[
        !is.na(x$measurement_unit) &
          toupper(trimws(x$measurement_unit)) == "TRUE"
      ] <- "t"
    }
    
    x
  }

  dataset1 <- qs::qread(dataset1_path)
  dataset2 <- qs::qread(dataset2_path)
  
  dataset1 <- prepare_cwp_dataset(dataset1)
  dataset2 <- prepare_cwp_dataset(dataset2)
  
  validate_cwp_dataset(dataset1, "dataset 1")
  validate_cwp_dataset(dataset2, "dataset 2")
  
  parameters_path <- Sys.getenv(
    "DASHBOARD_PARAMETERS_FILE",
    "data/default_parameters.csv"
  )
  
  parameters <- load_parameters_from_csv(parameters_path)
  
  mandatory_columns <- c(
    "measurement_unit",
    "measurement_value",
    parameters$time_cols,
    parameters$geo_dim
  )
  
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
  
  list(
    dataset1 = dataset1,
    dataset2 = dataset2,
    parameters = parameters,
    common_columns = common_columns,
    mandatory_columns = mandatory_columns,
    dimension_columns = dimension_columns
  )
}
