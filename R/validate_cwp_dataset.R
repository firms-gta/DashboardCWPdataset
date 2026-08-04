validate_cwp_dataset <- function(data, dataset_name) {
  required_columns <- c(
    "measurement_value",
    "measurement_unit"
  )
  
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      dataset_name,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  invisible(TRUE)
}