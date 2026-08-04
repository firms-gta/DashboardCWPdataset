validate_comparison_dataset <- function(data, label) {
  if (!inherits(data, "data.frame")) {
    stop(label, " is not a data.frame.")
  }
  
  required_columns <- c(
    "measurement_value",
    "measurement_unit"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      label,
      " is missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  invisible(TRUE)
}