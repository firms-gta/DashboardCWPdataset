# ---- Dev options & packages ----
options(shiny.fullstacktrace = TRUE)
options(shiny.sanitize.errors = FALSE)
options(shiny.maxRequestSize = 200*1024^2)  # 200 MB upload limit

# suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(ggplot2)
  library(dplyr)
  library(shinycssloaders)
  require(stringr)
  library(CWP.dataset)
  library(qs)
  library(shinyjs)
# })

PRELOAD_DATA <- Sys.getenv("SHINY_PRELOAD_DATA", "TRUE") == "TRUE"
PRELOAD_DATA_PATH <- Sys.getenv("SHINY_PRELOAD_DATA_PATH", "data/")

source("R/load_default_data.R")
source("R/load_parameters_from_csv.R")

# Variables globales (comme avant)
shapefile.fix <- NULL
continent <- ""

default_data <- load_default_data(PRELOAD_DATA = PRELOAD_DATA,PRELOAD_DATA_PATH  = PRELOAD_DATA_PATH)

if(!PRELOAD_DATA){
  if(!exists("continent")){
    
    WFS <- ows4R::WFSClient$new(
      url = "https://www.fao.org/fishery/geoserver/fifao/wfs",
      serviceVersion = "1.0.0",
      logger = "INFO"
    )
    continent <- WFS$getFeatures("fifao:UN_CONTINENT2")
    sf::st_crs(continent) <- 4326
  }
  shapefile.fix <- NULL
  if(is.null(shapefile.fix)){
    cwp_grid_file <- system.file("extdata", "cl_areal_grid.csv", package = "CWP.dataset")
    if (!file.exists(cwp_grid_file)) {
      stop("cl_areal_grid.csv not found in inst/extdata - run data-raw/download_codelists.R")
    }
    shapefile.fix <- sf::st_read(cwp_grid_file, show_col_types = FALSE)
  }
  shapefile.fix <- sf::st_as_sf(shapefile.fix, wkt = "geom_wkt", crs = 4326)
  shapefile.fix <- dplyr::rename(shapefile.fix,
                                 cwp_code = CWP_CODE,
                                 geom     = geom_wkt)
  pm <- TRUE
}
# ---- Compatibility fallbacks (non-breaking) ----
if (!exists("%notin%")) `%notin%` <- function(x, y) !(x %in% y)
if (!exists("filtering_function")) { try({ filtering_function <<- CWP.dataset::filtering_function }, silent = TRUE) }
if (!exists("is_null_or_not_exist")) { is_null_or_not_exist <- function(x) { is.null(x) || length(x) == 0 } }
if (!exists("last_path_reduced")) { last_path_reduced <- function(x) basename(as.character(x)) }

# ---- Helpers: generic renderers to be resilient to list structures ----
is_ggplot <- function(x) inherits(x, "ggplot")

or_null <- function(a, b) if (!is.null(a)) a else b

render_any_plot <- function(x) {
  if (is.null(x)) return(NULL)
  if (is_ggplot(x)) return(x)
  if (inherits(x, "gg")) return(x)                 # catch-all for gg objects
  if (is.list(x)) {
    # Try direct single plot fields
    cand <- or_null(x$plot, or_null(x$plott, or_null(x$figure, or_null(x$gg, NULL))))
    if (!is.null(cand)) return(render_any_plot(cand))
    # Try lists of plots
    if (!is.null(x$plots) && length(x$plots) > 0) return(render_any_plot(x$plots[[1]]))
    if (!is.null(x$figures) && length(x$figures) > 0) return(render_any_plot(x$figures[[1]]))
    if (!is.null(x$barplots) && length(x$barplots) > 0) return(render_any_plot(x$barplots[[1]]))
  }
  ggplot() +
    annotate("text", x = 0, y = 0, label = "No plot available", size = 5) +
    theme_void()
}

extract_indexed_plot <- function(x, idx = 1) {
  if (is.null(x)) return(NULL)
  if (!is.null(x$plots) && length(x$plots) >= idx) return(x$plots[[idx]])
  if (!is.null(x$figures) && length(x$figures) >= idx) return(x$figures[[idx]])
  if (!is.null(x$barplots) && length(x$barplots) >= idx) return(x$barplots[[idx]])
  render_any_plot(x)
}

as_dt <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "data.frame")) return(x)
  if (is.list(x)) {
    cand <- or_null(x$table, or_null(x$df, or_null(x$data, or_null(x$datatable, NULL))))
    if (inherits(cand, "data.frame")) return(cand)
  }
  NULL
}



require(futile.logger)



if (PRELOAD_DATA) {
  
  time_cols <- if (!is.null(default_data$parameters$time_cols)) default_data$parameters$time_cols else "time_start"
  geo_dim   <- if (!is.null(default_data$parameters$geo_dim))   default_data$parameters$geo_dim   else "geographic_identifier"
  geo_group <- if (!is.null(default_data$parameters$geo_group)) default_data$parameters$geo_group else "gridtype"
  fact      <- if (!is.null(default_data$parameters$fact))      default_data$parameters$fact      else "catch"
  plotting  <- if (!is.null(default_data$parameters$plotting_type)) default_data$parameters$plotting_type else "view"
  coverage  <- if (!is.null(default_data$parameters$coverage))  isTRUE(default_data$parameters$coverage) else TRUE
  removemap <- if (!is.null(default_data$parameters$removemap)) isTRUE(default_data$parameters$removemap) else FALSE
  debug_small <- if (!is.null(default_data$parameters$debug_small)) isTRUE(default_data$parameters$debug_small) else FALSE
  continent <- if (!is.null(default_data$parameters$continent)) default_data$parameters$continent else ""
  parameter_colnames_to_keep <- if (!is.null(default_data$parameters$continent)) default_data$parameters$continent else ""
  
  PRELOADED_RESULT <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
    parameter_init = default_data$dataset1,
    parameter_final =  default_data$dataset2,
    fig.path = getwd(),
    parameter_fact =  fact,
    parameter_short = FALSE,
    parameter_columns_to_keep = c("Precision","measurement_unit","Values dataset 1","Values dataset 2","Loss / Gain","Difference (in %)","Dimension","Difference in value"),
    parameter_diff_value_or_percent = "Difference (in %)",
    parameter_time_dimension =  time_cols,
    parameter_geographical_dimension =  geo_dim,
    parameter_geographical_dimension_groupping =  geo_group,
    parameter_colnames_to_keep = c("fishing_fleet", "Ocean", "species_name", "measurement_unit", "measurement_value"),
    outputonly = FALSE,
    plotting_type =  plotting,
    continent = continent,
    coverage = isTRUE(coverage),
    print_map = FALSE,
    parameter_resolution_filter = NULL,
    parameter_titre_dataset_1 = "FS",
    parameter_titre_dataset_2 = "GTA",
    unique_analyse = FALSE,
    removemap = isTRUE(removemap),
    topnumber = 6
  )
  PRELOADED_RESULT$summary_of_differences <- 
    PRELOADED_RESULT$summary_of_differences %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))%>%
    dplyr::mutate(across(
      where(is.numeric),
      ~ format(.x, big.mark = " ", scientific = FALSE)
    ))
  
} else {
  PRELOADED_RESULT <- NULL
}


