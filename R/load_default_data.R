# Fonction de chargement conditionnel
load_default_data <- function(PRELOAD_DATA = FALSE, PRELOAD_DATA_PATH = "data/") {
  if (!PRELOAD_DATA) {
    message("Preloading désactivé (SHINY_PRELOAD_DATA != TRUE)")
    return(NULL)
  }
  
  if (!dir.exists(PRELOAD_DATA_PATH)) {
    message("Dossier de données non trouvé: ", PRELOAD_DATA_PATH)
    return(NULL)
  }
  
  default_data <- list()
  
  # Charger dataset1
  dataset1_path <- file.path(PRELOAD_DATA_PATH, "FS_MAPPED.qs")
  if (file.exists(dataset1_path)) {
    default_data$dataset1 <- tryCatch({
      message("chargement + renommer")
      qs::qread(dataset1_path) %>% dplyr::rename(time_start = year) %>% dplyr::mutate(measurement_value = round(measurement_value,3)) %>% dplyr::mutate(measurement_unit = "Tons")
    }, error = function(e) {
      message("Erreur chargement FSJ MAPPED dataset: ", e$message)
      NULL
    })
  }
  
  # Charger dataset2
  dataset2_path <- file.path(PRELOAD_DATA_PATH, "NCD_MAPPED.qs")
  if (file.exists(dataset2_path)) {
    default_data$dataset2 <- tryCatch({
      qs::qread(dataset2_path)%>% dplyr::mutate(measurement_value = round(measurement_value,3)) %>% dplyr::mutate(measurement_unit = "Tons")
    }, error = function(e) {
      message("Erreur chargement GTA nominal catch mapped: ", e$message)
      NULL
    })
  }
  
  # Charger les paramètres depuis CSV
  params_path <- file.path(PRELOAD_DATA_PATH, "default_parameters.csv")
  default_data$parameters <- load_parameters_from_csv(params_path)
  
  # Log du statut
  datasets_loaded <- sum(!sapply(default_data[1:2], is.null))
  message("✓ Préchargement: ", datasets_loaded, " dataset(s), ", 
          if (!is.null(default_data$parameters)) "paramètres" else "pas de paramètres")
  
  return(default_data)
}
