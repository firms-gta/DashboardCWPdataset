load_parameters_from_csv <- function(file_path) {
  if (!file.exists(file_path)) {
    message("Fichier de paramètres non trouvé: ", file_path)
    return(NULL)
  }
  
  tryCatch({
    # Lire le CSV
    params_df <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    
    # Convertir en liste nommée
    parameters <- as.list(params_df$value)
    names(parameters) <- params_df$parameter
    
    # Convertir les types de données
    parameters <- lapply(parameters, function(x) {
      # Essayer de convertir en numérique si possible
      num_val <- suppressWarnings(as.numeric(x))
      if (!is.na(num_val)) return(num_val)
      
      # Convertir "TRUE"/"FALSE" en logique
      if (tolower(x) %in% c("true", "false")) {
        return(tolower(x) == "true")
      }
      
      # Convertir les listes (ex: "a,b,c" → c("a","b","c"))
      if (grepl(",", x)) {
        return(strsplit(x, "\\s*,\\s*")[[1]])
      }
      
      # Retourner la chaîne par défaut
      return(x)
    })
    
    message("✓ Paramètres chargés depuis: ", file_path)
    return(parameters)
    
  }, error = function(e) {
    message("❌ Erreur lecture paramètres CSV: ", e$message)
    return(NULL)
  })
}