source("global.R", local = TRUE)

# ---- UI ----
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Comparison", tabName = "comparison", icon = icon("exchange-alt")),
      menuItem("Overview", tabName = "run", icon = icon("play")),
      if (!PRELOAD_DATA) {
        menuItem("Coverage", tabName = "coverage", icon = icon("sliders"))
      },
      if (!PRELOAD_DATA) {
        menuItem("Spatial coverage", tabName = "spatial", icon = icon("globe"))
      },
      if (!PRELOAD_DATA) {
        menuItem("Combined summary", tabName = "summary", icon = icon("bar-chart"))
      }
    ),
    hr(),
    
    # --- Dataset 1 ---
   
    div(id = "Dataset_1_mode_block",
        h4("Dataset 1")
    ),
    fileInput("file_init", "Upload dataset 1 (CSV/RDS/RData)", accept = c(".csv", ".rds", ".RDS", ".rdata", ".RData")),
    checkboxInput("has_header", "CSV has header", TRUE),
    textInput("sep", "CSV separator", ","),
    div(id = "requirement_bloc_mode_block",
        tags$small("Required columns: measurement_value, time_start (Date/character), geographic_identifier (optional), gridtype (optional).")
    ),
    
    # --- Dataset 2 (pour comparaison) ---
    div(id = "Dataset_2_mode_block",
        h4("Dataset 2")
    ),
    fileInput("file_final", "Upload dataset 2 (CSV/RDS/RData)", accept = c(".csv", ".rds", ".RDS", ".rdata", ".RData")),
    checkboxInput("has_header2", "CSV has header", TRUE),
    textInput("sep2", "CSV separator", ","),
    
    hr(),
    div(id = "analysis_mode_block",
        h4("Analysis Mode"),
    radioButtons("analysis_mode", "Mode:",
                 choices = c("Unique analysis" = "unique", "Comparison" = "comparison"),
                 selected = "unique")
    ),
    div(id = "analysis_parameter",
        h4("Parameters")),
    textInput("title1", "Dataset 1 title", value = "Dataset 1"),
    textInput("title2", "Dataset 2 title", value = "Dataset 2"),
    selectInput("time_cols", "Time columns", choices = c("time_start"), selected = "time_start", multiple = TRUE),
    textInput("geo_dim", "Geo dimension column", value = "geographic_identifier"),
    textInput("geo_group", "Geo grouping column", value = "gridtype"),
    selectInput("fact", "Fact", choices = c("catch", "effort"), selected = "catch"),
    checkboxInput("coverage", "Compute coverage analyses", TRUE),
    checkboxInput("print_map", "Enable map generation (if available)", TRUE),
    selectInput("plotting_type", "Plotting type", choices = c("view", "static", "interactive"), selected = "view"),
    checkboxInput("removemap", "Remove map objects from outputs", FALSE),
    textInput("continent", "Continent filter (optional)", value = ""),
    checkboxInput("debug_small", "DEBUG: run on first 5000 rows", FALSE),
    
    hr(),
    h4("Filters (species / fleet)"),
    uiOutput("time_start_filter_ui"),
    selectizeInput("species_name", "Species", choices = NULL, multiple = TRUE, options = list(placeholder = "Select species...")),
    selectizeInput("fishing_fleet_label", "Fishing fleet", choices = NULL, multiple = TRUE, options = list(placeholder = "Select fleets...")),
    # selectizeInput("COUNTRY", "COUNTRY", choices = NULL, multiple = TRUE, options = list(placeholder = "Select COUNTRY")),
    # selectizeInput("species_aggregate", "species_aggregate", choices = NULL, multiple = TRUE, options = list(placeholder = "Select species_aggregate")),
    selectizeInput("Ocean", "Ocean", choices = NULL, multiple = TRUE, options = list(placeholder = "Select Ocean")),
    
    hr(),
    actionButton("run_btn", "Run analysis", icon = icon("rocket"), class = "btn-primary")
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML(".content-wrapper { background: #0b1220; color: #e5e7eb;} .box { border-radius: 1rem; } .box-header { border-bottom: 1px solid #111827;} .small-box {border-radius: 1rem;}"))),
    
    # message en haut si mode préchargé
    uiOutput("preload_banner"),
    
    tabItems(
      tabItem(
        tabName = "run",
        fluidRow(
          valueBoxOutput("vb_rows", width = 3),
          valueBoxOutput("vb_cols", width = 3),
          valueBoxOutput("vb_rows2", width = 3),
          valueBoxOutput("vb_cols2", width = 3)
        ),
        fluidRow(
          box(width = 6, title = "Preview Dataset 1 (first 5 rows)", status = "primary", solidHeader = TRUE,
              withSpinner(DTOutput("table_preview"))),
          box(width = 6, title = "Preview Dataset 2 (first 5 rows)", status = "warning", solidHeader = TRUE,
              withSpinner(DTOutput("table_preview2_home")))
        ),
        # fluidRow(
        #   box(width = 12, title = "Status / logs", status = "primary", solidHeader = TRUE,
        #       verbatimTextOutput("status_log"))
        # ),
        # fluidRow(
        #   box(width = 12, title = "Session / Client info", status = "primary", solidHeader = TRUE,
        #       verbatimTextOutput("client_info"))
        # )
      ),
      
      # --- Nouvel onglet Comparaison ---
      tabItem(
        tabName = "comparison",
        # fluidRow(
        #   valueBoxOutput("vb_rows2"),
        #   valueBoxOutput("vb_cols2"),
        #   # valueBoxOutput("vb_mem2")
        # ),
        fluidRow(
          box(width = 12, title = "Comparison Results", status = "info", solidHeader = TRUE,
              withSpinner(uiOutput("comparison_results")))
        )
      ),
      
      # tabItem(
      #   tabName = "coverage",
      #   fluidRow(
      #     box(width = 12, title = "Time coverage (plots)", status = "info", solidHeader = TRUE,
      #         uiOutput("time_cov_tabs")
      #     )
      #   )
      #   ,
      #   fluidRow(
      #     box(width = 12, title = "Other dimensions (plots)", status = "info", solidHeader = TRUE,
      #         uiOutput("other_cov_tabs")
      #     )
      #   )
      # ),
      tabItem(
        tabName = "spatial",
        fluidRow(
          box(width = 12, title = "Spatial coverage (plot)", status = "warning", solidHeader = TRUE,
              withSpinner(plotOutput("spatial_cov_plot", height = 420)))
        )
      ),
      tabItem(
        tabName = "summary",
        fluidRow(
          box(width = 12, title = "Combined summary histogram", status = "success", solidHeader = TRUE,
              withSpinner(plotOutput("combined_summary_plot", height = 420)))
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # --- MODE PRECHARGE DETECTE ICI ---
  preload_mode <- isTRUE(PRELOAD_DATA) && exists("default_data") && !is.null(default_data) && is.list(default_data)
  has_dataset1 <- preload_mode && !is.null(default_data$dataset1)
  has_dataset2 <- preload_mode && !is.null(default_data$dataset2)
  analysis <- reactiveVal(if (preload_mode && !is.null(PRELOADED_RESULT)) PRELOADED_RESULT else NULL)
  # --- Ajuster l'UI si preload_mode ---
  if (preload_mode) {
    observe({
      # cacher les uploads
      shinyjs::hide("file_init")
      shinyjs::hide("has_header")
      shinyjs::hide("sep")
      
      shinyjs::hide("file_final")
      shinyjs::hide("has_header2")
      shinyjs::hide("sep2")
      
      # cacher paramètres "avancés" qu'on ne veut pas voir
      shinyjs::hide("analysis_mode")
      shinyjs::hide("time_cols")
      shinyjs::hide("geo_dim")
      shinyjs::hide("geo_group")
      shinyjs::hide("fact")
      shinyjs::hide("coverage")
      shinyjs::hide("print_map")
      shinyjs::hide("plotting_type")
      shinyjs::hide("removemap")
      shinyjs::hide("continent")
      shinyjs::hide("debug_small")
      shinyjs::hide("analysis_mode_block")
      shinyjs::hide("analysis_parameter")
      shinyjs::hide("Dataset_1_mode_block")
      shinyjs::hide("Dataset_2_mode_block")
      shinyjs::hide("requirement_bloc_mode_block")
      shinyjs::hide("menu_spatial")
      
      # désactiver les titres (mais on les remplit)
      shinyjs::hide("title1")
      shinyjs::hide("title2")
    })
    
    # pré-remplir titres + mode comparaison
    observe({
      params <- default_data$parameters
      if (!is.null(params$title1)) {
        updateTextInput(session, "title1", value = params$title1)
      } else {
        updateTextInput(session, "title1", value = "CAPTURED MAPPED")
      }
      if (!is.null(params$title2)) {
        updateTextInput(session, "title2", value = params$title2)
      } else if (has_dataset2) {
        updateTextInput(session, "title2", value = "NCD MAPPED")
      }
      
      updateRadioButtons(session, "analysis_mode", selected = "comparison")
    })
  }
  
  # bannière en haut si mode préchargé
  output$preload_banner <- renderUI({
    if (!preload_mode) return(NULL)
    tags$div(
      style = "background-color:#d4edda;color:#155724;padding:10px;margin:10px;border-radius:6px;",
      tags$strong("✅ Preloaded mode enabled."),
      tags$br(),
      tags$small(
        paste0(
          "Dataset 1: ", if (has_dataset1) paste0(nrow(default_data$dataset1), " rows, ", ncol(default_data$dataset1), " cols") else "missing",
          if (has_dataset2) paste0(" | Dataset 2: ", nrow(default_data$dataset2), " rows, ", ncol(default_data$dataset2), " cols") else ""
        )
      )
    )
  })
  
  output$time_start_filter_ui <- renderUI({
    df <- get_active_dataset1()
    req(df)
    req("time_start" %in% names(df))
    
    # on convertit en Date (si déjà Date ça ne casse pas)
    x <- df$time_start
    x <- as.Date(x)
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NULL)
    
    sliderInput(
      "time_start_range",
      "Time start (range)",
      min = min(x), max = max(x),
      value = c(min(x), max(x)),
      timeFormat = "%Y-%m-%d"
    )
  })
  
  
  # --- Client info / session diag ---
  output$client_info <- renderText({
    cd <- session$clientData
    paste(c(
      paste("url_hostname:", cd$url_hostname),
      paste("pixelratio:", cd$pixelratio),
      paste("browser version:", cd$browser$name, cd$browser$version),
      paste("screen:", cd$screen$width, "x", cd$screen$height)
    ), collapse = "\n")
  })
  
  # --- Load dataset 1 (UPLOAD) ---
  load_dataset <- reactive({
    req(input$file_init)
    ext <- tools::file_ext(input$file_init$name)
    path <- input$file_init$datapath
    if (tolower(ext) == "csv") {
      read.csv(path, sep = input$sep, header = isTRUE(input$has_header), check.names = FALSE)
    } else if (tolower(ext) %in% c("rds", "rdata", "rda")) {
      .obj <- NULL
      if (tolower(ext) == "rds") {
        .obj <- readRDS(path)
      } else {
        e <- new.env()
        load(path, envir = e)
        cand <- Filter(is.data.frame, as.list(e))
        validate(need(length(cand) > 0, "No data.frame found in the RData file."))
        .obj <- cand[[1]]
      }
      as.data.frame(.obj)
    } else {
      validate(need(FALSE, "Unsupported file format. Use CSV or RDS/RData."))
    }
  })
  
  # --- Load dataset 2 (UPLOAD) ---
  load_dataset2 <- reactive({
    req(input$file_final)
    ext <- tools::file_ext(input$file_final$name)
    path <- input$file_final$datapath
    if (tolower(ext) == "csv") {
      read.csv(path, sep = input$sep2, header = isTRUE(input$has_header2), check.names = FALSE)
    } else if (tolower(ext) %in% c("rds", "rdata", "rda")) {
      .obj <- NULL
      if (tolower(ext) == "rds") {
        .obj <- readRDS(path)
      } else {
        e <- new.env()
        load(path, envir = e)
        cand <- Filter(is.data.frame, as.list(e))
        validate(need(length(cand) > 0, "No data.frame found in the RData file."))
        .obj <- cand[[1]]
      }
      as.data.frame(.obj)
    } else {
      validate(need(FALSE, "Unsupported file format. Use CSV or RDS/RData."))
    }
  })
  
  # --- DATASETS ACTIFS (UPLOAD OU PRECHARGES) ---
  get_active_dataset1 <- reactive({
    if (preload_mode && has_dataset1) {
      default_data$dataset1
    } else if (!is.null(input$file_init)) {
      load_dataset()
    } else {
      NULL
    }
  })
  
  get_active_dataset2 <- reactive({
    if (preload_mode && has_dataset2) {
      default_data$dataset2
    } else if (!is.null(input$file_final)) {
      load_dataset2()
    } else {
      NULL
    }
  })
  
  # logs upload (mode upload uniquement)
  observeEvent(input$file_init, {
    info <- try(file.info(input$file_init$datapath), silent = TRUE)
    # append_log_local("File 1 uploaded:", input$file_init$name,
    #                  "| ext:", tools::file_ext(input$file_init$name),
    #                  "| size:", if (!inherits(info, "try-error")) format(info$size, big.mark = " ") else "?")
  })
  
  observeEvent(input$file_final, {
    info <- try(file.info(input$file_final$datapath), silent = TRUE)
    # append_log_local("File 2 uploaded:", input$file_final$name,
    #                  "| ext:", tools::file_ext(input$file_final$name),
    #                  "| size:", if (!inherits(info, "try-error")) format(info$size, big.mark = " ") else "?")
  })
  
  # --- PREVIEW TABLES ---
  output$table_preview <- renderDT({
    df <- get_active_dataset1()
    req(df)
    datatable(
      utils::head(df, 5),
      options = list(pageLength = 5, scrollX = TRUE, searching = FALSE),
      filter = "none"
    )
  })
  
  output$table_preview2_home <- renderDT({
    df2 <- get_active_dataset2()
    if (is.null(df2)) return(NULL)
    datatable(
      utils::head(df2, 5),
      options = list(pageLength = 5, scrollX = TRUE, searching = FALSE),
      filter = "none"
    )
  })
  
  # --- Populate dynamic filter choices + valueBoxes dataset1 ---
  observe({
    df <- get_active_dataset1()
    if (!inherits(df, "data.frame")) return()
    
    output$vb_rows <- renderValueBox({ valueBox(format(nrow(df), big.mark = " "), "Rows Dataset 1", icon = icon("hashtag"), color = "purple") })
    output$vb_cols <- renderValueBox({ valueBox(format(ncol(df), big.mark = " "), "Columns Dataset 1", icon = icon("table"), color = "purple") })
    mem <- format(object.size(df), units = "auto")
    output$vb_mem <- renderValueBox({ valueBox(mem, "Approx. size Dataset 1", icon = icon("database"), color = "purple") })
    
    if ("species_name" %in% names(df)) updateSelectizeInput(session, "species_name", choices = sort(unique(df$species_name)), server = TRUE)
    if ("fishing_fleet_label" %in% names(df)) updateSelectizeInput(session, "fishing_fleet_label", choices = sort(unique(df$fishing_fleet_label)), server = TRUE)
    if(PRELOAD_DATA){
      # if ("COUNTRY" %in% names(df)) updateSelectizeInput(session, "COUNTRY", choices = sort(unique(df$COUNTRY)), server = TRUE)
      if ("Ocean" %in% names(df)) updateSelectizeInput(session, "Ocean", choices = sort(unique(df$Ocean)), server = TRUE)
      # if ("species_aggregate" %in% names(df)) updateSelectizeInput(session, "species_aggregate", choices = sort(unique(df$species_aggregate)), server = TRUE)
      
    }
    
  })
  
  # --- valueBoxes dataset2 ---
  observe({
    df2 <- get_active_dataset2()
    if (!inherits(df2, "data.frame")) return()
    
    output$vb_rows2 <- renderValueBox({ valueBox(format(nrow(df2), big.mark = " "), "Rows Dataset 2", icon = icon("hashtag"), color = "orange") })
    output$vb_cols2 <- renderValueBox({ valueBox(format(ncol(df2), big.mark = " "), "Columns Dataset 2", icon = icon("table"), color = "orange") })
    mem <- format(object.size(df2), units = "auto")
    output$vb_mem2 <- renderValueBox({ valueBox(mem, "Approx. size Dataset 2", icon = icon("database"), color = "orange") })
  })
  
  # --- Button click tracer ---
  # observeEvent(input$run_btn, {
  #   mode <- input$analysis_mode
  #   # append_log_local("Button clicked. Mode:", mode,
  #   #                  paste0("time_cols=", paste(input$time_cols, collapse=",")),
  #   #                  paste0("geo_dim=", input$geo_dim),
  #   #                  paste0("geo_group=", input$geo_group),
  #   #                  paste0("fact=", input$fact),
  #   #                  paste0("coverage=", input$coverage),
  #   #                  paste0("print_map=", input$print_map),
  #   #                  paste0("plotting_type=", input$plotting_type),
  #   #                  paste0("species_n=", length(input$species)),
  #   #                  paste0("fleet_n=", length(input$fleet)),
  #   #                  paste0("debug_small=", input$debug_small))
  # }, ignoreInit = TRUE)
  
  # --- Run analysis avec mode unique ou comparaison ---
  observeEvent(input$run_btn, {
    # mode forcé en comparaison si preload_mode
    mode <- if (preload_mode) "comparison" else input$analysis_mode
    
    # valeurs par défaut (ou paramètres UI si pas preload)
    time_cols <- if (!is.null(input$time_cols)) input$time_cols else "time_start"
    geo_dim   <- if (!is.null(input$geo_dim))   input$geo_dim   else "geographic_identifier"
    geo_group <- if (!is.null(input$geo_group)) input$geo_group else "gridtype"
    fact      <- if (!is.null(input$fact))      input$fact      else "catch"
    plotting  <- if (!is.null(input$plotting_type)) input$plotting_type else "view"
    coverage  <- if (!is.null(input$coverage)) isTRUE(input$coverage) else TRUE
    removemap <- if (!is.null(input$removemap)) isTRUE(input$removemap) else FALSE
    debug_small <- if (!is.null(input$debug_small)) isTRUE(input$debug_small) else FALSE
    continent_input <- if (!is.null(input$continent)) input$continent else ""
    parameter_colnames_to_keep <- if (PRELOAD_DATA) c("fishing_fleet_label", "Ocean", "species_name") else "all"
    
    df <- get_active_dataset1()
    if (is.null(df)) {
      showNotification("Dataset 1 is missing (neither preloaded nor uploaded).", type = "error")
      return()
    }
    
    if (mode == "unique") {
      parameter_final <- df
    } else {
      parameter_final <- get_active_dataset2()
      if (is.null(parameter_final)) {
        showNotification("Dataset 2 is missing for comparison (neither preloaded nor uploaded).", type = "error")
        return()
      }
    }
    
    if (!"measurement_value" %in% names(df)) {
      showNotification("Missing required column: 'measurement_value' in dataset 1.", type = "error", duration = 8)
      return()
    }
    
    if (isTRUE(debug_small) && nrow(df) > 5000) {
      df <- utils::head(df, 5000)
    }
    if (mode == "comparison" && isTRUE(debug_small) && nrow(parameter_final) > 5000) {
      parameter_final <- utils::head(parameter_final, 5000)
    }
    
    filt <- list(
      species_name = if (length(input$species_name) > 0) input$species_name else NULL,
      fishing_fleet_label = if (length(input$fishing_fleet_label) > 0) input$fishing_fleet_label else NULL,
      # COUNTRY = if (length(input$COUNTRY) > 0) input$COUNTRY else NULL,
      # species_aggregate = if (length(input$species_aggregate) > 0) input$species_aggregate else NULL,
      Ocean = if (length(input$Ocean) > 0) input$Ocean else NULL
      
    )
    
    t0 <- Sys.time()
    
    # --- filtre temps sur df et parameter_final ---
    if (!is.null(input$time_start_range) && "time_start" %in% names(df)) {
      rng <- as.Date(input$time_start_range)
      df$time_start <- as.Date(df$time_start)
      df <- df[df$time_start >= rng[1] & df$time_start <= rng[2], , drop = FALSE]
    }
    
    if (mode == "comparison" && !is.null(input$time_start_range) && "time_start" %in% names(parameter_final)) {
      rng <- as.Date(input$time_start_range)
      parameter_final$time_start <- as.Date(parameter_final$time_start)
      parameter_final <- parameter_final[parameter_final$time_start >= rng[1] &
                                           parameter_final$time_start <= rng[2], , drop = FALSE]
    }
    
    
    withProgress(message = if (mode == "unique") "Running unique analysis. Please wait..." else "Running comparison. Please wait...", value = 0.1, {
      tmp <- tryCatch(
        {
          pm <- if (isTRUE(input$print_map) && !is.null(shapefile.fix)) {
            TRUE
          } else if (isTRUE(input$print_map) && is.null(shapefile.fix)) {
            FALSE
          } else FALSE
          

          r <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
            parameter_init = df,
            parameter_final = parameter_final,
            fig.path = getwd(),
            parameter_fact = fact,
            parameter_short = FALSE,
            parameter_columns_to_keep = c("Precision","measurement_unit","Values dataset 1","Values dataset 2","Loss / Gain","Difference (in %)","Dimension","Difference in value"),
            parameter_diff_value_or_percent = "Difference (in %)",
            parameter_filtering = filt,
            parameter_time_dimension = time_cols,
            parameter_geographical_dimension = geo_dim,
            parameter_geographical_dimension_groupping = geo_group,
            parameter_colnames_to_keep = if (PRELOAD_DATA) c("fishing_fleet_label", "Ocean", "species_name", "measurement_unit", "measurement_value") else "all",
            outputonly = FALSE,
            plotting_type = plotting,
            print_map = pm,
            shapefile_fix = shapefile.fix,
            continent = continent_input,
            coverage = isTRUE(coverage),
            parameter_resolution_filter = NULL,
            parameter_titre_dataset_1 = input$title1,
            parameter_titre_dataset_2 = input$title2,
            unique_analyse = (mode == "unique"),
            removemap = isTRUE(removemap),
            topnumber = 6
          )
          r$summary_of_differences <- 
            r$summary_of_differences %>%
            dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))%>%
            dplyr::mutate(across(
              where(is.numeric),
              ~ format(.x, big.mark = " ", scientific = FALSE)
            ))
          list(res = r)
        },
        error = function(e) {
          showModal(modalDialog(title = "Analysis error", easyClose = TRUE,
                                tagList(tags$pre(conditionMessage(e)))))
          NULL
        }
      )
      
      res <- if (is.null(tmp)) NULL else tmp$res
      
      if (!is.list(res)) {
        return()
      }
      
      # ⬅️ c’est ici qu’on met à jour le reactiveVal
      analysis(res)
      
      incProgress(0.9)
    })
  }, ignoreInit = TRUE)
  
  if (preload_mode && has_dataset1 && has_dataset2) {
    observeEvent(TRUE, {
      updateRadioButtons(session, "analysis_mode", selected = "comparison")
      # append_log_local("Auto-running analysis in preloaded comparison mode...")
      session$sendCustomMessage("autoClick", "run_btn")
    }, once = TRUE)
  }
  
  # --- Output pour l'onglet Comparaison ---
  output$comparison_results <- renderUI({
    res <- analysis()
    
    # 1. Rien du tout
    if (is.null(res)) {
      return(tags$em("No analysis result available yet. Run an analysis first."))
    }
    
    # 2. Vérifier que les objets de comparaison existent
    has_comparison_data <- !is.null(res$summary_of_differences) || 
      !is.null(res$compare_strata_differences_list) ||
      !is.null(res$compare_dimension_differences_list) ||
      !is.null(res$Geographicdiff)
    
    if (!has_comparison_data) {
      # Petit debug utile pour toi :
      return(tagList(
        tags$em("No specific comparison data found in results."),
        tags$br(),
        tags$small(
          paste("Result elements:", paste(names(res), collapse = ", "))
        )
      ))
    }
    
    tagList(
      # Summary of differences
      if (!is.null(res$summary_of_differences)) {
        box(width = 12, title = "Summary of Differences", status = "primary", solidHeader = TRUE,
            DTOutput("summary_differences_table"))
      },
      
      # Strata differences
      if (!is.null(res$compare_strata_differences_list)) {
        box(width = 12, title = "Strata Differences Analysis", status = "warning", solidHeader = TRUE,
            uiOutput("strata_differences_ui"))
      },
      
      # Temporal differences
      if (!is.null(res$plot_titles_list) && !is.null(res$plot_titles_list$plots)) {
        fluidRow(
          
          # --- Bloc de gauche : temporal plots ---
          column(
            width = 6,
            box(
              width = 12, title = "Temporal Differences",
              status = "success", solidHeader = TRUE,
              uiOutput("temporal_differences_ui")
            )
          ),
          
          column(
            width = 6,
            box(
              width = 12, title = "Temporal coverage",
              status = "primary", solidHeader = TRUE,
              uiOutput("time_cov_tabs")   
            )
          )
          
        )
      },
      
      # Geographic differences
      if (!is.null(res$Geographicdiff) | !PRELOAD_DATA) {
        box(width = 12, title = "Geographic Differences", status = "danger", solidHeader = TRUE,
            plotOutput("geographic_diff_plot", height = 500),
            htmlOutput("geographic_diff_title"))
      },
      
      # Dimension differences
      if (!is.null(res$compare_dimension_differences_list)) {
        box(width = 12, title = "Dimension Differences", status = "info", solidHeader = TRUE,
            uiOutput("dimension_differences_ui"))
      }
      ,
      
      # Combined summary histogram
      if (!is.null(res$combined_summary_histogram) & !PRELOAD_DATA) {
        browser()
        box(width = 12, title = "Combined Summary Histogram", status = "info", solidHeader = TRUE,
            plotOutput("comp_combined_summary_plot", height = 400))
      }
    )
  })
  
  
  # --- Summary differences table ---
  output$summary_differences_table <- renderDT({
    res <- analysis()
    if (is.null(res$summary_of_differences)) return(NULL)
    
    datatable(
      res$summary_of_differences,
      options = list(
        pageLength = 10, 
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      caption = "Summary of the difference between the two datasets",
      rownames = FALSE
    ) %>%
      formatStyle(
        'Difference (in %)',
        backgroundColor = styleInterval(
          0,
          c('#ffcccc', '#ccffcc')   # rouge < 0, vert > 0
        )
      ) 
  })
  
  # --- Combined summary plot for comparison ---
  output$comp_combined_summary_plot <- renderPlot({
    res <- analysis()
    if (is.null(res$combined_summary_histogram) | PRELOAD_DATA) return(NULL)
    
    # Sauvegarder et restaurer le titre si nécessaire
    original_title <- res$combined_summary_histogram$labels$title
    if (!is.null(original_title) && nchar(original_title) > 0) {
      res$combined_summary_histogram$labels$title <- ""
      print(res$combined_summary_histogram)
      # Restaurer le titre pour un usage futur si nécessaire
      res$combined_summary_histogram$labels$title <- original_title
    } else {
      print(res$combined_summary_histogram)
    }
  })
  
  # --- Strata differences UI ---
  output$strata_differences_ui <- renderUI({
    res <- analysis()
    if (is.null(res$compare_strata_differences_list)) return(NULL)
    
    strata_list <- res$compare_strata_differences_list
    disap <- strata_list$disapandap
    
    tagList(
      tags$h3(
        "Strata differences between initial and final datasets",
        style = "font-weight: bold; margin-bottom: 20px;"
      ),
      
      fluidRow(
        column(
          width = 6,
          tags$h4("Shared strata (initial vs final)", style = "font-weight: bold;"),
          if (!is.null(strata_list$number_init_column_final_column)) {
            DTOutput("strata_numbers_table")
          }
        ),
        
        column(
          width = 6,
          tags$h4("Strata that disappeared or appeared", style = "font-weight: bold;"),
          
          # Message
          if (!is.null(strata_list$strates_perdues_first_10) &&
              nrow(strata_list$strates_perdues_first_10) != 0) {
            tags$p(
              style = "font-weight: bold; color: grey;",
              paste0(
                "Completely lost or appearing strata. The base dataset is ", input$title1, " and the one it is compared to is ", input$title2)
              
            )
          } else {
            tags$p(style = "font-weight: bold; color: green;", "No stratum is gained nor lost")
          },
          
          # Onglets par Dimension
          if (!is.null(disap) && nrow(disap) > 0 && "Dimension" %in% names(disap)) {
            dims <- sort(unique(disap$Dimension))
            
            do.call(
              tabsetPanel,
              c(
                list(type = "tabs"),
                lapply(dims, function(d) {
                  tabPanel(
                    title = d,
                    div(style = "overflow-x:auto;", DTOutput(paste0("disapandap_", d)))
                  )
                })
              )
            )
          }
        )
      )
    )
  })
  
  
  
  # output$strata_disapandap_table <- renderDT({
  #   res <- analysis()
  #   if (is.null(res$compare_strata_differences_list$disapandap)) return(NULL)
  #   
  #   datatable(
  #     res$compare_strata_differences_list$disapandap,
  #     options = list(pageLength = 10, scrollX = TRUE),
  #     caption = "Strata that disappeared or appeared",
  #     rownames = FALSE
  #   )
  # })
  
  observe({
    res <- analysis()
    if (is.null(res$compare_strata_differences_list)) return()
    
    disap <- res$compare_strata_differences_list$disapandap
    if (is.null(disap) || nrow(disap) == 0 || !"Dimension" %in% names(disap)) return()
    
    dims <- unique(disap$Dimension)
    
    lapply(dims, function(d) {
      output[[paste0("disapandap_", d)]] <- DT::renderDT({
        DT::datatable(
          disap[disap$Dimension == d, , drop = FALSE] %>% dplyr::select(-Dimension),
          options = list(
            pageLength = 10,
            scrollX = TRUE
          ),
          rownames = FALSE
        )%>%
          formatStyle(
            'Difference in millions',
            backgroundColor = styleInterval(0, c('#ffcccc', '#ccffcc'))
          )
      })
    })
  })
  
  output$strata_numbers_table <- renderDT({
    res <- analysis()
    if (is.null(res$compare_strata_differences_list$number_init_column_final_column)) return(NULL)
    
    datatable(
      res$compare_strata_differences_list$number_init_column_final_column,
      options = list(pageLength = 10, scrollX = TRUE),
      caption = "Comparison of number of stratas between the two datasets",
      rownames = FALSE
    ) %>%
      formatStyle(
        "Difference",
        backgroundColor = styleInterval(
          c(-1e-12, 1e-12),                 # close to 0
          c("#ffcccc", "transparent", "#ccffcc")
        )
      )
  })
  
  # --- Temporal differences UI ---
  output$temporal_differences_ui <- renderUI({
    res <- analysis()
    if (is.null(res$plot_titles_list) || is.null(res$plot_titles_list$plots)) return(NULL)
    
    plots <- res$plot_titles_list$plots
    titles <- res$plot_titles_list$titles
    
    if (length(plots) == 0) return(NULL)
    
    # Créer un onglet pour chaque plot temporel
    tabs <- lapply(seq_along(plots), function(i) {
      tabPanel(
        title = if (i <= length(titles)) titles[[i]] else paste("Time Plot", i),
        plotOutput(paste0("temporal_plot_", i), height = 400)
      )
    })
    
    tagList(
      tags$p(style = "font-weight: bold; color: grey ;", "Representing the differences in percent for each year."),
      do.call(tabsetPanel, c(tabs, list(id = "temporal_plots_tabset")))
    )
  })
  
  # Render temporal plots
  observe({
    res <- analysis()
    if (is.null(res$plot_titles_list) || is.null(res$plot_titles_list$plots)) return()
    
    plots <- res$plot_titles_list$plots
    for (i in seq_along(plots)) {
      local({
        ii <- i
        output[[paste0("temporal_plot_", ii)]] <- renderPlot({
          if (ii <= length(plots)) {
            print(plots[[ii]])
          }
        })
      })
    }
  })
  
  # --- Geographic differences ---
  output$geographic_diff_plot <- renderPlot({
    res <- analysis()
    if (is.null(res$Geographicdiff) || is.null(res$Geographicdiff$plott)) return(NULL)
    print(res$Geographicdiff$plott)
  })
  
  output$geographic_diff_title <- renderUI({
    res <- analysis()
    if (is.null(res$Geographicdiff) || is.null(res$Geographicdiff$title)) return(NULL)
    tags$h4(style = "text-align: center; margin-top: 10px;", res$Geographicdiff$title)
  })
  
  # --- Dimension differences UI ---
  # output$dimension_differences_ui <- renderUI({ ancien
  #   res <- analysis()
  #   if (is.null(res$compare_dimension_differences_list)) return(NULL)
  #   
  #   dim_list <- res$compare_dimension_differences_list
  #   
  #   tagList(
  #     if (!is.null(dim_list$Groupped_all_not_disap_or_app_to_dysplay) && 
  #         nrow(dim_list$Groupped_all_not_disap_or_app_to_dysplay) != 0) {
  #       DTOutput("dimension_differences_table")
  #     } else {
  #       tags$p(style = "font-weight: bold; color: green;", 
  #              "There are no differences between stratas aside the appearing and disappearing ones")
  #     }
  #   )
  # })
  
  output$dimension_differences_ui <- renderUI({
    res <- analysis()
    df <- res$compare_dimension_differences_list$Groupped_all_not_disap_or_app_to_dysplay
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    if (!"Dimension" %in% names(df)) {
      return(tagList(
        tags$p(style="font-weight:bold;color:#ffcc00;",
               "Column 'Dimension' not found in dimension differences table."),
        DTOutput("dimension_differences_table_fallback")
      ))
    }
    
    dims <- sort(unique(df$Dimension))
    
    panels <- lapply(dims, function(d) {
      tabPanel(
        title = d,
        div(style = "overflow-x:auto;", DTOutput(paste0("dimdiff_", d)))
      )
    })
    
    tagList(
      tags$h3("Dimension differences between datasets",
              style="font-weight:bold; margin-bottom: 10px;"),
      do.call(tabsetPanel, c(list(type = "tabs"), panels))
    )
  })
  
  observe({
    res <- analysis()
    df <- res$compare_dimension_differences_list$Groupped_all_not_disap_or_app_to_dysplay
    if (is.null(df) || nrow(df) == 0 || !"Dimension" %in% names(df)) return()
    
    dims <- unique(df$Dimension)
    
    lapply(dims, function(d) {
      output[[paste0("dimdiff_", d)]] <- DT::renderDT({
        sub <- df[df$Dimension == d, , drop = FALSE] %>%dplyr::ungroup() %>% 
          dplyr::select(-Dimension) %>%
          rename(
            !!paste0("Value ", input$title1) := `Values dataset 1`,
            !!paste0("Value ", input$title2) := `Values dataset 2`
          )
        
        
        dt <- DT::datatable(
          sub,
          options = list(pageLength = 10, scrollX = TRUE),
          caption = paste0("Dimension differences — ", d),
          rownames = FALSE
        )
        
        # Coloriser seulement si la colonne existe
        col_to_color <- "Difference (in %)"
        if (col_to_color %in% names(sub)) {
          dt <- dt %>% DT::formatStyle(
            col_to_color,
            backgroundColor = DT::styleInterval(0, c("#ffcccc", "#ccffcc"))
          )
        }
        
        dt
      })
    })
  })
  
  output$dimension_differences_table <- renderDT({
    res <- analysis()
    if (is.null(res$compare_dimension_differences_list$Groupped_all_not_disap_or_app_to_dysplay)) return(NULL)
    
    datatable(
      res$compare_dimension_differences_list$Groupped_all_not_disap_or_app_to_dysplay,
      options = list(pageLength = 10, scrollX = TRUE),
      caption = "Dimension differences between datasets",
      rownames = FALSE
    ) %>%
      formatStyle(
        'Difference (in %)',
        backgroundColor = styleInterval(0, c('#ffcccc', '#ccffcc'))
      )
  })
  
  # ---- Onglets dynamiques : TIME ----
  output$time_cov_tabs <- renderUI({
    res <- analysis()
    if (is.null(res) || is.null(res$time_coverage_analysis_list) ||
        is.null(res$time_coverage_analysis_list$plots)) {
      return(tags$em("No time plots"))
    }
    plots <- res$time_coverage_analysis_list$plots
    n     <- length(plots)
    labs  <- res$time_coverage_analysis_list$titles
    if (is.null(labs) || length(labs) != n) labs <- paste("Time plot", seq_len(n))
    
    tabs <- lapply(seq_len(n), function(i) {
      tabPanel(
        labs[[i]],
        withSpinner(plotOutput(paste0("time_plot_", i), height = 420), hide.ui = FALSE)
      )
    })
    do.call(tabsetPanel, c(tabs, list(id = "time_cov_tabset", type = "pills")))
  })
  
  observeEvent(analysis(), {
    res <- analysis()
    if (is.null(res) || is.null(res$time_coverage_analysis_list) ||
        is.null(res$time_coverage_analysis_list$plots)) return()
    
    plots <- res$time_coverage_analysis_list$plots
    for (i in seq_along(plots)) {
      local({
        ii <- i
        output[[paste0("time_plot_", ii)]] <- renderPlot({
          p <- plots[[ii]]
          p <- if (inherits(p, "gg") || inherits(p, "ggplot")) p else render_any_plot(p)
          print(p)
        }, res = 96, execOnResize = FALSE)
        outputOptions(output, paste0("time_plot_", ii), suspendWhenHidden = FALSE)
      })
    }
  })
  
  # --- Spatial coverage ---
  output$spatial_cov_plot <- renderPlot({
    res <- analysis()
    if (is.null(res) || is.null(res$spatial_coverage_analysis_list)) return(NULL)
    res$spatial_coverage_analysis_list$plots
  })
  
  # --- Combined summary histogram ---
  output$combined_summary_plot <- renderPlot({
    res <- analysis()
    if (is.null(res) || is.null(res$combined_summary_histogram)) return(NULL)
    render_any_plot(res$combined_summary_histogram)
  })
  
  # session end
  # outputOptions(output, "spatial_cov_plot", suspendWhenHidden = FALSE)
  # outputOptions(output, "combined_summary_plot", suspendWhenHidden = FALSE)
  # 
}
shinyApp(ui, server)
