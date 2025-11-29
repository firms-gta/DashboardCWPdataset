source("global.R", local = TRUE)

# ---- UI ----
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Run analysis", tabName = "run", icon = icon("play")),
      menuItem("Comparison", tabName = "comparison", icon = icon("exchange-alt")),
      menuItem("Coverage", tabName = "coverage", icon = icon("sliders")),
      menuItem("Spatial coverage", tabName = "spatial", icon = icon("globe")),
      menuItem("Combined summary", tabName = "summary", icon = icon("bar-chart"))
    ),
    hr(),
    
    # --- Dataset 1 ---
    h4("Dataset 1"),
    fileInput("file_init", "Upload dataset 1 (CSV/RDS/RData)", accept = c(".csv", ".rds", ".RDS", ".rdata", ".RData")),
    checkboxInput("has_header", "CSV has header", TRUE),
    textInput("sep", "CSV separator", ","),
    tags$small("Required columns: measurement_value, time_start (Date/character), geographic_identifier (optional), gridtype (optional)."),
    
    # --- Dataset 2 (pour comparaison) ---
    h4("Dataset 2 (for comparison)"),
    fileInput("file_final", "Upload dataset 2 (CSV/RDS/RData)", accept = c(".csv", ".rds", ".RDS", ".rdata", ".RData")),
    checkboxInput("has_header2", "CSV has header", TRUE),
    textInput("sep2", "CSV separator", ","),
    
    hr(),
    h4("Analysis Mode"),
    radioButtons("analysis_mode", "Mode:",
                 choices = c("Unique analysis" = "unique", "Comparison" = "comparison"),
                 selected = "unique"),
    
    h4("Parameters"),
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
    selectizeInput("species", "Species", choices = NULL, multiple = TRUE, options = list(placeholder = "Select species...")),
    selectizeInput("fleet", "Fishing fleet", choices = NULL, multiple = TRUE, options = list(placeholder = "Select fleets...")),
    
    hr(),
    actionButton("run_btn", "Run analysis", icon = icon("rocket"), class = "btn-primary"),
    actionButton("clear_logs", "Clear logs", icon = icon("eraser"), class = "btn"),
    downloadButton("download_logs", "Download logs")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(".content-wrapper { background: #0b1220; color: #e5e7eb;} .box { border-radius: 1rem; } .box-header { border-bottom: 1px solid #111827;} .small-box {border-radius: 1rem;}"))),
    tabItems(
      tabItem(
        tabName = "run",
        fluidRow(
          valueBoxOutput("vb_rows"),
          valueBoxOutput("vb_cols"),
          valueBoxOutput("vb_mem")
        ),
        fluidRow(
          box(width = 12, title = "Preview Dataset 1 (sortable / filterable)", status = "primary", solidHeader = TRUE,
              withSpinner(DTOutput("table_preview")))
        ),
        fluidRow(
          box(width = 12, title = "Status / logs", status = "primary", solidHeader = TRUE,
              verbatimTextOutput("status_log"))
        ),
        fluidRow(
          box(width = 12, title = "Session / Client info", status = "primary", solidHeader = TRUE,
              verbatimTextOutput("client_info"))
        )
      ),
      
      # --- Nouvel onglet Comparaison ---
      tabItem(
        tabName = "comparison",
        fluidRow(
          valueBoxOutput("vb_rows2"),
          valueBoxOutput("vb_cols2"),
          valueBoxOutput("vb_mem2")
        ),
        fluidRow(
          box(width = 12, title = "Preview Dataset 2 (sortable / filterable)", status = "warning", solidHeader = TRUE,
              withSpinner(DTOutput("table_preview2")))
        ),
        fluidRow(
          box(width = 12, title = "Comparison Results", status = "info", solidHeader = TRUE,
              withSpinner(uiOutput("comparison_results")))
        )
      ),
      
      tabItem(
        tabName = "coverage",
        fluidRow(
          box(width = 12, title = "Time coverage (plots)", status = "info", solidHeader = TRUE,
              uiOutput("time_cov_tabs")
          )
        ),
        fluidRow(
          box(width = 12, title = "Other dimensions (plots)", status = "info", solidHeader = TRUE,
              uiOutput("other_cov_tabs")
          )
        )
      ),
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
  
  # simple log helper
  log_txt <- reactiveVal("")
  append_log <- function(...) {
    msg <- paste(format(Sys.time(), "%H:%M:%S"), "-", paste(..., collapse = " "))
    old <- log_txt()
    log_txt(paste0(old, if (nzchar(old)) "\n" else "", msg))
  }
  output$status_log <- renderText(log_txt())
  
  observeEvent(input$clear_logs, { log_txt("") })
  output$download_logs <- downloadHandler(
    filename = function() sprintf("shiny_logs_%s.txt", format(Sys.time(), "%Y%m%d-%H%M%S")),
    content = function(file) writeLines(log_txt(), file)
  )
  
  # warn if package not present 
  observe({
    if (!requireNamespace("CWP.dataset", quietly = TRUE)) {
      append_log("Package 'CWP.dataset' not installed. Functions with 'CWP.dataset::' may fail.")
      showNotification("Package 'CWP.dataset' not installed.", type = "error", duration = 8)
    }
  })
  
  # --- Client info / session diag ---
  output$client_info <- renderText({
    cd <- session$clientData
    paste(c(
      paste("url_hostname:", cd$url_hostname),
      paste("pixelratio:", cd$pixelratio),
      paste("browser version:", cd$browser$name, cd$browser$version),
      paste("screen:", cd$screen$width, "x", cd$screen$height),
      paste("output plot dims (summary):", paste(names(cd)[grepl("output_.*_width|output_.*_height", names(cd))][1:4], collapse=", "))
    ), collapse = "\n")
  })
  
  # --- Load dataset 1 ---
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
  
  # --- Load dataset 2 ---
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
  
  observeEvent(input$file_init, {
    info <- try(file.info(input$file_init$datapath), silent = TRUE)
    append_log("File 1 uploaded:", input$file_init$name, "| ext:", tools::file_ext(input$file_init$name), "| size:", if (!inherits(info, "try-error")) format(info$size, big.mark = " ") else "?")
  })
  
  observeEvent(input$file_final, {
    info <- try(file.info(input$file_final$datapath), silent = TRUE)
    append_log("File 2 uploaded:", input$file_final$name, "| ext:", tools::file_ext(input$file_final$name), "| size:", if (!inherits(info, "try-error")) format(info$size, big.mark = " ") else "?")
  })
  
  output$table_preview <- renderDT({
    req(input$file_init)
    datatable(load_dataset(), options = list(pageLength = 10, scrollX = TRUE), filter = "top")
  })
  
  output$table_preview2 <- renderDT({
    req(input$file_final)
    datatable(load_dataset2(), options = list(pageLength = 10, scrollX = TRUE), filter = "top")
  })
  
  # --- Populate dynamic filter choices ---
  observe({
    df <- try(load_dataset(), silent = TRUE)
    if (inherits(df, "data.frame")) {
      output$vb_rows <- renderValueBox({ valueBox(format(nrow(df), big.mark = " "), "Rows Dataset 1", icon = icon("hashtag"), color = "purple") })
      output$vb_cols <- renderValueBox({ valueBox(format(ncol(df), big.mark = " "), "Columns Dataset 1", icon = icon("table"), color = "purple") })
      mem <- format(object.size(df), units = "auto")
      output$vb_mem <- renderValueBox({ valueBox(mem, "Approx. size Dataset 1", icon = icon("database"), color = "purple") })
      if ("species" %in% names(df)) updateSelectizeInput(session, "species", choices = sort(unique(df$species)), server = TRUE)
      if ("fishing_fleet" %in% names(df)) updateSelectizeInput(session, "fleet", choices = sort(unique(df$fishing_fleet)), server = TRUE)
    }
  })
  
  observe({
    df2 <- try(load_dataset2(), silent = TRUE)
    if (inherits(df2, "data.frame")) {
      output$vb_rows2 <- renderValueBox({ valueBox(format(nrow(df2), big.mark = " "), "Rows Dataset 2", icon = icon("hashtag"), color = "orange") })
      output$vb_cols2 <- renderValueBox({ valueBox(format(ncol(df2), big.mark = " "), "Columns Dataset 2", icon = icon("table"), color = "orange") })
      mem <- format(object.size(df2), units = "auto")
      output$vb_mem2 <- renderValueBox({ valueBox(mem, "Approx. size Dataset 2", icon = icon("database"), color = "orange") })
    }
  })
  
  # --- Button click tracer ---
  observeEvent(input$run_btn, {
    mode <- input$analysis_mode
    append_log("Button clicked. Mode:", mode,
               paste0("time_cols=", paste(input$time_cols, collapse=",")),
               paste0("geo_dim=", input$geo_dim),
               paste0("geo_group=", input$geo_group),
               paste0("fact=", input$fact),
               paste0("coverage=", input$coverage),
               paste0("print_map=", input$print_map),
               paste0("plotting_type=", input$plotting_type),
               paste0("species_n=", length(input$species)),
               paste0("fleet_n=", length(input$fleet)),
               paste0("debug_small=", input$debug_small))
  }, ignoreInit = TRUE)
  
  # --- Run analysis avec mode unique ou comparaison ---
  analysis <- eventReactive(input$run_btn, {
    mode <- input$analysis_mode
    
    if (mode == "unique") {
      # Mode unique - seulement dataset 1
      if (is.null(input$file_init)) {
        showNotification("Upload dataset 1 first.", type = "error")
        append_log("Run aborted: no dataset 1 uploaded.")
        return(NULL)
      }
      
      df <- try(load_dataset(), silent = TRUE)
      if (!inherits(df, "data.frame")) {
        showNotification("Could not read dataset 1 (CSV/RDS/RData).", type = "error")
        append_log("Run aborted: dataset 1 couldn't be read.")
        return(NULL)
      }
      
      parameter_final <- df  # Même dataset pour unique_analyse = TRUE
      
    } else {
      # Mode comparaison - besoin des deux datasets
      if (is.null(input$file_init) || is.null(input$file_final)) {
        showNotification("Upload both datasets for comparison.", type = "error")
        append_log("Run aborted: need both datasets for comparison.")
        return(NULL)
      }
      
      df <- try(load_dataset(), silent = TRUE)
      df2 <- try(load_dataset2(), silent = TRUE)
      
      if (!inherits(df, "data.frame") || !inherits(df2, "data.frame")) {
        showNotification("Could not read one or both datasets.", type = "error")
        append_log("Run aborted: one or both datasets couldn't be read.")
        return(NULL)
      }
      
      parameter_final <- df2  # Dataset différent pour unique_analyse = FALSE
    }
    
    if (!"measurement_value" %in% names(df)) {
      showNotification("Missing required column: 'measurement_value' in dataset 1.", type = "error", duration = 8)
      append_log("Run aborted: missing 'measurement_value' in dataset 1.")
      return(NULL)
    }
    
    if (isTRUE(input$debug_small) && nrow(df) > 5000) {
      append_log("DEBUG: subsetting dataset 1 to first 5000 rows (debug_small)")
      df <- utils::head(df, 5000)
    }
    
    if (mode == "comparison" && isTRUE(input$debug_small) && nrow(parameter_final) > 5000) {
      append_log("DEBUG: subsetting dataset 2 to first 5000 rows (debug_small)")
      parameter_final <- utils::head(parameter_final, 5000)
    }
    
    filt <- list(
      species = if (length(input$species) > 0) input$species else NULL,
      fishing_fleet = if (length(input$fleet) > 0) input$fleet else NULL
    )
    
    append_log("Launching analysis (mode:", mode, ")... dataset1 rows=", nrow(df), "dataset2 rows=", nrow(parameter_final))
    t0 <- Sys.time()
    
    withProgress(message = if (mode == "unique") "Running unique analysis..." else "Running comparison...", value = 0.1, {
      res <- NULL
      cap <- NULL
      tmp <- tryCatch(
        {
          pm <- if (isTRUE(input$print_map)) {
            showNotification("print_map=TRUE mais aucun shapefile fourni → désactivation pour éviter l'erreur 'Please provide a shape for the polygons'.", type = "warning", duration = 6)
            append_log("No shapefile provided: forcing print_map=FALSE")
            FALSE
          } else FALSE
          
          r <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
            parameter_init = df,
            parameter_final = parameter_final,
            fig.path = getwd(),
            parameter_fact = input$fact,
            parameter_short = FALSE,
            parameter_columns_to_keep = c("Precision","measurement_unit","Values dataset 1","Values dataset 2","Loss / Gain","Difference (in %)","Dimension","Difference in value"),
            parameter_diff_value_or_percent = "Difference (in %)",
            parameter_filtering = filt,
            parameter_time_dimension = input$time_cols,
            parameter_geographical_dimension = input$geo_dim,
            parameter_geographical_dimension_groupping = input$geo_group,
            parameter_colnames_to_keep = "all",
            outputonly = FALSE,
            plotting_type = input$plotting_type,
            print_map = pm,
            shapefile_fix = shapefile.fix,
            continent = continent,
            coverage = isTRUE(input$coverage),
            parameter_resolution_filter = NULL,
            parameter_titre_dataset_1 = input$title1,
            parameter_titre_dataset_2 = input$title2,
            unique_analyse = (mode == "unique"),  # TRUE pour unique, FALSE pour comparaison
            removemap = isTRUE(input$removemap),
            topnumber = 6
          )
          list(res = r)
        },
        error = function(e) {
          showModal(modalDialog(title = "Analysis error", easyClose = TRUE,
                                tagList(tags$pre(conditionMessage(e)))))
          append_log(paste("ERROR:", conditionMessage(e)))
          NULL
        }
      )
      
      res <- if (is.null(tmp)) NULL else tmp$res
      cap <- if (!is.null(res)) utils::capture.output(utils::str(res, max.level = 1)) else character(0)
      
      if (!is.null(cap) && length(cap) > 0) {
        append_log(paste0("Console output (first 10 lines):\n", paste(utils::head(cap, 10), collapse = "\n")))
        if (length(cap) > 10) append_log(paste("... (", length(cap)-10, "more lines)"))
      }
      
      incProgress(0.8)
      t1 <- Sys.time()
      append_log("Analysis finished in", round(as.numeric(difftime(t1, t0, units = "secs")), 2), "sec")
      
      if (!is.list(res)) {
        append_log("No result returned (NULL or non-list).")
        return(NULL)
      }
      
      # quick result stats
      keys <- c("time_coverage_analysis_list","other_dimension_analysis_list","spatial_coverage_analysis_list","combined_summary_histogram")
      for (k in keys) append_log("Result has", k, ":", if (!is.null(res[[k]])) "YES" else "NO")
      
      res
    })
  }, ignoreInit = TRUE)
  
  # --- Output pour l'onglet Comparaison ---
  output$comparison_results <- renderUI({
    res <- analysis()
    mode <- input$analysis_mode
    
    if (is.null(res) || mode == "unique") {
      return(tags$em("No comparison results available. Run analysis in comparison mode first."))
    }
    
    # Vérifier que les objets de comparaison existent
    has_comparison_data <- !is.null(res$summary_of_differences) || 
      !is.null(res$compare_strata_differences_list) ||
      !is.null(res$compare_dimension_differences_list) ||
      !is.null(res$Geographicdiff)
    
    if (!has_comparison_data) {
      return(tags$em("No specific comparison data found in results."))
    }
    
    tagList(
      # Summary of differences
      if (!is.null(res$summary_of_differences)) {
        box(width = 12, title = "Summary of Differences", status = "primary", solidHeader = TRUE,
            DTOutput("summary_differences_table"))
      },
      
      # Combined summary histogram
      if (!is.null(res$combined_summary_histogram)) {
        box(width = 12, title = "Combined Summary Histogram", status = "info", solidHeader = TRUE,
            plotOutput("comp_combined_summary_plot", height = 400))
      },
      
      # Strata differences
      if (!is.null(res$compare_strata_differences_list)) {
        box(width = 12, title = "Strata Differences Analysis", status = "warning", solidHeader = TRUE,
            uiOutput("strata_differences_ui"))
      },
      
      # Temporal differences
      if (!is.null(res$plot_titles_list) && !is.null(res$plot_titles_list$plots)) {
        box(width = 12, title = "Temporal Differences", status = "success", solidHeader = TRUE,
            uiOutput("temporal_differences_ui"))
      },
      
      # Geographic differences
      if (!is.null(res$Geographicdiff)) {
        box(width = 12, title = "Geographic Differences", status = "danger", solidHeader = TRUE,
            plotOutput("geographic_diff_plot", height = 500),
            htmlOutput("geographic_diff_title"))
      },
      
      # Dimension differences
      if (!is.null(res$compare_dimension_differences_list)) {
        box(width = 12, title = "Dimension Differences", status = "info", solidHeader = TRUE,
            uiOutput("dimension_differences_ui"))
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
        'Difference',
        backgroundColor = styleInterval(0, c('#ffcccc', '#ccffcc'))
      )
  })
  
  # --- Combined summary plot for comparison ---
  output$comp_combined_summary_plot <- renderPlot({
    res <- analysis()
    if (is.null(res$combined_summary_histogram)) return(NULL)
    
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
    
    tagList(
      # Message sur les strates perdues/trouvées
      if (!is.null(strata_list$strates_perdues_first_10) && nrow(strata_list$strates_perdues_first_10) != 0) {
        tags$p(style = "font-weight: bold;",
               "The strata differences (completely lost or appearing) between the first one and the second one (representing ",
               round(strata_list$pourcentage_strates_perdues), "% of the total number of strata) are:")
      } else {
        tags$p(style = "font-weight: bold; color: green;", "No stratum is gained nor lost")
      },
      
      # Table disapandap si disponible
      if (!is.null(strata_list$disapandap) && nrow(strata_list$disapandap) > 0) {
        DTOutput("strata_disapandap_table")
      },
      
      # Table number_init_column_final_column
      if (!is.null(strata_list$number_init_column_final_column)) {
        DTOutput("strata_numbers_table")
      }
    )
  })
  
  output$strata_disapandap_table <- renderDT({
    res <- analysis()
    if (is.null(res$compare_strata_differences_list$disapandap)) return(NULL)
    
    datatable(
      res$compare_strata_differences_list$disapandap,
      options = list(pageLength = 10, scrollX = TRUE),
      caption = "Strata that disappeared or appeared",
      rownames = FALSE
    )
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
        'Difference',
        backgroundColor = styleInterval(0, c('#ffcccc', '#ccffcc'))
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
      tags$p(style = "font-weight: bold;", "Representing the differences in percent for each year."),
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
  output$dimension_differences_ui <- renderUI({
    res <- analysis()
    if (is.null(res$compare_dimension_differences_list)) return(NULL)
    
    dim_list <- res$compare_dimension_differences_list
    
    tagList(
      if (!is.null(dim_list$Groupped_all_not_disap_or_app_to_dysplay) && 
          nrow(dim_list$Groupped_all_not_disap_or_app_to_dysplay) != 0) {
        DTOutput("dimension_differences_table")
      } else {
        tags$p(style = "font-weight: bold; color: green;", 
               "There are no differences between stratas aside the appearing and disappearing ones")
      }
    )
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
  
  # [Le reste du code pour les onglets existants reste inchangé...]
  # ... (time_cov_tabs, other_cov_tabs, spatial_cov_plot, combined_summary_plot, etc.)
  
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
  
  # ---- Onglets dynamiques : OTHER DIMENSIONS ----
  output$other_cov_tabs <- renderUI({
    res <- analysis()
    if (is.null(res) || is.null(res$other_dimension_analysis_list) ||
        is.null(res$other_dimension_analysis_list$figures)) {
      return(tags$em("No dimension plots"))
    }
    figs <- res$other_dimension_analysis_list$figures
    n    <- length(figs)
    labs <- res$other_dimension_analysis_list$dimension_title_subfigures
    if (is.null(labs) || length(labs) != n) labs <- paste("Figure", seq_len(n))
    
    tabs <- lapply(seq_len(n), function(i) {
      tabPanel(
        labs[[i]],
        withSpinner(plotOutput(paste0("other_plot_", i), height = 420), hide.ui = FALSE)
      )
    })
    do.call(tabsetPanel, c(tabs, list(id = "other_cov_tabset", type = "pills")))
  })
  
  observeEvent(analysis(), {
    res <- analysis()
    if (is.null(res) || is.null(res$other_dimension_analysis_list) ||
        is.null(res$other_dimension_analysis_list$figures)) return()
    
    figs <- res$other_dimension_analysis_list$figures
    for (i in seq_along(figs)) {
      local({
        ii <- i
        output[[paste0("other_plot_", ii)]] <- renderPlot({
          p <- figs[[ii]]
          p <- if (inherits(p, "gg") || inherits(p, "ggplot")) p else render_any_plot(p)
          print(p)
        }, res = 96, execOnResize = FALSE)
        outputOptions(output, paste0("other_plot_", ii), suspendWhenHidden = FALSE)
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
  outputOptions(output, "spatial_cov_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "combined_summary_plot", suspendWhenHidden = FALSE)
  
  session$onSessionEnded(function() {
    append_log("Session ended.")
  })
}

shinyApp(ui, server)