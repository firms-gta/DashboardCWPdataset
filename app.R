source("global.R", local = TRUE)

# ---- UI ----
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Unique Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Run analysis", tabName = "run", icon = icon("play")),
      menuItem("Coverage", tabName = "coverage", icon = icon("sliders")),
      menuItem("Spatial coverage", tabName = "spatial", icon = icon("globe")),
      menuItem("Combined summary", tabName = "summary", icon = icon("bar-chart"))
    ),
    hr(),
    fileInput("file_init", "Upload dataset (CSV/RDS/RData)", accept = c(".csv", ".rds", ".RDS", ".rdata", ".RData")),
    checkboxInput("has_header", "CSV has header", TRUE),
    textInput("sep", "CSV separator", ","),
    tags$small("Required columns: measurement_value, time_start (Date/character), geographic_identifier (optional), gridtype (optional)."),
    hr(),
    h4("Parameters"),
    textInput("title1", "Dataset 1 title", value = "Dataset 1"),
    textInput("title2", "Dataset 2 title (ignored in unique mode)", value = "NONE"),
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
    actionButton("run_btn", "Run unique analysis", icon = icon("rocket"), class = "btn-primary"),
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
          box(width = 12, title = "Preview (sortable / filterable)", status = "primary", solidHeader = TRUE,
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
      tabItem(
        tabName = "coverage",
        fluidRow(
          box(width = 12, title = "Time coverage (plots)", status = "info", solidHeader = TRUE,
              # onglets dynamiques des plots temporels
              uiOutput("time_cov_tabs")
          )
        ),
        fluidRow(
          box(width = 12, title = "Other dimensions (plots)", status = "info", solidHeader = TRUE,
              # onglets dynamiques des plots "other dimensions"
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
    log_txt(paste0(old, if (nzchar(old)) "
" else "", msg))
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
    ), collapse = "
")
  })
  
  # --- Load dataset ---
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
  
  observeEvent(input$file_init, {
    info <- try(file.info(input$file_init$datapath), silent = TRUE)
    append_log("File uploaded:", input$file_init$name, "| ext:", tools::file_ext(input$file_init$name), "| size:", if (!inherits(info, "try-error")) format(info$size, big.mark = " ") else "?")
  })
  
  output$table_preview <- renderDT({
    req(input$file_init)
    datatable(load_dataset(), options = list(pageLength = 10, scrollX = TRUE), filter = "top")
  })
  
  # --- Populate dynamic filter choices ---
  observe({
    df <- try(load_dataset(), silent = TRUE)
    if (inherits(df, "data.frame")) {
      output$vb_rows <- renderValueBox({ valueBox(format(nrow(df), big.mark = " "), "Rows", icon = icon("hashtag"), color = "purple") })
      output$vb_cols <- renderValueBox({ valueBox(format(ncol(df), big.mark = " "), "Columns", icon = icon("table"), color = "purple") })
      mem <- format(object.size(df), units = "auto")
      output$vb_mem <- renderValueBox({ valueBox(mem, "Approx. size", icon = icon("database"), color = "purple") })
      if ("species" %in% names(df)) updateSelectizeInput(session, "species", choices = sort(unique(df$species)), server = TRUE)
      if ("fishing_fleet" %in% names(df)) updateSelectizeInput(session, "fleet", choices = sort(unique(df$fishing_fleet)), server = TRUE)
    }
  })
  
  # --- Button click tracer ---
  observeEvent(input$run_btn, {
    append_log("Button clicked. Params:",
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
  
  # --- Run analysis with unique_analyse = TRUE ---
  analysis <- eventReactive(input$run_btn, {
    if (is.null(input$file_init)) {
      showNotification("Upload a dataset first.", type = "error")
      append_log("Run aborted: no dataset uploaded.")
      return(NULL)
    }
    
    df <- try(load_dataset(), silent = TRUE)
    if (!inherits(df, "data.frame")) {
      showNotification("Could not read the dataset (CSV/RDS/RData).", type = "error")
      append_log("Run aborted: dataset couldn't be read.")
      return(NULL)
    }
    
    if (!"measurement_value" %in% names(df)) {
      showNotification("Missing required column: 'measurement_value'.", type = "error", duration = 8)
      append_log("Run aborted: missing 'measurement_value'.")
      return(NULL)
    }
    
    if (isTRUE(input$debug_small) && nrow(df) > 5000) {
      append_log("DEBUG: subsetting to first 5000 rows (debug_small)")
      df <- utils::head(df, 5000)
    }
    
    filt <- list(
      species = if (length(input$species) > 0) input$species else NULL,
      fishing_fleet = if (length(input$fleet) > 0) input$fleet else NULL
    )
    
    append_log("Launching analysis (unique mode)... rows=", nrow(df), "cols=", ncol(df))
    t0 <- Sys.time()
    
    withProgress(message = "Running unique analysis...", value = 0.1, {
      res <- NULL
      cap <- NULL
      tmp <- tryCatch(
        {
          # pm <- if (isTRUE(input$print_map)) {
          #   showNotification("print_map=TRUE mais aucun shapefile fourni → désactivation pour éviter l'erreur 'Please provide a shape for the polygons'.", type = "warning", duration = 6)
          #   append_log("No shapefile provided: forcing print_map=FALSE")
          #   FALSE
          # } else FALSE
          r <- CWP.dataset::comprehensive_cwp_dataframe_analysis(
            parameter_init = df,
            parameter_final = df,
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
            unique_analyse = TRUE,
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
        append_log(paste0("Console output (first 10 lines):
", paste(utils::head(cap, 10), collapse = "
")))
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
  
  # --- Coverage tab outputs ---
  output$time_cov_plot <- renderPlot({
    res <- analysis()
    if (is.null(res) || is.null(res$time_coverage_analysis_list)) return(NULL)
    idx <- if (is.null(input$time_plot_index)) 1 else input$time_plot_index
    p <- extract_indexed_plot(res$time_coverage_analysis_list, idx)
    # render_any_plot(p)
    p
  })
  
  output$other_cov_plot <- renderPlot({
    res <- analysis()
    if (is.null(res) || is.null(res$other_dimension_analysis_list)) return(NULL)
    idx <- if (is.null(input$other_fig_index)) 1 else input$other_fig_index
    p <- extract_indexed_plot(res$other_dimension_analysis_list, idx)
    # render_any_plot(p)
    p
  })
  
  # --- Spatial coverage ---
  output$spatial_cov_plot <- renderPlot({
    res <- analysis()
    if (is.null(res) || is.null(res$spatial_coverage_analysis_list)) return(NULL)
    # render_any_plot(res$spatial_coverage_analysis_list)
    res$spatial_coverage_analysis_list$plots
  })
  
  # --- Combined summary histogram ---
  output$combined_summary_plot <- renderPlot({
    res <- analysis()
    if (is.null(res) || is.null(res$combined_summary_histogram)) return(NULL)
    render_any_plot(res$combined_summary_histogram)
  })
  
  # session end
  # Keep outputs active when switching tabs (no need to re-run)
  outputOptions(output, "time_cov_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "other_cov_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "spatial_cov_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "combined_summary_plot", suspendWhenHidden = FALSE)
  
  session$onSessionEnded(function() {
    append_log("Session ended.")
  })
}

shinyApp(ui, server)
