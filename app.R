# BOINETC Shiny App
# Run locally with: shiny::runApp("path/to/boinetc_app")

required_pkgs <- c("shiny", "DT")
missing_required <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_required) > 0) {
  stop("Please install required packages first: ", paste(missing_required, collapse = ", "), call. = FALSE)
}

library(shiny)

load_boinetc <- function() {
  if (requireNamespace("BOINETC", quietly = TRUE)) {
    return(TRUE)
  }

  # Standalone-app convenience: try installing from a tarball placed next to app.R.
  app_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  local_tarballs <- c(
    file.path(app_dir, "BOINETC_0.1.0.tar.gz"),
    file.path(dirname(app_dir), "BOINETC_0.1.0.tar.gz")
  )
  local_tarballs <- local_tarballs[file.exists(local_tarballs)]

  if (length(local_tarballs) > 0) {
    ok <- tryCatch({
      utils::install.packages(local_tarballs[1], repos = NULL, type = "source", quiet = TRUE)
      requireNamespace("BOINETC", quietly = TRUE)
    }, error = function(e) FALSE)
    if (isTRUE(ok)) {
      return(TRUE)
    }
  }

  FALSE
}

boinetc_available <- load_boinetc()

parse_num_vector <- function(x, name, integer = FALSE, expected_length = NULL) {
  x <- gsub("[;[:space:]]+", ",", as.character(x))
  values <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  values <- values[nzchar(values)]
  nums <- suppressWarnings(as.numeric(values))
  if (length(nums) == 0 || anyNA(nums)) {
    stop(name, " must be a comma-separated numeric vector.", call. = FALSE)
  }
  if (!is.null(expected_length) && length(nums) != expected_length) {
    stop(name, " must contain exactly ", expected_length, " value(s).", call. = FALSE)
  }
  if (integer) {
    nums <- as.integer(nums)
  }
  nums
}

plot_probability_matrix <- function(mat, main = "Probability matrix", target = NULL) {
  nr <- nrow(mat)
  nc <- ncol(mat)
  z <- t(mat[nr:1, , drop = FALSE])

  op <- graphics::par(mar = c(4.2, 4.8, 3.2, 1.2))
  on.exit(graphics::par(op), add = TRUE)

  graphics::image(
    x = seq_len(nc), y = seq_len(nr), z = z,
    axes = FALSE, xlab = "Dose level B", ylab = "Dose level A", main = main
  )
  graphics::axis(1, at = seq_len(nc), labels = paste0("B", seq_len(nc)))
  graphics::axis(2, at = seq_len(nr), labels = paste0("A", rev(seq_len(nr))), las = 1)
  graphics::box()

  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      graphics::text(j, nr - i + 1, labels = formatC(mat[i, j], format = "f", digits = 2))
    }
  }

  if (!is.null(target) && length(target) > 0) {
    target <- as.integer(target)
    target <- target[target >= 1 & target <= nr * nc]
    if (length(target) > 0) {
      target_row <- ((target - 1) %% nr) + 1
      target_col <- ((target - 1) %/% nr) + 1
      target_y <- nr - target_row + 1
      graphics::points(target_col, target_y, pch = 0, cex = 2.2, lwd = 2)
    }
  }
}

as_display_df <- function(x) {
  if (is.null(x)) {
    return(data.frame())
  }
  out <- as.data.frame(x, check.names = FALSE)
  rownames(out) <- NULL
  out
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .app-title { margin-bottom: 6px; }
      .muted { color: #667085; }
      .status-ok { color: #027a48; font-weight: 700; }
      .status-bad { color: #b42318; font-weight: 700; }
      .well { background-color: #fbfbfb; }
      .small-note { font-size: 0.92em; color: #667085; }
      pre { white-space: pre-wrap; }
    "))
  ),
  titlePanel(div(class = "app-title", "BOIN-ETC Simulation Shiny App")),
  div(class = "muted", "Interactive front end for the BOINETC R package."),
  br(),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput("package_status"),
      tags$hr(),
      selectInput(
        "methods", "BOIN-ETC method(s)",
        choices = c("BOIN-ETC1" = "BOINETC_m1", "BOIN-ETC2" = "BOINETC_m2", "BOIN-ETC3" = "BOINETC_m3"),
        selected = c("BOINETC_m1", "BOINETC_m2", "BOINETC_m3"),
        multiple = TRUE
      ),
      selectInput(
        "scenario_ids", "Scenario(s) to simulate",
        choices = stats::setNames(as.character(1:10), paste("Scenario", 1:10)),
        selected = as.character(3:6),
        multiple = TRUE
      ),
      selectInput(
        "scenario_preview", "Scenario preview",
        choices = stats::setNames(as.character(1:10), paste("Scenario", 1:10)),
        selected = "3"
      ),
      tags$hr(),
      numericInput("ntrial", "Number of simulated trials", value = 100, min = 1, step = 100),
      textInput("ncohort", "Max cohorts, comma-separated", value = "16, 24"),
      numericInput("cohortsize", "Cohort size", value = 3, min = 1, step = 1),
      textInput("n_earlystop", "Early-stop patients, comma-separated", value = "6, 9"),
      tags$hr(),
      numericInput("phi", "Target toxicity, phi", value = 0.30, min = 0, max = 1, step = 0.01),
      textInput("delta", "Target efficacy delta", value = "0.60"),
      textInput("lambda1", "lambda1", value = "0.14"),
      textInput("lambda2", "lambda2", value = "0.35"),
      textInput("eta1", "eta1", value = "0.48"),
      textInput("u", "Utility vector u", value = "100, 25, 75, 0"),
      numericInput("alpha", "Utility prior alpha", value = 1, min = 0, step = 0.1),
      numericInput("beta", "Utility prior beta", value = 1, min = 0, step = 0.1),
      numericInput("seed", "Random seed", value = 1234, min = 1, step = 1),
      tags$hr(),
      actionButton("run", "Run simulation", class = "btn-primary"),
      br(), br(),
      div(class = "small-note", "Tip: start with 50–100 trials for a fast smoke test; increase to 1,000+ for final operating characteristics.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Scenario",
          br(),
          fluidRow(
            column(5, h4("Toxicity probability matrix"), tableOutput("tox_matrix")),
            column(5, h4("Efficacy probability matrix"), tableOutput("eff_matrix")),
            column(2, h4("Target dose(s)"), verbatimTextOutput("target_dose"))
          ),
          fluidRow(
            column(6, plotOutput("tox_plot", height = "360px")),
            column(6, plotOutput("eff_plot", height = "360px"))
          )
        ),
        tabPanel(
          "Run summary",
          br(),
          uiOutput("run_message"),
          fluidRow(
            column(4, verbatimTextOutput("run_metadata")),
            column(8, h4("Summary: one row per method/scenario/run setting"), DT::DTOutput("sum2_table"))
          ),
          br(),
          h4("Dose-level summary"),
          DT::DTOutput("sum1_table")
        ),
        tabPanel(
          "Generated files",
          br(),
          uiOutput("file_message"),
          DT::DTOutput("files_table"),
          br(),
          downloadButton("download_sum1", "Download dose-level summary CSV"),
          downloadButton("download_sum2", "Download run summary CSV"),
          downloadButton("download_all", "Download generated files ZIP")
        ),
        tabPanel(
          "How to use",
          br(),
          h4("Local setup"),
          tags$ol(
            tags$li("Install the BOINETC package tarball."),
            tags$li("Install Shiny app dependencies: shiny, DT, openxlsx, and either Iso or UniIsoRegression."),
            tags$li("Run this folder with shiny::runApp().")
          ),
          tags$pre("install.packages(c('shiny', 'DT', 'openxlsx', 'Iso'))\ninstall.packages('BOINETC_0.1.0.tar.gz', repos = NULL, type = 'source')\nshiny::runApp('BOINETC_Shiny')"),
          h4("Workflow used by the app"),
          tags$p("The app calls BOINETC::run_boinetc_workflow(), which runs the selected BOIN-ETC simulations, calls CALC.SUM(), and returns two summary tables plus the generated output files."),
          tags$p("Excel workbook generation is enabled automatically when openxlsx is installed. The summary calculation requires a package that provides biviso(), such as Iso or UniIsoRegression.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(result = NULL, error = NULL, started = FALSE)

  output$package_status <- renderUI({
    if (isTRUE(boinetc_available)) {
      tagList(
        div(class = "status-ok", "BOINETC package: available"),
        div(class = "small-note", paste("Version", as.character(utils::packageVersion("BOINETC"))))
      )
    } else {
      tagList(
        div(class = "status-bad", "BOINETC package: not available"),
        div(class = "small-note", "Install BOINETC_0.1.0.tar.gz, then restart the app.")
      )
    }
  })

  scenario_preview <- reactive({
    req(boinetc_available)
    BOINETC::get_boinetc_scenario(as.integer(input$scenario_preview))
  })

  output$tox_matrix <- renderTable({
    scenario_preview()$pt.true.mat
  }, rownames = TRUE, digits = 3)

  output$eff_matrix <- renderTable({
    scenario_preview()$pe.true.mat
  }, rownames = TRUE, digits = 3)

  output$target_dose <- renderText({
    sc <- scenario_preview()
    paste(sc$tdose, collapse = ", ")
  })

  output$tox_plot <- renderPlot({
    sc <- scenario_preview()
    plot_probability_matrix(sc$pt.true.mat, main = paste("Scenario", sc$id, "toxicity"), target = sc$tdose)
  })

  output$eff_plot <- renderPlot({
    sc <- scenario_preview()
    plot_probability_matrix(sc$pe.true.mat, main = paste("Scenario", sc$id, "efficacy"), target = sc$tdose)
  })

  observeEvent(input$run, {
    rv$started <- TRUE
    rv$error <- NULL
    rv$result <- NULL

    tryCatch({
      if (!isTRUE(boinetc_available)) {
        stop("BOINETC is not installed. Install the package tarball and restart the app.", call. = FALSE)
      }
      if (!requireNamespace("Iso", quietly = TRUE) && !requireNamespace("UniIsoRegression", quietly = TRUE)) {
        stop("Install 'Iso' or 'UniIsoRegression' before running summaries. Example: install.packages('Iso')", call. = FALSE)
      }

      methods <- input$methods
      if (length(methods) == 0) {
        stop("Select at least one BOIN-ETC method.", call. = FALSE)
      }

      scenario_ids <- as.integer(input$scenario_ids)
      if (length(scenario_ids) == 0 || anyNA(scenario_ids)) {
        stop("Select at least one scenario.", call. = FALSE)
      }

      ncohort <- parse_num_vector(input$ncohort, "Max cohorts", integer = TRUE)
      n_earlystop <- parse_num_vector(input$n_earlystop, "Early-stop patients", integer = TRUE)
      if (length(ncohort) != length(n_earlystop)) {
        stop("Max cohorts and early-stop patients must have the same number of values.", call. = FALSE)
      }

      delta <- parse_num_vector(input$delta, "delta")
      lambda1 <- parse_num_vector(input$lambda1, "lambda1")
      lambda2 <- parse_num_vector(input$lambda2, "lambda2")
      eta1 <- parse_num_vector(input$eta1, "eta1")
      u <- parse_num_vector(input$u, "Utility vector u", expected_length = 4)

      base_output_dir <- file.path(getwd(), "BOINETC_Output")
      dir.create(base_output_dir, recursive = TRUE, showWarnings = FALSE)
      outdir <- file.path(base_output_dir, paste0("boinetc_shiny_", format(Sys.time(), "%Y%m%d_%H%M%S")))
      dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

      withProgress(message = "Running BOIN-ETC simulation", value = 0.1, {
        incProgress(0.2, detail = "Simulating selected methods and scenarios")
        res <- BOINETC::run_boinetc_workflow(
          methods = methods,
          scenario_ids = scenario_ids,
          ncohort = ncohort,
          cohortsize = as.integer(input$cohortsize),
          n.earlystop = n_earlystop,
          ntrial = as.integer(input$ntrial),
          phi = as.numeric(input$phi),
          delta = delta,
          lambda1 = lambda1,
          lambda2 = lambda2,
          eta1 = eta1,
          u = u,
          alpha = as.numeric(input$alpha),
          beta = as.numeric(input$beta),
          outdir = outdir,
          seed = as.integer(input$seed),
          write_workbook = requireNamespace("openxlsx", quietly = TRUE)
        )
        incProgress(0.8, detail = "Preparing outputs")
        rv$result <- res
      })
    }, error = function(e) {
      rv$error <- conditionMessage(e)
    })
  })

  output$run_message <- renderUI({
    if (!is.null(rv$error)) {
      div(class = "alert alert-danger", strong("Run failed: "), rv$error)
    } else if (!is.null(rv$result)) {
      div(class = "alert alert-success", "Simulation completed. Review the tables below or download the generated files.")
    } else if (isTRUE(rv$started)) {
      div(class = "alert alert-info", "No result is available yet.")
    } else {
      div(class = "alert alert-info", "Set the inputs in the sidebar and click Run simulation.")
    }
  })

  output$run_metadata <- renderText({
    req(rv$result)
    res <- rv$result
    paste(
      "Output directory:", res$outdir,
      "\nWorkbook:", ifelse(is.na(res$workbook_file), "Not written; install openxlsx to enable Excel output.", res$workbook_file),
      "\nNumber of simulation result files:", length(res$simulation),
      sep = ""
    )
  })

  output$sum2_table <- DT::renderDT({
    req(rv$result)
    DT::datatable(as_display_df(rv$result$out.sum2), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$sum1_table <- DT::renderDT({
    req(rv$result)
    DT::datatable(as_display_df(rv$result$out.sum1), options = list(scrollX = TRUE, pageLength = 10))
  })

  generated_files <- reactive({
    req(rv$result)
    files <- list.files(rv$result$outdir, full.names = TRUE, recursive = FALSE)
    info <- file.info(files)
    data.frame(
      file = basename(files),
      size_kb = round(info$size / 1024, 1),
      modified = as.character(info$mtime),
      path = files,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })

  output$file_message <- renderUI({
    if (is.null(rv$result)) {
      div(class = "alert alert-info", "Run a simulation to generate downloadable files.")
    } else {
      div(class = "alert alert-success", "Generated files are stored in a temporary app directory for this R session.")
    }
  })

  output$files_table <- DT::renderDT({
    req(rv$result)
    DT::datatable(generated_files(), options = list(scrollX = TRUE, pageLength = 15))
  })

  output$download_sum1 <- downloadHandler(
    filename = function() paste0("BOINETC_dose_level_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$result)
      utils::write.csv(rv$result$out.sum1, file, row.names = FALSE)
    }
  )

  output$download_sum2 <- downloadHandler(
    filename = function() paste0("BOINETC_run_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$result)
      utils::write.csv(rv$result$out.sum2, file, row.names = FALSE)
    }
  )

  output$download_all <- downloadHandler(
    filename = function() paste0("BOINETC_generated_files_", Sys.Date(), ".zip"),
    content = function(file) {
      req(rv$result)
      files <- list.files(rv$result$outdir, full.names = TRUE, recursive = FALSE)
      oldwd <- setwd(rv$result$outdir)
      on.exit(setwd(oldwd), add = TRUE)
      utils::zip(zipfile = file, files = basename(files))
    }
  )
}

shinyApp(ui, server)
