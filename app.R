# BOINETC Trial Design Shiny App
# Open this file in RStudio and click "Run App".

required_packages <- c("shiny", "DT", "ggplot2", "readxl", "openxlsx")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Please install these packages first: ", paste(missing_packages, collapse = ", "),
       call. = FALSE)
}

library(shiny)
library(DT)
library(ggplot2)

library(Iso)
library(UniIsoRegression)
library(mfp)
library(openxlsx)
library(readr)
library(tidyverse)

`%||%` <- function(x, y) if (is.null(x)) y else x
this_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE), error = function(e) NULL)
APP_DIR <- normalizePath(dirname(this_file %||% getwd()), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(APP_DIR, "output"))) APP_DIR <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
OUT_DIR <- file.path(APP_DIR, "output")
PROG_DIR <- file.path(APP_DIR, "prog")

source(file.path(PROG_DIR, "BOINETC.scenario.R"), local = TRUE)

list_ptox <- list(pt.true.mat1, pt.true.mat2, pt.true.mat3, pt.true.mat4, pt.true.mat5,
                  pt.true.mat6, pt.true.mat7, pt.true.mat8, pt.true.mat9, pt.true.mat10)
list_peff <- list(pe.true.mat1, pe.true.mat2, pe.true.mat3, pe.true.mat4, pe.true.mat5,
                  pe.true.mat6, pe.true.mat7, pe.true.mat8, pe.true.mat9, pe.true.mat10)
list_tdose <- list(c(4, 5), c(2, 3), c(8, 11, 14), c(7, 10), c(4, 7), c(7, 10),
                   c(9, 11, 13), c(6, 8, 10), c(9, 11), c(8, 10))

summary_file <- list.files(OUT_DIR, pattern = "summary.*\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
if (length(summary_file) == 0) stop("No summary xlsx file found in output/", call. = FALSE)
summary_file <- summary_file[1]
sum1_all <- readxl::read_excel(summary_file, sheet = "Sum1") |> as.data.frame()
sum2_all <- readxl::read_excel(summary_file, sheet = "Sum2") |> as.data.frame()

available_scenarios <- sort(unique(sum2_all$scenario))
available_methods <- sort(unique(sum2_all$method))
available_es <- sort(unique(sum2_all$n.earlystop))

linear_to_ab <- function(idx, n_a) {
  data.frame(
    Dose_A = ((idx - 1) %% n_a) + 1,
    Dose_B = floor((idx - 1) / n_a) + 1
  )
}

truth_df <- function(sc) {
  tox <- list_ptox[[sc]]
  eff <- list_peff[[sc]]
  n_a <- nrow(tox); n_b <- ncol(tox)
  expand.grid(Dose_A = seq_len(n_a), Dose_B = seq_len(n_b)) |>
    transform(
      Toxicity = as.vector(tox),
      Efficacy = as.vector(eff),
      Linear_Dose = seq_len(n_a * n_b),
      True_ODC = seq_len(n_a * n_b) %in% list_tdose[[sc]]
    )
}

selected_sum2 <- function(input) {
  subset(sum2_all,
         scenario == input$scenario & method == input$method & n.earlystop == input$earlystop)
}

selected_sum1 <- function(input) {
  subset(sum1_all,
         scenario == input$scenario & method == input$method & n.earlystop == input$earlystop)
}

raw_file_name <- function(method, sc, earlystop, cohortsize = 3) {
  file.path(OUT_DIR, paste0(method, ".new.sc", sc, "_cs", cohortsize, "_ns", earlystop, "_data.csv"))
}

allocation_df <- function(method, sc, earlystop) {
  f <- raw_file_name(method, sc, earlystop)
  if (!file.exists(f)) return(data.frame())
  dat <- read.csv(f, check.names = FALSE)
  dose_rows <- dat[1:2, -1, drop = FALSE]
  nd <- ncol(list_ptox[[sc]]) * nrow(list_ptox[[sc]])
  trial_rows <- dat[-c(1, 2), -1, drop = FALSE]
  npts <- as.data.frame(lapply(trial_rows[, seq_len(nd), drop = FALSE], as.numeric))
  data.frame(
    Dose_A = as.numeric(dose_rows[1, seq_len(nd)]),
    Dose_B = as.numeric(dose_rows[2, seq_len(nd)]),
    Mean_NPTS = colMeans(npts, na.rm = TRUE),
    Mean_PPTS = colMeans(npts, na.rm = TRUE) / rowSums(npts, na.rm = TRUE)[1] * 100
  )
}


run_one_boinetc <- function(method, sc, n.es, ntrial.run) {
  sim_packages <- c("Iso", "UniIsoRegression", "mfp", "readr")
  missing_sim <- sim_packages[!vapply(sim_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_sim) > 0) {
    stop("Install simulation packages first: ", paste(missing_sim, collapse = ", "), call. = FALSE)
  }

  sc <- as.numeric(sc)
  n.es <- as.numeric(n.es)
  ntrial.run <- as.numeric(ntrial.run)
  ncohort.now <- if (n.es == 6) 16 else if (n.es == 9) 24 else stop("Only n.earlystop 6 or 9 is mapped to ncohort in the current 4x4 design.", call. = FALSE)

  sim_env <- new.env(parent = globalenv())
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(PROG_DIR)

  sys.source("FUNC_BOINETC.subtrial.R", envir = sim_env)
  sys.source("get.oc.BOINETC_m1_v02.R", envir = sim_env)
  sys.source("get.oc.BOINETC_m2_v02.R", envir = sim_env)
  sys.source("get.oc.BOINETC_m3_v02.R", envir = sim_env)
  sys.source("FUNC_CALC.SUM.R", envir = sim_env)
  sys.source("BOINETC.scenario.R", envir = sim_env)

  sim_env$list.p.truetox <- list(sim_env$pt.true.mat1, sim_env$pt.true.mat2, sim_env$pt.true.mat3,
                                 sim_env$pt.true.mat4, sim_env$pt.true.mat5, sim_env$pt.true.mat6,
                                 sim_env$pt.true.mat7, sim_env$pt.true.mat8, sim_env$pt.true.mat9,
                                 sim_env$pt.true.mat10)
  sim_env$list.p.trueeff <- list(sim_env$pe.true.mat1, sim_env$pe.true.mat2, sim_env$pe.true.mat3,
                                 sim_env$pe.true.mat4, sim_env$pe.true.mat5, sim_env$pe.true.mat6,
                                 sim_env$pe.true.mat7, sim_env$pe.true.mat8, sim_env$pe.true.mat9,
                                 sim_env$pe.true.mat10)
  sim_env$list.tdose <- list(c(4, 5), c(2, 3), c(8, 11, 14), c(7, 10), c(4, 7), c(7, 10),
                             c(9, 11, 13), c(6, 8, 10), c(9, 11), c(8, 10))

  sim_env$phi <- 0.30
  sim_env$delta <- 0.60
  sim_env$cohortsize <- 3
  sim_env$ntrial <- ntrial.run
  sim_env$lambda1 <- 0.16
  sim_env$lambda2 <- 0.33
  sim_env$eta1 <- 0.46
  sim_env$u <- c(100, 25, 75, 0)
  sim_env$alpha <- 1
  sim_env$beta <- 1
  sim_env$u.min <- min(sim_env$u)
  sim_env$OUT <- paste0(normalizePath(OUT_DIR, winslash = "/", mustWork = FALSE), "/")
  sim_env$OUTFILE <- sim_env$OUT
  sim_env$method <- method
  sim_env$n.es <- n.es
  sim_env$ncohort <- ncohort.now

  filename <- paste0(method, ".new.sc", sc, "_cs", sim_env$cohortsize, "_ns", n.es)
  fun_name <- switch(method,
                     BOINETC_m1 = "get.oc.BOINETC",
                     BOINETC_m2 = "get.oc.BOINETC_m2",
                     BOINETC_m3 = "get.oc.BOINETC_m3",
                     stop("Unknown method: ", method, call. = FALSE))

  set.seed(1234)
  do.call(get(fun_name, envir = sim_env), list(
    ncohort = ncohort.now,
    cohortsize = sim_env$cohortsize,
    n.earlystop = n.es,
    ntrial = ntrial.run,
    phi = sim_env$phi,
    delta = sim_env$delta,
    lambda1 = sim_env$lambda1,
    lambda2 = sim_env$lambda2,
    eta1 = sim_env$eta1,
    u.min = sim_env$u.min,
    n.scr = sc,
    tdose = sim_env$list.tdose[[sc]],
    pt.true.mat = sim_env$list.p.truetox[[sc]],
    pe.true.mat = sim_env$list.p.trueeff[[sc]],
    filename = filename
  ))

  aaa <- sim_env$CALC.SUM(FILENAME = filename, SC = sc)
  list(filename = filename,
       data_file = file.path(OUT_DIR, paste0(filename, "_data.csv")),
       sum_file = file.path(OUT_DIR, paste0(filename, "_sum.txt")),
       out.sum1 = aaa$out.sum1,
       out.sum2 = aaa$out.sum2)
}

replace_rows <- function(old, new) {
  keep <- !(old$scenario == new$scenario[1] & old$method == new$method[1] & old$n.earlystop == new$n.earlystop[1])
  rbind(old[keep, , drop = FALSE], new)
}

plot_matrix <- function(df, value_col, title, fill_label) {
  ggplot(df, aes(x = Dose_B, y = Dose_A, fill = .data[[value_col]])) +
    geom_tile(color = "white") +
    geom_text(aes(label = ifelse(True_ODC, paste0(.data[[value_col]], "\nODC"), .data[[value_col]])), size = 4) +
    scale_x_continuous(breaks = sort(unique(df$Dose_B))) +
    scale_y_continuous(breaks = sort(unique(df$Dose_A))) +
    labs(title = title, x = "Dose B", y = "Dose A", fill = fill_label) +
    theme_minimal(base_size = 13)
}

ui <- fluidPage(
  titlePanel("BOINETC Trial Design Shiny App"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("Design Inputs"),
      selectInput("scenario", "Scenario", choices = available_scenarios, selected = 4),
      selectInput("method", "Method", choices = available_methods, selected = "BOINETC_m3"),
      selectInput("earlystop", "Early stopping threshold (n.earlystop)", choices = available_es, selected = 9),
      tags$hr(),
      h4("Fixed Settings in Current Output"),
      tags$p("The current precomputed files use cohort size = 3 and utility score = 100-25-75-0."),
      numericInput("ntrial", "Simulation replications for new run", value = 100, min = 1, step = 50),
      actionButton("run_sim", "Run New Simulation", class = "btn-primary"),
      br(), br(),
      verbatimTextOutput("run_status"),
      tags$hr(),
      downloadButton("download_sum2", "Download selected Sum2"),
      br(), br(),
      downloadButton("download_truth", "Download truth matrix")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Overview",
          br(),
          h4("Selected Setting"),
          verbatimTextOutput("overview_text"),
          h4("Selected Summary Metrics"),
          DTOutput("selected_sum2_table"),
          h4("True ODC(s)"),
          DTOutput("true_odc_table")
        ),
        tabPanel("Scenario Truth",
          br(),
          fluidRow(
            column(6, plotOutput("tox_plot", height = "420px")),
            column(6, plotOutput("eff_plot", height = "420px"))
          ),
          h4("Truth Matrix Table"),
          DTOutput("truth_table")
        ),
        tabPanel("Compare Methods",
          br(),
          h4("All Method / Early-stop Results for Current Scenario"),
          DTOutput("compare_table"),
          h4("Correct pODC Comparison"),
          plotOutput("compare_plot", height = "420px")
        ),
        tabPanel("Dose Allocation",
          br(),
          h4("Mean Number of Patients Allocated to Each Dose Combination"),
          plotOutput("allocation_plot", height = "460px"),
          DTOutput("allocation_table")
        ),
        tabPanel("Sum1 Details",
          br(),
          h4("Dose-A Level Summary from Sum1"),
          DTOutput("sum1_table")
        ),
        tabPanel("Raw Output Data",
          br(),
          tags$p("This is the raw precomputed simulation output for the selected scenario/method/early stopping setting."),
          DTOutput("raw_table")
        ),
        tabPanel("Run Simulation Note",
          br(),
          h4("Important"),
          tags$p("This app is currently connected to the precomputed simulation output in output/."),
          tags$p("A full Run Simulation button can be added after the mentor confirms which inputs should be user-editable. Running the full simulation may take time because it calls the BOINETC simulation functions repeatedly."),
          h4("Files included"),
          verbatimTextOutput("file_note")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    sum1_all = sum1_all,
    sum2_all = sum2_all,
    last_run = NULL,
    status = "No new simulation run in this session.",
    active_scenario = 4,
    active_method = "BOINETC_m3",
    active_earlystop = 9
  )

  # IMPORTANT:
  # Outputs below use rv$active_* instead of input$*.
  # Therefore changing dropdowns will NOT change the displayed results.
  # The displayed setting changes only after clicking Run New Simulation.
  input_df <- reactive(truth_df(as.numeric(rv$active_scenario)))
  odc_df <- reactive(subset(input_df(), True_ODC))
  sum2_sel <- reactive(subset(rv$sum2_all, scenario == rv$active_scenario & method == rv$active_method & n.earlystop == rv$active_earlystop))
  sum1_sel <- reactive(subset(rv$sum1_all, scenario == rv$active_scenario & method == rv$active_method & n.earlystop == rv$active_earlystop))
  alloc_sel <- reactive(allocation_df(rv$active_method, as.numeric(rv$active_scenario), rv$active_earlystop))



  observeEvent(input$run_sim, {
    rv$active_scenario <- as.numeric(input$scenario)
    rv$active_method <- input$method
    rv$active_earlystop <- as.numeric(input$earlystop)

    rv$status <- "Running simulation..."
    tryCatch({
      res <- run_one_boinetc(rv$active_method, rv$active_scenario, rv$active_earlystop, input$ntrial)
      rv$sum1_all <- replace_rows(rv$sum1_all, res$out.sum1)
      rv$sum2_all <- replace_rows(rv$sum2_all, res$out.sum2)
      rv$last_run <- res
      wb <- createWorkbook()
      addWorksheet(wb, "Sum1")
      addWorksheet(wb, "Sum2")
      writeDataTable(wb, "Sum1", x = rv$sum1_all)
      writeDataTable(wb, "Sum2", x = rv$sum2_all)
      saveWorkbook(wb, summary_file, overwrite = TRUE)
      rv$status <- paste0("Finished: ", basename(res$data_file), " and ", basename(res$sum_file), " were written to output/.")
    }, error = function(e) {
      rv$status <- paste("Simulation failed:", conditionMessage(e))
    })
  })

  output$run_status <- renderText(rv$status)

  output$overview_text <- renderText({
    s2 <- sum2_sel()
    if (nrow(s2) == 0) return("No matching summary row found.")
    paste0(
      "Scenario: ", rv$active_scenario, "\n",
      "Method: ", rv$active_method, "\n",
      "Early stopping threshold: ", rv$active_earlystop, "\n",
      "Correct pODC (%): ", s2$correct.pODC[1], "\n",
      "Mean number of patients: ", s2$mean.npat[1], "\n",
      "Target PPTS: ", s2$target.PPTS[1]
    )
  })

  output$selected_sum2_table <- renderDT({
    datatable(sum2_sel(), rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE))
  })

  output$true_odc_table <- renderDT({
    datatable(odc_df(), rownames = FALSE, options = list(pageLength = 10))
  })

  output$tox_plot <- renderPlot({
    plot_matrix(input_df(), "Toxicity", paste("Scenario", rv$active_scenario, "True Toxicity"), "Toxicity")
  })

  output$eff_plot <- renderPlot({
    plot_matrix(input_df(), "Efficacy", paste("Scenario", rv$active_scenario, "True Efficacy"), "Efficacy")
  })

  output$truth_table <- renderDT({
    datatable(input_df(), rownames = FALSE, options = list(pageLength = 16, scrollX = TRUE))
  })

  output$compare_table <- renderDT({
    x <- subset(rv$sum2_all, scenario == rv$active_scenario)
    x <- x[order(-x$correct.pODC, x$target.PPTS), ]
    datatable(x, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$compare_plot <- renderPlot({
    x <- subset(rv$sum2_all, scenario == rv$active_scenario)
    ggplot(x, aes(x = method, y = correct.pODC, group = factor(n.earlystop))) +
      geom_point(size = 3) +
      geom_line(aes(linetype = factor(n.earlystop))) +
      labs(x = "Method", y = "Correct pODC (%)", linetype = "n.earlystop",
           title = paste("Scenario", rv$active_scenario, "Correct pODC by Method")) +
      theme_minimal(base_size = 13)
  })

  output$allocation_plot <- renderPlot({
    x <- alloc_sel()
    validate(need(nrow(x) > 0, "No raw output file found for this setting."))
    ggplot(x, aes(x = Dose_B, y = Dose_A, fill = Mean_NPTS)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Mean_NPTS, 1)), size = 4) +
      scale_x_continuous(breaks = sort(unique(x$Dose_B))) +
      scale_y_continuous(breaks = sort(unique(x$Dose_A))) +
      labs(x = "Dose B", y = "Dose A", fill = "Mean NPTS") +
      theme_minimal(base_size = 13)
  })

  output$allocation_table <- renderDT({
    datatable(alloc_sel(), rownames = FALSE, options = list(pageLength = 16))
  })

  output$sum1_table <- renderDT({
    datatable(sum1_sel(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$raw_table <- renderDT({
    f <- raw_file_name(rv$active_method, rv$active_scenario, rv$active_earlystop)
    validate(need(file.exists(f), paste("File not found:", basename(f))))
    x <- read.csv(f, check.names = FALSE)
    datatable(x, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$file_note <- renderText({
    paste0("app.R\nprog/ original BOINETC R functions\noutput/ precomputed BOINETC simulation results\n\nSummary file: ", basename(summary_file))
  })

  output$download_sum2 <- downloadHandler(
    filename = function() paste0("BOINETC_selected_sum2_sc", rv$active_scenario, "_", rv$active_method, "_ns", rv$active_earlystop, ".csv"),
    content = function(file) write.csv(sum2_sel(), file, row.names = FALSE)
  )

  output$download_truth <- downloadHandler(
    filename = function() paste0("BOINETC_truth_matrix_sc", rv$active_scenario, ".csv"),
    content = function(file) write.csv(input_df(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
