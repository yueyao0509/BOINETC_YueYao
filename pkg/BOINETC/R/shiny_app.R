run_boinetc_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the BOINETC Shiny app. Install it with install.packages('shiny').", call. = FALSE)
  }
  app_dir <- system.file("shiny", "boinetc_app", package = "BOINETC")
  if (!nzchar(app_dir)) {
    stop("The BOINETC Shiny app was not found in this package installation.", call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
