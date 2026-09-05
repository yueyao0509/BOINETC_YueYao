run_boinetc_study <- function(methods = c("BOINETC_m1", "BOINETC_m2", "BOINETC_m3"),
                              scenario_ids = 3:6,
                              ncohort = c(16, 24),
                              cohortsize = 3,
                              n.earlystop = c(6, 9),
                              ntrial = 1000,
                              phi = 0.30,
                              delta = 0.60,
                              lambda1 = 0.14,
                              lambda2 = 0.35,
                              eta1 = 0.48,
                              u.min = NULL,
                              outdir = getOption("BOINETC.outdir", "."),
                              seed = NULL,
                              filename_prefix = NULL) {
  if (length(ncohort) != length(n.earlystop)) {
    stop("ncohort and n.earlystop must have the same length.", call. = FALSE)
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }

  outdir <- .boinetc_ensure_outdir(outdir)
  methods <- vapply(methods, .boinetc_method_name, character(1L))

  results <- list()
  counter <- 1L
  for (kk in seq_along(delta)) {
    for (sc in scenario_ids) {
      scenario <- get_boinetc_scenario(sc)
      for (ll in seq_along(n.earlystop)) {
        for (method in methods) {
          fn <- .boinetc_method_function(method)
          lambda1_use <- lambda1[min(kk, length(lambda1))]
          lambda2_use <- lambda2[min(kk, length(lambda2))]
          eta1_use <- eta1[min(kk, length(eta1))]
          delta_use <- delta[kk]
          # Include the parameter-set index in every simulation filename. Without
          # this tag, multiple delta/lambda/eta settings overwrite one another.
          base_filename <- paste0(method, ".new.sc", sc, "_cs", cohortsize,
                                  "_ns", n.earlystop[ll], "_ps", kk)
          if (!is.null(filename_prefix) && nzchar(filename_prefix)) {
            base_filename <- paste0(filename_prefix, base_filename)
          }
          res <- fn(ncohort = ncohort[ll],
                    cohortsize = cohortsize,
                    n.earlystop = n.earlystop[ll],
                    ntrial = ntrial,
                    phi = phi,
                    delta = delta_use,
                    lambda1 = lambda1_use,
                    lambda2 = lambda2_use,
                    eta1 = eta1_use,
                    u.min = u.min,
                    n.scr = sc,
                    tdose = scenario$tdose,
                    pt.true.mat = scenario$pt.true.mat,
                    pe.true.mat = scenario$pe.true.mat,
                    filename = base_filename,
                    outdir = outdir)
          results[[counter]] <- list(
            method = method,
            scenario = sc,
            ncohort = ncohort[ll],
            cohortsize = cohortsize,
            n.earlystop = n.earlystop[ll],
            ntrial = ntrial,
            phi = phi,
            delta = delta_use,
            lambda1 = lambda1_use,
            lambda2 = lambda2_use,
            eta1 = eta1_use,
            parameter_set = kk,
            filename = base_filename,
            file = res$file
          )
          counter <- counter + 1L
        }
      }
    }
  }

  class(results) <- c("boinetc_study_results", class(results))
  results
}

write_boinetc_summary_workbook <- function(out.sum1,
                                           out.sum2,
                                           file,
                                           overwrite = TRUE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required to write Excel workbooks. Install it or use the returned data frames directly.", call. = FALSE)
  }
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sum1")
  openxlsx::addWorksheet(wb, "Sum2")
  openxlsx::writeDataTable(wb, "Sum1", x = out.sum1)
  openxlsx::writeDataTable(wb, "Sum2", x = out.sum2)
  openxlsx::saveWorkbook(wb, file, overwrite = overwrite)
  invisible(file)
}

run_boinetc_workflow <- function(methods = c("BOINETC_m1", "BOINETC_m2", "BOINETC_m3"),
                                 scenario_ids = 3:6,
                                 ncohort = c(16, 24),
                                 cohortsize = 3,
                                 n.earlystop = c(6, 9),
                                 ntrial = 1000,
                                 phi = 0.30,
                                 delta = 0.60,
                                 lambda1 = 0.14,
                                 lambda2 = 0.35,
                                 eta1 = 0.48,
                                 u = c(100, 25, 75, 0),
                                 alpha = 1,
                                 beta = 1,
                                 u.min = NULL,
                                 outdir = getOption("BOINETC.outdir", "."),
                                 seed = 1234,
                                 write_workbook = TRUE,
                                 workbook_file = NULL) {
  outdir <- .boinetc_ensure_outdir(outdir)
  methods <- vapply(methods, .boinetc_method_name, character(1L))

  sim <- run_boinetc_study(methods = methods,
                           scenario_ids = scenario_ids,
                           ncohort = ncohort,
                           cohortsize = cohortsize,
                           n.earlystop = n.earlystop,
                           ntrial = ntrial,
                           phi = phi,
                           delta = delta,
                           lambda1 = lambda1,
                           lambda2 = lambda2,
                           eta1 = eta1,
                           u.min = u.min,
                           outdir = outdir,
                           seed = seed)

  sum1_list <- list()
  sum2_list <- list()
  for (i in seq_along(sim)) {
    item <- sim[[i]]
    aaa <- CALC.SUM(SC = item$scenario,
                    FILENAME = item$filename,
                    outdir = outdir,
                    ntrial = item$ntrial,
                    phi = item$phi,
                    ncohort = item$ncohort,
                    cohortsize = item$cohortsize,
                    delta = item$delta,
                    n.es = item$n.earlystop,
                    lambda1 = item$lambda1,
                    lambda2 = item$lambda2,
                    eta1 = item$eta1,
                    parameter_set = item$parameter_set,
                    u = u,
                    alpha = alpha,
                    beta = beta,
                    method = item$method,
                    write_summary = TRUE)
    sum1_list[[i]] <- aaa$out.sum1
    sum2_list[[i]] <- aaa$out.sum2
  }

  out.sum1.all <- do.call(rbind, sum1_list)
  out.sum2.all <- do.call(rbind, sum2_list)

  if (is.null(workbook_file)) {
    dims <- unique(vapply(scenario_ids, function(sc) paste(dim(BOINETC_ptox[[sc]]), collapse = "x"), character(1L)))
    out.dl <- paste(dims, collapse = "-")
    out.util <- paste(u[1], u[2], u[3], u[4], sep = "-")
    workbook_file <- file.path(outdir, paste0("BOINETC.new.summary_", out.dl, "_", out.util, ".xlsx"))
  }

  if (isTRUE(write_workbook)) {
    write_boinetc_summary_workbook(out.sum1.all, out.sum2.all, workbook_file, overwrite = TRUE)
  }

  list(
    simulation = sim,
    out.sum1 = out.sum1.all,
    out.sum2 = out.sum2.all,
    workbook_file = if (isTRUE(write_workbook)) workbook_file else NA_character_,
    outdir = outdir
  )
}
