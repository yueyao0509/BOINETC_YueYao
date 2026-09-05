CALC.SUM <- function(FILENAME, SC,
                     outdir = NULL,
                     list.tdose = BOINETC_tdose,
                     list.p.truetox = BOINETC_ptox,
                     list.p.trueeff = BOINETC_peff,
                     ntrial = NULL,
                     phi = NULL,
                     ncohort = NULL,
                     cohortsize = NULL,
                     delta = NULL,
                     n.es = NULL,
                     lambda1 = NULL,
                     lambda2 = NULL,
                     eta1 = NULL,
                     parameter_set = 1L,
                     u = NULL,
                     alpha = NULL,
                     beta = NULL,
                     method = NULL,
                     write_summary = TRUE) {
  pf <- parent.frame()
  outdir <- .boinetc_resolve_outdir(outdir, pf)

  if (is.null(ntrial)) {
    ntrial <- .boinetc_parent_value("ntrial", default = NULL, env = pf)
  }
  if (is.null(phi)) {
    phi <- .boinetc_parent_value("phi", default = NA_real_, env = pf)
  }
  if (is.null(ncohort)) {
    ncohort <- .boinetc_parent_value("ncohort", default = NA_real_, env = pf)
  }
  if (is.null(cohortsize)) {
    cohortsize <- .boinetc_parent_value("cohortsize", default = NA_real_, env = pf)
  }
  if (is.null(delta)) {
    delta <- .boinetc_parent_value("delta", default = NA_real_, env = pf)
  }
  if (is.null(n.es)) {
    n.es <- .boinetc_parent_value("n.es", default = .boinetc_parent_value("n.earlystop", default = NA_real_, env = pf), env = pf)
  }
  if (is.null(lambda1)) {
    lambda1 <- .boinetc_parent_value("lambda1", default = NA_real_, env = pf)
  }
  if (is.null(lambda2)) {
    lambda2 <- .boinetc_parent_value("lambda2", default = NA_real_, env = pf)
  }
  if (is.null(eta1)) {
    eta1 <- .boinetc_parent_value("eta1", default = NA_real_, env = pf)
  }
  if (is.null(u)) {
    u <- .boinetc_parent_value("u", default = c(100, 25, 75, 0), env = pf)
  }
  if (is.null(alpha)) {
    alpha <- .boinetc_parent_value("alpha", default = 1, env = pf)
  }
  if (is.null(beta)) {
    beta <- .boinetc_parent_value("beta", default = 1, env = pf)
  }
  if (is.null(method)) {
    method <- .boinetc_parent_value("method", default = sub("\\.new.*$", "", FILENAME), env = pf)
  }

  infile <- .boinetc_output_file(outdir, FILENAME, "_data.csv")
  data <- utils::read.csv(infile, check.names = FALSE)
  DATA <- as.data.frame(lapply(data[-c(1, 2), -1, drop = FALSE], as.numeric))
  DOSE <- as.data.frame(lapply(data[1:2, -1, drop = FALSE], as.numeric))

  if (is.null(ntrial)) {
    ntrial <- nrow(DATA)
  }

  tdose <- list.tdose[[SC]]
  nd1 <- max(as.numeric(unlist(data[1, -1], use.names = FALSE)), na.rm = TRUE)
  nd2 <- max(as.numeric(unlist(data[2, -1], use.names = FALSE)), na.rm = TRUE)
  nd <- nd1 * nd2
  non.ODC <- 0

  for (i in seq_len(ntrial)) {
    row_values <- function(cols) {
      as.numeric(unlist(DATA[i, cols, drop = FALSE], use.names = FALSE))
    }
    npts <- matrix(row_values(1:nd), nrow = nd1)
    ntox <- matrix(row_values((nd + 1):(nd * 2)), nrow = nd1)
    neff <- matrix(row_values((nd * 2 + 1):(nd * 3)), nrow = nd1)
    ppts <- matrix(row_values((nd * 3 + 1):(nd * 4)), nrow = nd1)
    ptox <- matrix(row_values((nd * 4 + 1):(nd * 5)), nrow = nd1)
    peff <- matrix(row_values((nd * 5 + 1):(nd * 6)), nrow = nd1)
    nct1 <- matrix(row_values((nd * 6 + 1):(nd * 7)), nrow = nd1)
    nct2 <- matrix(row_values((nd * 7 + 1):(nd * 8)), nrow = nd1)
    nct3 <- matrix(row_values((nd * 8 + 1):(nd * 9)), nrow = nd1)
    nct4 <- matrix(row_values((nd * 9 + 1):(nd * 10)), nrow = nd1)
    pct1 <- matrix(row_values((nd * 10 + 1):(nd * 11)), nrow = nd1)
    pct2 <- matrix(row_values((nd * 11 + 1):(nd * 12)), nrow = nd1)
    pct3 <- matrix(row_values((nd * 12 + 1):(nd * 13)), nrow = nd1)
    pct4 <- matrix(row_values((nd * 13 + 1):(nd * 14)), nrow = nd1)

    if (i == 1) {
      NPTS <- list(npts); NTOX <- list(ntox); NEFF <- list(neff)
      PPTS <- list(ppts); PTOX <- list(ptox); PEFF <- list(peff)
      NCT1 <- list(nct1); NCT2 <- list(nct2); NCT3 <- list(nct3)
      NCT4 <- list(nct4); PCT1 <- list(pct1); PCT2 <- list(pct2)
      PCT3 <- list(pct3); PCT4 <- list(pct4)
    } else {
      NPTS <- append(NPTS, list(npts)); NTOX <- append(NTOX, list(ntox)); NEFF <- append(NEFF, list(neff))
      PPTS <- append(PPTS, list(ppts)); PTOX <- append(PTOX, list(ptox)); PEFF <- append(PEFF, list(peff))
      NCT1 <- append(NCT1, list(nct1)); NCT2 <- append(NCT2, list(nct2)); NCT3 <- append(NCT3, list(nct3))
      NCT4 <- append(NCT4, list(nct4)); PCT1 <- append(PCT1, list(pct1)); PCT2 <- append(PCT2, list(pct2))
      PCT3 <- append(PCT3, list(pct3)); PCT4 <- append(PCT4, list(pct4))
    }

    ntox.c <- row_values((nd + 1):(nd * 2))
    npts.c <- row_values(1:nd)
    trial.ptox <- matrix((ntox.c + 0.05) / (npts.c + 0.1), nrow = nd1)
    phat.tox <- .boinetc_biviso(trial.ptox)

    dl.MTD <- which(abs(phat.tox - phi) == apply(abs(phat.tox - phi), 1, min))

    for (k in seq_along(dl.MTD)) {
      dd1 <- as.numeric(DOSE[1, dl.MTD[k], drop = TRUE])
      dd2 <- as.numeric(DOSE[2, dl.MTD[k], drop = TRUE])
      if (k == 1) {
        dl.tol <- c(0:(dd2 - 1)) * nd1 + dd1
      } else {
        dl.tol <- c(dl.tol, c(0:(dd2 - 1)) * nd1 + dd1)
      }
    }
    ind.tol <- matrix(numeric(nd), nrow = nd1, ncol = nd2)
    ind.tol[dl.tol] <- 1

    nct1.c <- row_values((nd * 6 + 1):(nd * 7))
    nct2.c <- row_values((nd * 7 + 1):(nd * 8))
    nct3.c <- row_values((nd * 8 + 1):(nd * 9))
    nct4.c <- row_values((nd * 9 + 1):(nd * 10))

    x <- (u[1] * nct1.c + u[2] * nct2.c + u[3] * nct3.c + u[4] * nct4.c) / 100
    util <- matrix((x + alpha) / (npts + alpha + beta), nrow = nd1)
    util <- util * ind.tol

    odc <- which(c(util) == max(util, na.rm = TRUE) & util > 0)
    if (i == 1) {
      ODC <- list(odc)
    } else {
      ODC <- append(ODC, list(odc))
    }
    if (length(odc) == 0) {
      non.ODC <- non.ODC + 1
    }

    if (i == 1) {
      SUM.N <- sum(npts)
    } else {
      SUM.N <- c(SUM.N, sum(npts))
    }
  }

  m.NPTS <- round(Reduce("+", NPTS) / ntrial, digits = 2)
  m.NTOX <- round(Reduce("+", NTOX) / ntrial, digits = 2)
  m.NEFF <- round(Reduce("+", NEFF) / ntrial, digits = 2)
  m.PPTS <- round(Reduce("+", PPTS) / ntrial * 100, digits = 1)
  m.pTOX <- round(.boinetc_ratio(Reduce("+", NTOX), Reduce("+", NPTS)) * 100, digits = 1)
  m.pEFF <- round(.boinetc_ratio(Reduce("+", NEFF), Reduce("+", NPTS)) * 100, digits = 1)

  selected <- unlist(ODC, use.names = FALSE)
  tODC <- round(table(factor(selected, levels = 1:nd)) / ntrial * 100, digits = 1)
  pODC <- matrix(tODC, nrow = nd1)
  correct.pODC <- sum(pODC[tdose])
  target.PPTS <- sum(m.PPTS[tdose], na.rm = TRUE)

  totaln <- if (is.na(ncohort) || is.na(cohortsize)) max(SUM.N) else ncohort * cohortsize

  summary_file <- NA_character_
  if (isTRUE(write_summary)) {
    summary_file <- .boinetc_output_file(outdir, FILENAME, "_sum.txt")
    sink(summary_file, append = FALSE)
    on.exit({
      if (sink.number() > 0) {
        sink()
      }
    }, add = TRUE)
    cat("#################################################################\n")
    cat("# Simulation results of BOIN-ETC \n")
    cat("#################################################################\n\n\n")
    cat("#####################################\n")
    cat("# Settings \n")
    cat("#####################################\n")
    cat(paste("simulation times               = ", ntrial, "\n", sep = ""))
    cat(paste("number of max cohort           = ", ncohort, "\n", sep = ""))
    cat(paste("cohortsize                     = ", cohortsize, "\n", sep = ""))
    cat(paste("(target tox, target eff)       = (", phi, ",", delta, ")\n", sep = ""))
    cat(paste("#pats to move to next subtrial = ", n.es, "\n", sep = ""))
    cat(paste("(lambda1, lambda2, eta1)       = (", lambda1, ", ", lambda2, ", ", eta1, ")\n", sep = ""))
    cat("\n")
    cat(paste("scenario = ", SC, "\n", sep = ""))
    cat("true probability (tox) \n")
    print(list.p.truetox[[SC]])
    cat("true probability (eff) \n")
    print(list.p.trueeff[[SC]])
    cat("\n")
    cat("parameter for utility calculation \n")
    print(u)
    cat("\n\n")
    cat("#####################################\n")
    cat("# Results \n")
    cat("#####################################\n")
    cat("# mean number of patients by dose level \n")
    print(m.NPTS); cat("\n")
    cat("# percentage of number of patients treated by dose level \n")
    print(m.PPTS); cat("\n")
    cat("# mean number of tox by dose level \n")
    print(m.NTOX); cat("\n")
    cat("# mean number of eff by dose level \n")
    print(m.NEFF); cat("\n")
    cat("# observed P(tox) by dose level \n")
    print(m.pTOX); cat("\n")
    cat("# observed P(eff) by dose level \n")
    print(m.pEFF); cat("\n")
    cat("# percentage to select as ODC by dose level \n")
    print(pODC); cat("\n")
    cat("# percentage to not select ODC \n")
    print(round(non.ODC / ntrial, digits = 1)); cat("\n")
    cat("# percentage to select target dose correctly \n")
    print(correct.pODC); cat("\n")
    cat("# percentage of number of patients assigned to target doses \n")
    print(target.PPTS); cat("\n")
    cat("# mean number of patients \n")
    print(summary(SUM.N))
    sink()
  }

  out.sum1 <- data.frame(
    method = rep(method, nd1),
    totaln = rep(totaln, nd1),
    cohortsize = rep(cohortsize, nd1),
    delta = rep(delta, nd1),
    lambda1 = rep(lambda1, nd1),
    lambda2 = rep(lambda2, nd1),
    eta1 = rep(eta1, nd1),
    parameter_set = rep(as.integer(parameter_set), nd1),
    n.earlystop = rep(n.es, nd1),
    scenario = rep(SC, nd1),
    doseA = nd1:1
  )
  for (i in seq_len(nd2)) {
    out1 <- data.frame(rev(pODC[, i])); names(out1) <- paste("pODC", i, sep = "")
    out2 <- data.frame(rev(m.PPTS[, i])); names(out2) <- paste("m.PPTS", i, sep = "")
    out3 <- data.frame(rev(m.NPTS[, i])); names(out3) <- paste("m.NPTS", i, sep = "")
    out.sum1 <- cbind(out.sum1, out1, out2, out3)
  }

  out.sum2 <- data.frame(
    method = method,
    totaln = totaln,
    cohortsize = cohortsize,
    delta = delta,
    lambda1 = lambda1,
    lambda2 = lambda2,
    eta1 = eta1,
    parameter_set = as.integer(parameter_set),
    n.earlystop = n.es,
    scenario = SC,
    correct.pODC = correct.pODC,
    mean.npat = round(mean(SUM.N), digits = 1),
    sd.npat = round(stats::sd(SUM.N), digits = 2),
    min.npat = min(SUM.N),
    med.npat = round(stats::median(SUM.N), digits = 1),
    max.npat = max(SUM.N),
    target.PPTS = target.PPTS
  )

  list(
    out.sum1 = out.sum1,
    out.sum2 = out.sum2,
    pODC = pODC,
    m.NPTS = m.NPTS,
    m.NTOX = m.NTOX,
    m.NEFF = m.NEFF,
    m.PPTS = m.PPTS,
    m.pTOX = m.pTOX,
    m.pEFF = m.pEFF,
    SUM.N = SUM.N,
    summary_file = summary_file,
    input_file = infile
  )
}
