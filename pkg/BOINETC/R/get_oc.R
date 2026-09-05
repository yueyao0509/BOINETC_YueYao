get.oc.BOINETC <- function(ncohort, cohortsize, n.earlystop, ntrial,
                           phi, delta, lambda1, lambda2, eta1,
                           tdose = NULL, u.min = NULL, n.scr = NULL,
                           pt.true.mat, pe.true.mat, filename,
                           outdir = NULL) {
  outdir <- .boinetc_resolve_outdir(outdir, parent.frame())

  nd1 <- nrow(pt.true.mat)
  nd2 <- ncol(pt.true.mat)
  nd <- nd1 * nd2

  OUT.STUDY <- rbind(rep(rep(seq_len(nd1), nd2), 14),
                     rep(rep(seq_len(nd2), each = nd1), 14))
  colnames(OUT.STUDY) <- paste(rep(c("npts", "ntox", "neff",
                                     "ppts", "ptox", "peff",
                                     "nct1", "nct2", "nct3", "nct4",
                                     "pct1", "pct2", "pct3", "pct4"),
                                   each = nd),
                               rep(rep(seq_len(nd1), nd2), 14),
                               rep(rep(seq_len(nd2), each = nd1), 14), sep = "")

  dosespace <- list(c(seq_len(nd1), (2:nd2) * nd1))
  for (j in (nd1 - 1):1) {
    dosespace <- append(dosespace, list(1:(nd2 - 1) * nd1 + j))
  }

  for (jjj in seq_len(ntrial)) {
    ntox <- neff <- npts <- elimi <- matrix(0, nrow = nd1, ncol = nd2)
    nct1 <- nct2 <- nct3 <- nct4 <- matrix(0, nrow = nd1, ncol = nd2)
    sum.n <- 0
    odc <- numeric(length(dosespace))
    dspace <- 1
    startdose <- 1

    for (jj in seq_len(nd1)) {
      pt.true <- as.vector(pt.true.mat)[dosespace[[dspace]]]
      pe.true <- as.vector(pe.true.mat)[dosespace[[dspace]]]

      sub <- BOINETC.subtrial(pt.true, pe.true, dosespace[[dspace]],
                              npts, ntox, neff, nct1, nct2, nct3, nct4,
                              elimi, ncohort, cohortsize,
                              phi, delta, lambda1, lambda2, eta1,
                              n.earlystop, startdose,
                              cutoff.eli = 0.95, extrasafe = FALSE,
                              offset = 0.05,
                              sum.nn = ncohort * cohortsize - sum.n,
                              titration.first.trial = FALSE)

      odc[jj] <- sub$selectdose
      npts <- sub$npts
      ntox <- sub$ntox
      neff <- sub$neff
      nct1 <- sub$nct1
      nct2 <- sub$nct2
      nct3 <- sub$nct3
      nct4 <- sub$nct4
      sum.n <- sub$sum.n
      elimi <- sub$elimi
      l.selectdose <- sub$l.selectdose

      if (jj == 1) {
        if (l.selectdose == 1) {
          dspace <- nd1
          startdose <- 1
        } else if (l.selectdose <= nd1) {
          dspace <- nd1 - l.selectdose + 2
          startdose <- 1
        } else {
          dspace <- 2
          startdose <- l.selectdose - nd1
        }
      } else {
        dspace <- dspace + 1
        startdose <- l.selectdose
      }

      if (dspace > nd1) {
        break
      }
      if (sub$sum.n >= ncohort * cohortsize) {
        break
      }
    }

    ppts <- .boinetc_ratio(npts, sum(npts))
    ptox <- .boinetc_ratio(ntox, npts)
    peff <- .boinetc_ratio(neff, npts)
    pct1 <- .boinetc_ratio(nct1, npts)
    pct2 <- .boinetc_ratio(nct2, npts)
    pct3 <- .boinetc_ratio(nct3, npts)
    pct4 <- .boinetc_ratio(nct4, npts)

    OUT.STUDY <- rbind(OUT.STUDY, c(as.vector(npts),
                                    as.vector(ntox),
                                    as.vector(neff),
                                    as.vector(ppts),
                                    as.vector(ptox),
                                    as.vector(peff),
                                    as.vector(nct1),
                                    as.vector(nct2),
                                    as.vector(nct3),
                                    as.vector(nct4),
                                    as.vector(pct1),
                                    as.vector(pct2),
                                    as.vector(pct3),
                                    as.vector(pct4)))
  }

  outfile <- .boinetc_output_file(outdir, filename, "_data.csv")
  utils::write.csv(x = OUT.STUDY, file = outfile)
  invisible(list(data = OUT.STUDY, file = outfile, method = "BOINETC_m1"))
}

get.oc.BOINETC_m2 <- function(ncohort, cohortsize, n.earlystop, ntrial,
                              phi, delta, lambda1, lambda2, eta1,
                              tdose = NULL, u.min = NULL, n.scr = NULL,
                              pt.true.mat, pe.true.mat, filename,
                              outdir = NULL) {
  outdir <- .boinetc_resolve_outdir(outdir, parent.frame())

  nd1 <- nrow(pt.true.mat)
  nd2 <- ncol(pt.true.mat)
  nd <- nd1 * nd2

  OUT.STUDY <- rbind(rep(rep(seq_len(nd1), nd2), 14),
                     rep(rep(seq_len(nd2), each = nd1), 14))
  colnames(OUT.STUDY) <- paste(rep(c("npts", "ntox", "neff",
                                     "ppts", "ptox", "peff",
                                     "nct1", "nct2", "nct3", "nct4",
                                     "pct1", "pct2", "pct3", "pct4"),
                                   each = nd),
                               rep(rep(seq_len(nd1), nd2), 14),
                               rep(rep(seq_len(nd2), each = nd1), 14), sep = "")

  dosespace <- list(c(seq_len(nd1), (2:nd2) * nd1))
  for (j in (nd1 - 1):1) {
    dosespace <- append(dosespace, list(1:(nd2 - 1) * nd1 + j))
  }

  for (jjj in seq_len(ntrial)) {
    ntox <- neff <- npts <- elimi <- matrix(0, nrow = nd1, ncol = nd2)
    nct1 <- nct2 <- nct3 <- nct4 <- matrix(0, nrow = nd1, ncol = nd2)
    sum.n <- 0
    odc <- numeric(length(dosespace))
    dspace <- 1

    for (jj in seq_len(nd1)) {
      pt.true <- as.vector(pt.true.mat)[dosespace[[dspace]]]
      pe.true <- as.vector(pe.true.mat)[dosespace[[dspace]]]

      sub <- BOINETC.subtrial(pt.true, pe.true, dosespace[[dspace]],
                              npts, ntox, neff, nct1, nct2, nct3, nct4,
                              elimi, ncohort, cohortsize,
                              phi, delta, lambda1, lambda2, eta1,
                              n.earlystop, startdose = 1,
                              cutoff.eli = 0.95, extrasafe = FALSE,
                              offset = 0.05,
                              sum.nn = ncohort * cohortsize - sum.n,
                              titration.first.trial = FALSE)

      odc[jj] <- sub$selectdose
      npts <- sub$npts
      ntox <- sub$ntox
      neff <- sub$neff
      nct1 <- sub$nct1
      nct2 <- sub$nct2
      nct3 <- sub$nct3
      nct4 <- sub$nct4
      sum.n <- sub$sum.n
      elimi <- sub$elimi
      l.selectdose <- sub$l.selectdose

      if (jj == 1) {
        if (l.selectdose == 1) {
          dspace <- nd1
        } else if (l.selectdose <= nd1) {
          dspace <- nd1 - l.selectdose + 2
        } else {
          dspace <- dspace + 1
        }
      } else {
        dspace <- dspace + 1
      }

      if (dspace > nd1) {
        break
      }
      if (sub$sum.n >= ncohort * cohortsize) {
        break
      }
    }

    ppts <- .boinetc_ratio(npts, sum(npts))
    ptox <- .boinetc_ratio(ntox, npts)
    peff <- .boinetc_ratio(neff, npts)
    pct1 <- .boinetc_ratio(nct1, npts)
    pct2 <- .boinetc_ratio(nct2, npts)
    pct3 <- .boinetc_ratio(nct3, npts)
    pct4 <- .boinetc_ratio(nct4, npts)

    OUT.STUDY <- rbind(OUT.STUDY, c(as.vector(npts),
                                    as.vector(ntox),
                                    as.vector(neff),
                                    as.vector(ppts),
                                    as.vector(ptox),
                                    as.vector(peff),
                                    as.vector(nct1),
                                    as.vector(nct2),
                                    as.vector(nct3),
                                    as.vector(nct4),
                                    as.vector(pct1),
                                    as.vector(pct2),
                                    as.vector(pct3),
                                    as.vector(pct4)))
  }

  outfile <- .boinetc_output_file(outdir, filename, "_data.csv")
  utils::write.csv(x = OUT.STUDY, file = outfile)
  invisible(list(data = OUT.STUDY, file = outfile, method = "BOINETC_m2"))
}

get.oc.BOINETC_m3 <- function(ncohort, cohortsize, n.earlystop, ntrial,
                              phi, delta, lambda1, lambda2, eta1,
                              tdose = NULL, u.min = NULL, n.scr = NULL,
                              pt.true.mat, pe.true.mat, filename,
                              outdir = NULL) {
  outdir <- .boinetc_resolve_outdir(outdir, parent.frame())

  nd1 <- nrow(pt.true.mat)
  nd2 <- ncol(pt.true.mat)
  nd <- nd1 * nd2

  OUT.STUDY <- rbind(rep(rep(seq_len(nd1), nd2), 14),
                     rep(rep(seq_len(nd2), each = nd1), 14))
  colnames(OUT.STUDY) <- paste(rep(c("npts", "ntox", "neff",
                                     "ppts", "ptox", "peff",
                                     "nct1", "nct2", "nct3", "nct4",
                                     "pct1", "pct2", "pct3", "pct4"),
                                   each = nd),
                               rep(rep(seq_len(nd1), nd2), 14),
                               rep(rep(seq_len(nd2), each = nd1), 14), sep = "")

  dosespace <- list(c(seq_len(nd1), (2:nd2) * nd1))
  for (j in 1:(nd1 - 1)) {
    if (nd1 > j + 1) {
      dosespace <- append(dosespace, list(c(j * nd1 + 1:(nd1 - j), ((j + 2):nd2) * nd1 - j)))
    } else {
      dosespace <- append(dosespace, list(nd1 * (j:(nd2 - 1)) + 1))
    }
  }

  for (jjj in seq_len(ntrial)) {
    ntox <- neff <- npts <- elimi <- matrix(0, nrow = nd1, ncol = nd2)
    nct1 <- nct2 <- nct3 <- nct4 <- matrix(0, nrow = nd1, ncol = nd2)
    sum.n <- 0
    odc <- numeric(length(dosespace))
    dspace <- 1

    for (jj in seq_len(nd1)) {
      pt.true <- as.vector(pt.true.mat)[dosespace[[dspace]]]
      pe.true <- as.vector(pe.true.mat)[dosespace[[dspace]]]

      sub <- BOINETC.subtrial(pt.true, pe.true, dosespace[[dspace]],
                              npts, ntox, neff, nct1, nct2, nct3, nct4,
                              elimi, ncohort, cohortsize,
                              phi, delta, lambda1, lambda2, eta1,
                              n.earlystop, startdose = 1,
                              cutoff.eli = 0.95, extrasafe = FALSE,
                              offset = 0.05,
                              sum.nn = ncohort * cohortsize - sum.n,
                              titration.first.trial = FALSE)

      odc[jj] <- sub$selectdose
      npts <- sub$npts
      ntox <- sub$ntox
      neff <- sub$neff
      nct1 <- sub$nct1
      nct2 <- sub$nct2
      nct3 <- sub$nct3
      nct4 <- sub$nct4
      sum.n <- sub$sum.n
      elimi <- sub$elimi
      l.selectdose <- sub$l.selectdose

      dspace <- dspace + 1
      if (l.selectdose == 1) {
        startdose <- 1
      } else if (l.selectdose <= nd1 - jj + 1) {
        startdose <- l.selectdose - 1
      } else {
        startdose <- l.selectdose - (nd1 - jj + 1)
      }

      if (dspace > nd1) {
        break
      }
      if (sub$sum.n >= ncohort * cohortsize) {
        break
      }
    }

    ppts <- .boinetc_ratio(npts, sum(npts))
    ptox <- .boinetc_ratio(ntox, npts)
    peff <- .boinetc_ratio(neff, npts)
    pct1 <- .boinetc_ratio(nct1, npts)
    pct2 <- .boinetc_ratio(nct2, npts)
    pct3 <- .boinetc_ratio(nct3, npts)
    pct4 <- .boinetc_ratio(nct4, npts)

    OUT.STUDY <- rbind(OUT.STUDY, c(as.vector(npts),
                                    as.vector(ntox),
                                    as.vector(neff),
                                    as.vector(ppts),
                                    as.vector(ptox),
                                    as.vector(peff),
                                    as.vector(nct1),
                                    as.vector(nct2),
                                    as.vector(nct3),
                                    as.vector(nct4),
                                    as.vector(pct1),
                                    as.vector(pct2),
                                    as.vector(pct3),
                                    as.vector(pct4)))
  }

  outfile <- .boinetc_output_file(outdir, filename, "_data.csv")
  utils::write.csv(x = OUT.STUDY, file = outfile)
  invisible(list(data = OUT.STUDY, file = outfile, method = "BOINETC_m3"))
}
