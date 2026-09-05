BOINETC.subtrial <- function(pt.true, pe.true, dosespace,
                             npts, ntox, neff,
                             nct1, nct2, nct3, nct4,
                             elimi, ncohort, cohortsize,
                             phi, delta, lambda1, lambda2, eta1,
                             n.earlystop = 20, startdose = 1,
                             cutoff.eli = 0.95, extrasafe = FALSE,
                             offset = 0.05,
                             sum.nn, titration.first.trial = FALSE) {
  ndose <- length(dosespace)
  y.t <- y.e <- y.c1 <- y.c2 <- y.c3 <- y.c4 <- n <- elm <- rep(0, ndose)
  estop <- 0
  d <- startdose

  for (icohort in seq_len(ncohort)) {
    if (titration.first.trial && n[d] < cohortsize) {
      nnn <- cohortsize - 1
    } else {
      nnn <- cohortsize
    }

    r.y.t <- stats::runif(nnn) < pt.true[d]
    r.y.e <- stats::runif(nnn) < pe.true[d]
    y.t[d]  <- y.t[d]  + sum(r.y.t)
    y.e[d]  <- y.e[d]  + sum(r.y.e)
    y.c1[d] <- y.c1[d] + sum(r.y.t == 0 & r.y.e == 1)
    y.c2[d] <- y.c2[d] + sum(r.y.t == 0 & r.y.e == 0)
    y.c3[d] <- y.c3[d] + sum(r.y.t == 1 & r.y.e == 1)
    y.c4[d] <- y.c4[d] + sum(r.y.t == 1 & r.y.e == 0)
    n[d]    <- n[d]    + nnn

    if (stats::pbeta(phi, y.t[d] + 1, n[d] - y.t[d] + 1) < 0.05) {
      elm[d] <- 1
    }

    if (d == 1) {
      dd <- c(d, d + 1)
      p.e.list <- c(y.e[d] / n[d], y.e[d + 1] / n[d + 1])
      elm.list <- c(elm[d], elm[d + 1])
    } else if (d == ndose) {
      dd <- c(d - 1, d)
      p.e.list <- c(y.e[d - 1] / n[d - 1], y.e[d] / n[d])
      elm.list <- c(elm[d - 1], elm[d])
    } else {
      dd <- c(d - 1, d, d + 1)
      p.e.list <- c(y.e[d - 1] / n[d - 1], y.e[d] / n[d], y.e[d + 1] / n[d + 1])
      elm.list <- c(elm[d - 1], elm[d], elm[d + 1])
    }

    p.t <- y.t[d] / n[d]
    p.e <- y.e[d] / n[d]
    dd.elig <- dd[which(elm.list == 0)]
    if (length(dd.elig) == 0) {
      estop <- 1
      break
    }
    p.e.elig <- p.e.list[which(elm.list == 0)]

    if (elm[d + 1] == 0 && p.t < lambda1 && p.e < eta1 && d != ndose) {
      d <- d + 1
    } else if (p.t >= lambda2 && d != 1) {
      d <- d - 1
    } else if (d > 1 && elm[d] == 1 && elm[d - 1] == 0 && n[d - 1] == 0) {
      d <- d - 1
    } else if (lambda1 < p.t && p.t < lambda2 && p.e < eta1) {
      if (d != ndose && n[d + 1] == 0) {
        d <- d + 1
      } else {
        dd.elig <- dd.elig[!is.na(dd.elig)]
        p.e.elig <- p.e.elig[!is.na(p.e.elig)]
        dd.eligx <- dd.elig[which(p.e.elig == max(p.e.elig))]
        nn <- length(dd.eligx)
        if (nn == 1) {
          d <- dd.eligx
        } else {
          d <- dd.elig[ceiling(stats::runif(1) * nn)]
        }
      }
    }

    if (sum(n) >= sum.nn) {
      break
    }
    if (n[d] >= n.earlystop) {
      break
    }
  }

  if (estop == 1) {
    selectdose <- 99
  } else {
    selectdose <- dosespace[d]
  }

  npts[dosespace] <- n
  ntox[dosespace] <- y.t
  neff[dosespace] <- y.e
  nct1[dosespace] <- y.c1
  nct2[dosespace] <- y.c2
  nct3[dosespace] <- y.c3
  nct4[dosespace] <- y.c4
  elimi[dosespace] <- elm

  list(
    ncohort = icohort,
    ntotal = icohort * cohortsize,
    startdose = startdose,
    npts = npts,
    ntox = ntox,
    neff = neff,
    nct1 = nct1,
    nct2 = nct2,
    nct3 = nct3,
    nct4 = nct4,
    sum.tox = sum(ntox),
    sum.eff = sum(neff),
    sum.n = sum(npts),
    estop = estop,
    selectdose = selectdose,
    l.selectdose = d,
    elimi = elimi
  )
}
