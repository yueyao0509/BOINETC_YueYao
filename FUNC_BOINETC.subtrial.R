version#############################################################################
# BOINETC.subtrial.R: function to select next dose in subtrial
#############################################################################
BOINETC.subtrial <- function(pt.true, pe.true, dosespace,
                             npts, ntox, neff, 
                             nct1, nct2, nct3, nct4, 
                             elimi, ncohort, cohortsize, 
                             phi, delta, lambda1, lambda2, eta1, 
                             n.earlystop = 20, startdose = 1, 
                             cutoff.eli = 0.95, extrasafe = FALSE, 
                             offset = 0.05,
                             sum.nn, titration.first.trial = FALSE){

  ndose <- length(dosespace)
  y.t <- y.e <- y.c1 <- y.c2 <- y.c3 <- y.c4 <- n <- elm <- rep(0, ndose)
  estop <- 0
  d <- startdose
  for (icohort in 1:ncohort){
    if (titration.first.trial & n[d] < cohortsize) {
      nnn = cohortsize-1;
    }
    else{
      nnn = cohortsize;
    }
    r.y.t   = runif(nnn) < pt.true[d];
    r.y.e   = runif(nnn) < pe.true[d];
    y.t[d]  = y.t[d] + sum(r.y.t); # tox
    y.e[d]  = y.e[d] + sum(r.y.e); # eff
    y.c1[d] = y.c1[d] + sum(r.y.t == 0 & r.y.e == 1);
    y.c2[d] = y.c2[d] + sum(r.y.t == 0 & r.y.e == 0);
    y.c3[d] = y.c3[d] + sum(r.y.t == 1 & r.y.e == 1);
    y.c4[d] = y.c4[d] + sum(r.y.t == 1 & r.y.e == 0);
    n[d]    = n[d]   + nnn;
    
    if (pbeta(phi, y.t[d]+1, n[d]-y.t[d]+1) < 0.05){elm[d] = 1}
    
    if (d==1){
      dd = c(d, d+1)
      p.e.list = c(y.e[d]/n[d], y.e[d+1]/n[d+1])
      elm.list = c(elm[d], elm[d+1])
    }
    else if (d==ndose){
      dd = c(d-1, d)
      p.e.list = c(y.e[d-1]/n[d-1], y.e[d]/n[d])
      elm.list = c(elm[d-1], elm[d])
    }
    else {
      dd = c(d-1, d, d+1)
      p.e.list = c(y.e[d-1]/n[d-1], y.e[d]/n[d], y.e[d+1]/n[d+1])
      elm.list = c(elm[d-1], elm[d], elm[d+1])
    }
    p.t = y.t[d]/n[d]; p.e = y.e[d]/n[d]
    dd.elig   = dd[which(elm.list == 0)]
    if (length(dd.elig)==0){estop=1; break}
    p.e.elig  = p.e.list[which(elm.list == 0)]

    if (elm[d+1] == 0 && p.t < lambda1 && p.e < eta1 && d != ndose) {d = d+1}
    else if (p.t >= lambda2 && d != 1) {d = d-1}
    else if (d>1 && elm[d]==1 && elm[d-1]==0 && n[d-1]==0){d = d-1}
    else if (lambda1 < p.t && p.t < lambda2 && p.e < eta1){
      if (d != ndose && n[d+1] == 0){
        d = d+1
      }
      else {
        dd.elig = dd.elig[!is.na(dd.elig)]
        p.e.elig = p.e.elig[!is.na(p.e.elig)]
        dd.eligx = dd.elig[which(p.e.elig == max(p.e.elig))]
        nn       = length(dd.eligx)
        if (nn==1){d = dd.eligx}
        else {d = dd.elig[ceiling(runif(1)*nn)]}
      }
    }

    if (sum(n) >= sum.nn) {break}
    if (n[d] >= n.earlystop) {break}
  }
  if (estop==1){selectdose = 99}
  else {selectdose = dosespace[d]}
  npts[dosespace] = n
  ntox[dosespace] = y.t
  neff[dosespace] = y.e
  nct1[dosespace] = y.c1
  nct2[dosespace] = y.c2
  nct3[dosespace] = y.c3
  nct4[dosespace] = y.c4
  elimi[dosespace] = elm
  list(ncohort = icohort, ntotal = icohort*cohortsize,
       startdose = startdose, npts = npts, ntox = ntox, neff = neff,
       nct1 = nct1, nct2 = nct2, nct3 = nct3, nct4 = nct4,
       sum.tox = sum(ntox), sum.eff = sum(neff), sum.n = sum(npts), 
       estop = estop, 
       selectdose = selectdose,
       l.selectdose = d,
       elimi = elimi)
  
}

#############################################################################
# End of program
#############################################################################


#############################################################################
# Modified subtrial rule used by BOIN-ETC2 and BOIN-ETC3 only.
# BOIN-ETC1 above is kept as the original baseline.
#############################################################################
BOINETC.subtrial.improved <- function(pt.true, pe.true, dosespace,
                                 npts, ntox, neff,
                                 nct1, nct2, nct3, nct4,
                                 elimi, ncohort, cohortsize,
                                 phi, delta, lambda1, lambda2, eta1,
                                 n.earlystop = 20, startdose = 1,
                                 cutoff.eli = 0.95, extrasafe = FALSE,
                                 offset = 0.05,
                                 sum.nn, titration.first.trial = FALSE,
                                 util.weight = c(100, 25, 75, 0),
                                 explore.weight = 0.060,
                                 tox.penalty.weight = 1.25,
                                 eff.weight = 0.15){

  ndose <- length(dosespace)
  y.t <- y.e <- y.c1 <- y.c2 <- y.c3 <- y.c4 <- n <- elm <- rep(0, ndose)
  estop <- 0
  d <- max(1, min(startdose, ndose))

  choose_best_local <- function(cand){
    cand <- cand[!is.na(cand) & cand >= 1 & cand <= ndose & elm[cand] == 0]
    if (length(cand) == 0) return(NA_integer_)

    # Posterior means with mild shrinkage; avoids unstable 0/0 decisions.
    tox.hat <- (y.t[cand] + 0.5)/(n[cand] + 1)
    eff.hat <- (y.e[cand] + 0.5)/(n[cand] + 1)

    # Category posterior expected utility. Category 1 = no tox + eff is best.
    a.cat <- 0.25
    u.raw <- (util.weight[1]*(y.c1[cand] + a.cat) +
              util.weight[2]*(y.c2[cand] + a.cat) +
              util.weight[3]*(y.c3[cand] + a.cat) +
              util.weight[4]*(y.c4[cand] + a.cat))/
             (100*(n[cand] + 4*a.cat))

    # Exploration bonus deliberately gives under-sampled neighboring doses a chance.
    explore <- explore.weight * sqrt(log(sum(n) + 2)/(n[cand] + 1))

    # Penalize posterior toxicity above phi; reward efficacy mildly so method 2/3
    # do not stay too conservative in Scenario 4/6, where target doses are interior.
    tox.penalty <- tox.penalty.weight * pmax(0, tox.hat - phi)
    score <- u.raw + eff.weight*eff.hat + explore - tox.penalty

    cand[which.max(score)]
  }

  for (icohort in 1:ncohort){
    if (titration.first.trial & n[d] < cohortsize) {
      nnn <- cohortsize - 1
    } else {
      nnn <- cohortsize
    }

    r.y.t   <- runif(nnn) < pt.true[d]
    r.y.e   <- runif(nnn) < pe.true[d]
    y.t[d]  <- y.t[d] + sum(r.y.t)
    y.e[d]  <- y.e[d] + sum(r.y.e)
    y.c1[d] <- y.c1[d] + sum(r.y.t == 0 & r.y.e == 1)
    y.c2[d] <- y.c2[d] + sum(r.y.t == 0 & r.y.e == 0)
    y.c3[d] <- y.c3[d] + sum(r.y.t == 1 & r.y.e == 1)
    y.c4[d] <- y.c4[d] + sum(r.y.t == 1 & r.y.e == 0)
    n[d]    <- n[d]   + nnn

    # Toxicity elimination: posterior Pr(p_tox > phi) > cutoff.
    if ((1 - pbeta(phi, y.t[d] + 1, n[d] - y.t[d] + 1)) > cutoff.eli){
      elm[d] <- 1
    }

    p.t <- (y.t[d] + 0.5)/(n[d] + 1)
    p.e <- (y.e[d] + 0.5)/(n[d] + 1)

    if (d == 1){
      cand <- c(d, d + 1)
    } else if (d == ndose){
      cand <- c(d - 1, d)
    } else {
      cand <- c(d - 1, d, d + 1)
    }
    cand <- cand[cand >= 1 & cand <= ndose]
    cand.elig <- cand[elm[cand] == 0]
    if (length(cand.elig) == 0){estop <- 1; break}

    # Modified rule:
    # - de-escalate if current posterior tox is clearly high;
    # - escalate if current dose is safe but weakly efficacious;
    # - otherwise choose the best local dose by posterior utility/exploration score.
    if (elm[d] == 1 && d > 1 && elm[d - 1] == 0){
      d <- d - 1
    } else if (p.t >= lambda2 && d > 1){
      lower <- cand.elig[cand.elig < d]
      if (length(lower) > 0) d <- max(lower) else d <- choose_best_local(cand.elig)
    } else if (p.t < lambda1 && p.e < eta1 && d < ndose && elm[d + 1] == 0){
      d <- d + 1
    } else {
      d.new <- choose_best_local(cand.elig)
      if (!is.na(d.new)) d <- d.new
    }

    if (sum(n) >= sum.nn) {break}
    if (n[d] >= n.earlystop) {break}
  }

  if (estop == 1){selectdose <- 99} else {selectdose <- dosespace[d]}
  npts[dosespace] <- n
  ntox[dosespace] <- y.t
  neff[dosespace] <- y.e
  nct1[dosespace] <- y.c1
  nct2[dosespace] <- y.c2
  nct3[dosespace] <- y.c3
  nct4[dosespace] <- y.c4
  elimi[dosespace] <- elm

  list(ncohort = icohort, ntotal = icohort*cohortsize,
       startdose = startdose, npts = npts, ntox = ntox, neff = neff,
       nct1 = nct1, nct2 = nct2, nct3 = nct3, nct4 = nct4,
       sum.tox = sum(ntox), sum.eff = sum(neff), sum.n = sum(npts),
       estop = estop,
       selectdose = selectdose,
       l.selectdose = d,
       elimi = elimi)
}


