#############################################################################
# BOINETC_sim_study.R: Code to conduct simulation of BOIN-ETC1, 2, 3 
#############################################################################

#############################################################################
# BOIN-ETC1
#############################################################################
for (kk in 1:length(delta)){
  for (ll in 1:length(c.n.earlystop)){
    aaa <- get.oc.BOINETC(
        ncohort=c.ncohort[ll], cohortsize=cohortsize, 
        n.earlystop=c.n.earlystop[ll], ntrial=ntrial,
        phi=phi, delta=delta[kk], 
        lambda1=lambda1[kk], lambda2=lambda2[kk], eta1=eta1[kk],
        u.min=u.min, n.scr = sc.list[[ll]],
        tdose=list.tdose[[sc.list[ll]]],
        pt.true.mat=list.p.truetox[[sc.list[ll]]], 
        pe.true.mat=list.p.trueeff[[sc.list[ll]]],
        filename=paste("BOINETC_m1.new.sc", sc.list[ll], "_cs", 
                       cohortsize, "_ns", c.n.earlystop[ll], sep=""))
  }
}

#############################################################################
# BOIN-ETC2
#############################################################################
for (kk in 1:length(delta)){
  for (ll in 1:length(c.n.earlystop)){
   aaa <- get.oc.BOINETC_m2(
      ncohort=c.ncohort[ll], cohortsize=cohortsize, 
      n.earlystop=c.n.earlystop[ll], ntrial=ntrial,
      phi=phi, delta=delta[kk], 
      lambda1=lambda1[kk], lambda2=lambda2[kk], eta1=eta1[kk],
      u.min=u.min, n.scr = sc.list[ll],
      tdose=list.tdose[[sc.list[ll]]],
      pt.true.mat=list.p.truetox[[sc.list[ll]]], 
      pe.true.mat=list.p.trueeff[[sc.list[ll]]],
      filename=paste("BOINETC_m2.new.sc", sc.list[ll], "_cs", 
                     cohortsize, "_ns", c.n.earlystop[ll], sep=""))
  }
}

#############################################################################
# BOIN-ETC3
#############################################################################
for (kk in 1:length(delta)){
  for (ll in 1:length(c.n.earlystop)){
    aaa <- get.oc.BOINETC_m3(
      ncohort=c.ncohort[ll], cohortsize=cohortsize, 
      n.earlystop=c.n.earlystop[ll], ntrial=ntrial,
      phi=phi, delta=delta[kk], 
      lambda1=lambda1[kk], lambda2=lambda2[kk], eta1=eta1[kk],
      u.min=u.min, n.scr = sc.list[ll],
      tdose=list.tdose[[sc.list[ll]]],
      pt.true.mat=list.p.truetox[[sc.list[ll]]], 
      pe.true.mat=list.p.trueeff[[sc.list[ll]]],
      filename=paste("BOINETC_m3.new.sc", sc.list[ll], "_cs", 
                     cohortsize, "_ns", c.n.earlystop[ll], sep=""))
  }
}

#############################################################################
# end of program
#############################################################################
