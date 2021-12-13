# condition.R - DESC
# /condition.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

its <- 100

# ICES SA datasets

load("boot/sol2740.Rdata")

plot(sol)

# GET stock-recruits relationship

library(FLSRTMB)

solsr <- srrTMB(as.FLSR(sol, model=bevholtSV), spr0=spr0y(sol))

plot(solsr)

# FUTURE rec deviances

devs <- ar1rlnorm(rho=0.43, years=2021:2040, iters=its, meanlog=0, sdlog=0.5)

residuals(solsr) <- append(propagate(exp(residuals(solsr)), its), devs)

# OM: biols + fishery + refpts

om <- FLom(stock=sol, sr=solsr, refpts=rps, projection=mseCtrl(method=fwd.om))

om <- propagate(fwdWindow(om, end=2040), its)

# OEM

oem <- FLoem(method=sampling.oem,
  observations=list(stk=propagate(fwdWindow(sol, end=2040), its),
    idx=lapply(indices, function(x) propagate(fwdWindow(x, end=2040), its))),
  deviances=list(catch.n=rlnorm(its, 0, catch.n(stock(om)) %=% 0.2))
)

# catch.n
# catch.wt
# maturity

# index.q

# SAVE

save(om, oem, file="data/sol2740.Rdata", compress="xz")
