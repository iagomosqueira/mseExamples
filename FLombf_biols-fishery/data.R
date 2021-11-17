# condition.R - DESC
# /condition.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

# ICES SA datasets

load("boot/ices_sas.RData")

plot(ple, sol)

# COERCE to FLBiol

pleb <- as(ple, 'FLBiol')

solb <- as(sol, 'FLBiol')

plot(FLBiols(ple=pleb, sol=solb))

# ADD stock-recruits relationship

library(FLSRTMB)

plesr <- srrTMB(as.FLSR(ple, model=bevholtSV), spr0=spr0y(ple))
solsr <- srrTMB(as.FLSR(sol, model=bevholtSV), spr0=spr0y(sol))

plot(plesr)
plot(solsr)

rec(pleb) <- predictModel(FLQuants(residuals=exp(residuals(plesr))),
  model=bevholt()$model, params=params(plesr))

rec(solb) <- predictModel(FLQuants(residuals=exp(residuals(solsr))),
  model=bevholt()$model, params=params(solsr))

plot(FLQuants(PLE=exp(residuals(plesr)), SOL=exp(residuals(solsr)))) +
  geom_hline(yintercept=1, linetype=2)

# CONSTRUCT FLFishery

plec <- as(ple, "FLCatch")
solc <- as(sol, "FLCatch")

# SET effort series

# F = effort * selectivity * alpha * biomass ^ -beta

effp <- (harvest(ple) / catch.sel(ple))[1,]
effs <- (harvest(sol) / catch.sel(sol))[1,]

eff <- (effp + effs) / 2
dimnames(eff) <- list(effort='all')

bt <- FLFishery(effort=eff, PLE=plec, SOL=solc)

# OM: biols + fishery + refpts

om <- FLombf(biols=FLBiols(PLE=pleb, SOL=solb), fisheries=FLFisheries(BT=bt),
  projection=mseCtrl(method=fwd.om))

om <- fwdWindow(om, end=2040)

# OEM

oem <- FLoem(method=perfect.oem,
  observations=list(PLE=list(stk=stf(ple, end=2040)),
  SOL=list(stk=stf(ple, end=2040)))
)

# SAVE

save(om, oem, file="data/plesol.RData", compress="xz")

