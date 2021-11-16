# condition.R - DESC
# /condition.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

# ICES SA datasets

load("data/ices_sas.RData")

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

rec(pleb) <- plesr
rec(solb) <- solsr

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

# --- TEST fwd(ple)

ctrl <- fwdControl(            
list(year=1958:2020, quant="catch", value=catch(plec)[,-1], fishery=1, catch=1)
)

hindp <- fwd(FLBiols(PLE=pleb, SOL=solb), bt, control=ctrl,
  deviances=FLQuants(PLE=exp(residuals(plesr)), SOL=exp(residuals(solsr))))

plot(catch(hindp$fisheries[[1]]), catch(plec))
plot(catch(hindp$fisheries[[2]]), catch(solc))

plot(
ssb(hindp$biols[[1]], catch.n=catch.n(hindp$fisheries[[1]])),
ssb(ple)
)

# FWD(sol)

ctrl <- fwdControl(            
list(year=1958:2020, quant="catch", value=catch(solc)[,-1], fishery=1, catch=2)
)

hinds <- fwd(FLBiols(PLE=pleb, SOL=solb), bt, control=ctrl,
  deviances=FLQuants(PLE=residuals(plesr), SOL=residuals(solsr)))

plot(hinds$biols)

plot(catch(hinds$fisheries[[1]]), catch(plec))
plot(catch(hinds$fisheries[[2]]), catch(solc))

effort(hinds$fisheries)

# refpts


# OM: biols + fishery + refpts

om <- FLombf(biols=FLBiols(PLE=pleb, SOL=solb), fisheries=FLFisheries(BT=bt),
  projection=mseCtrl(method=fwd.om))

# OEM

oem <- FLoem(method=perfect.oem,
  observations=list(stk=FLStocks(PLE=ple, SOL=sol))
)

# SAVE

save(om, oem, file="data/plesol.RData", compress="xz")
