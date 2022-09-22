# data.R - CONDITION a simple OM
# mseExamples/single_stock-model_free/condition.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

set.seed(189)

# SET dimensions (iters, final year)

its <- 100
fy <- 2042

# LOAD data: ICES WGNSSK SOL 27.4 AAP SA results

load("boot/sol2740.Rdata")

plot(stk)

# GET stock-recruits relationship

library(FLSRTMB)

srr <- srrTMB(as.FLSR(stk, model=bevholtSV), spr0=spr0y(stk))

# FUTURE rec deviances: AR1 LN, rho=0.43, sdlog=0.5

devs <- ar1rlnorm(rho=0.43, years=seq(2021, fy), iters=its, meanlog=0, sdlog=0.5)

residuals(srr) <- append(exp(residuals(srr)), devs)

# EXTEND stk to fy & PROPAGATE for its

stk <- propagate(fwdWindow(stk, end=fy), its)

# --- OM: biols + fishery + refpts

om <- FLom(stock=stk, sr=srr, refpts=rps, projection=mseCtrl(method=fwd.om))

# --- OEM

# observations

index.q(indices) <- computeQ(indices, stk, index(indices))

obs <- list(stk=stk,
  idx=lapply(indices, function(x) propagate(fwdWindow(x, end=fy), its)))

# deviances

devs <- list(stk=FLQuants(catch.n=rlnorm(its, 0, catch.n(obs$stk) %=% 0.2)),
  idx=FLQuants(BTS=rlnorm(its, 0, index.q(obs$idx[[1]]) %=% 0.2),
    SNS=rlnorm(its, 0, index.q(obs$idx[[2]]) %=% 0.2)))

ggplot(FLQuants(c(devs$stk, devs$idx)),
  aes(x=year, y=data, fill=factor(age))) + 
  geom_hline(yintercept=1, linetype=2) +
  geom_flquantiles() + facet_grid(age~qname)

# sampling.oem

oem <- FLoem(method=sampling.oem, observations=obs, deviances=devs)

# SAVE

save(om, oem, file="data/sol2740_om.Rdata", compress="xz")
