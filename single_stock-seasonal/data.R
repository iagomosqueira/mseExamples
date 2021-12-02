# condition.R - DESC
# /condition.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)
library(ss3om)

its <- 200

# LOAD SS 3.30 run

alb <- readOMSss3('boot/albio')

# SIMPLIFY 'unit'

stock <- simplify(alb$stock, 'unit')

# sr

params <- params(alb$sr)
params$v <- params$v * 2
params$R0 <- params$R0 * 2

resds<- rlnorm(its, rec(stock) %=% 0, 0.8)

sr <- FLSR(model=bevholtss3, params=params, residuals=resds)

# refpts

refpts <- alb$refpts
refpts$SB0 <- refpts$SB0 * 2
refpts$R0 <- refpts$R0 * 2
refpts$SBMSY <- refpts$SBMSY * 2

# FLom with iters

om <- propagate(FLom(stock=stock, sr=sr, refpts=refpts,
  projection=mseCtrl(method=fwd.om)), its)

# SETUP OEM, annual stk and idx

stk <- propagate(simplify(alb$stock), its)
idx <- lapply(alb$indices[1:4], function(x) propagate(x[,,,1], its))

oem <- FLoem(observations=list(stk=stk, idx=idx),
  deviances=list(stk=list(catch.n=catch.n(stk) %=% 1)),
  method=sampling.oem)

# SAVE

save(om, oem, file="data/albio.RData", compress="xz")

