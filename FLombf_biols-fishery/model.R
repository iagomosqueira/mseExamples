# plesol.R - DESC
# /plesol.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

load('data/plesol.RData')

method(oem) <- perfect.oem

mseargs <- list(iy=2000, stock=1)

# control: perfect.sa + trend.hcr + seasonal.is

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # CCSBT trend HCR
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=0.85, nyears=5, metric=ssb))
  ))

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # CCSBT trend HCR
  hcr = mseCtrl(method=fixedF.hcr,
    args=list(ctrg=45000))
  ))

observations(oem) <- list(
  PLE=list(stk=observations(oem)$stk[[1]], idx=observations(oem)$idx[[1]]),
  SOL=list(stk=observations(oem)$stk[[1]], idx=observations(oem)$idx[[1]]))

# RUN mp

system.time(hind <- mp(om, oem=oem, ctrl=control, args=mseargs))


plot(biols(om(hind)))
plot(fisheries(om(hind))[[1]])

fbar(biols(om(hind))[[1]], fisheries(om(hind)))


# oem(FLom)

a <- FLom(stock=stock(om)[[1]])

obs <- perfect.oem(a, deviances=NULL, observations=NULL,
  args=list(y0=1957, dy=2020), tracking=FLQuant())

# oem(FLombf)

a <- FLombf(biols=biols(om)[1], fisheries=fisheries(om))



plot(obs$stk)
plot(obs$idx)

# hcr

dec <- trend.hcr(obs$stk[[1]], args=list(ay=2019, data_lag=1, management_lag=1, frq=1, it=1),
  tracking=FLQuant(), k1=1.5, k2=3, gamma=1, nyears=5, metric=ssb)

dec$ctrl

dec <- trend.hcr(obs$stk[[2]], args=list(ay=2019, data_lag=1, management_lag=1, frq=1, it=1),
  tracking=FLQuant(), k1=1.5, k2=3, gamma=1, nyears=5, metric=ssb)

dec$ctrl

# om

fut <- fwd(om, control=dec$ctrl)





ctrl <- fwdControl(
  lapply(2000:2010, function(x)
    list(year=x, quant="catch", value=50000, fishery=1, catch=1)
    ))

tes <- fwd(om, control=ctrl)

ctrl <- fwdControl(
    list(year=2000:2020, quant="catch", value=50000, fishery=1, catch=1),
    list(year=2000:2020, quant="catch", max=10000, biol=2)
)

tes <- fwd(om, control=ctrl)

catch(fisheries(tes), by='catch')
