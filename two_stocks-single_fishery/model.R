# plesol.R - DESC
# /plesol.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# NOTE - Example currently runs on mse listModules branch, install using:
# remotes::install_github("flr/FLFishery")
# remotes::install_github("flr/mse", ref="listModules")


library(mse)

load('data/plesol.RData')

# SET intermediate year and stock on which hcr applies

mseargs <- list(iy=2020, stock=1)

# control: perfect.sa + trend.hcr

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # CCSBT trend HCR
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=0.95, nyears=5, metric=ssb))
  ))

# control: perfect.sa + ices.hcr + tac.is

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # ICES HCR
  hcr = mseCtrl(method=ices.hcr,
    args=list(ftrg=0.21, sblim=0.42, sbsafe=350000, minfbar=1, maxfbar=6)),
  # One-year forecast for TAC
  isys=mseCtrl(method=tac.is, args=list(dtaclow=0.85, dtacupp=1.15, recyrs=30))
  ))

# control: xsa.sa + ices.hcr + tac.is

library(FLXSA)

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=xsa.sa),
  # ICES HCR
  hcr = mseCtrl(method=ices.hcr,
    args=list(ftrg=0.21, sblim=0.42, sbsafe=350000, minfbar=1, maxfbar=6)),
  # One-year forecast for TAC
  isys=mseCtrl(method=tac.is, args=list(dtaclow=0.85, dtacupp=1.15, recyrs=30))
  ))

# control: aap.sa + ices.hcr + tac.is

library(AAP)

mseargs <- list(iy=2020, stock=2)

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=aap.sa, args=list(control=AAP.control(pGrp=TRUE, qplat.surveys=8,
    qplat.Fmatrix=9, Sage.knots=6, Fage.knots=8, Ftime.knots=28, mcmc=FALSE))),
  # ICES HCR
  hcr = mseCtrl(method=ices.hcr,
    args=list(ftrg=0.21, sblim=0.42, sbsafe=350000, minfbar=1, maxfbar=6)),
  # One-year forecast for TAC
  isys=mseCtrl(method=tac.is, args=list(dtaclow=0.85, dtacupp=1.15, recyrs=30))
  ))


# RUN mp

system.time(tes <- mp(om, oem=oem, ctrl=control, args=mseargs))

# PLOT TODO REWRITE plot(FLombf, FLmse)

plot(biols(om(tes)))
plot(fisheries(om(tes))[[1]])

# Fbar

plot(FLQuants(
  PLE=fbar(biols(om(tes))[[1]], fisheries(om(tes)), minfbar=1, maxfbar=6, fcb=1),
  SOL=fbar(biols(om(tes))[[2]], fisheries(om(tes))[[1]], minfbar=1, maxfbar=6, fcb=2)
)) + ylim(c(0, NA))
