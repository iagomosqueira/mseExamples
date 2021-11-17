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

# control: perfect.sa + trend.hcr + seasonal.is

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # CCSBT trend HCR
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=0.95, nyears=5, metric=ssb))
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
))
