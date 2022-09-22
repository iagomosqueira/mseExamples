# model.R - Running mse with NS sol.27.40
# FLom/model.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


#install.packages(c("mse"), repos=c(
#    FLR="https://flr-project.org/R",
#    CRAN="https://cloud.r-project.org"))

#remotes::install_github('flr/mse')


library(mse)

load('data/sol2740_om.Rdata')

# SET intermediate year, start of runs

mseargs <- list(iy=2020, fy=2023)


# --- APPLY CCSBT trend HCR

# control: perfect.sa + trend.hcr

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # CCSBT trend HCR
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1, k2=3, gamma=0.80, nyears=5, metric=ssb))
))

# RUN mp

trend <- mp(om, oem=oem, ctrl=control, args=mseargs)

# Same MP can be run every 3 years

trend3y <- mp(om, oem=oem, ctrl=control, args=list(iy=2020, frq=3))


# control: mlc + trend.hcr

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=len.ind, args=list(indicator=c("lbar", "lmax5" ),
    params=FLPar(linf=35, k=0.352, t0=-0.26), cv=0.2)),
  # CCSBT trend HCR
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1, k2=3, gamma=0.80, nyears=5, metric=ssb))
))

# RUN mp

ltrend <- mp(om, oem=oem, ctrl=control, args=mseargs)


# --- RUN mean length indicator + target level HCR

control <- mpCtrl(list(
  # perfect.sa
  est = list(mseCtrl(method=perfect.sa),
    mseCtrl(method=len.ind, args=list(indicator="mlc",
    params=FLPar(linf=35, k=0.352, t0=-0.26), cv=0.2))),
  # CCSBT trend HCR
  hcr = mseCtrl(method=target.hcr,
    args=list(lim=15, target=20, metric="mlc"))
))

length <- mp(om, oem=oem, ctrl=control, args=mseargs)


# --- APPLY ICES HCR and TAC short-term forecast

# control: perfect.sa + ices.hcr + tac.is

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # ICES HCR
  hcr = mseCtrl(method=ices.hcr,
    args=list(ftrg=c(refpts(om)$Fmsy) * 0.5, sblim=c(refpts(om)$SBlim),
      sbsafe=c(refpts(om)$Bpa))),
  # One-year forecast for TAC
  isys=mseCtrl(method=tac.is, args=list(dtaclow=0.85, dtacupp=1.15, recyrs=30))
  ))

# RUN mp

ices <- mp(om, oem=oem, ctrl=control, args=mseargs)


# --- TUNE MP for 60% P(SB = SBMSY) over years 2030:2040

# TODO: EXAMPLE on performance and probability

tun <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets, statistic=statistics["PSBMSY"], years=2030:2040,
  tune=list(sblim=c(10000, 50000)), prob=0.6, tol=0.01, maxit=12)


# --- ASSEMBLE MP runs

runs <- list(TREND=trend, TREND2Y=trend3y, LEN=length, ICES=ices)

plot(window(om, start=2005), runs)

save(runs, file="model/runs.Rdata", compress="xz")
