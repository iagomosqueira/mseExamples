# model.R - Running mse with NS sol.27.40
# FLom/model.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

load('data/sol2740.Rdata')

# SET intermediate year, start of runs

mseargs <- list(iy=2020)

# --- APPLY CCSBT trend HCR

# control: perfect.sa + trend.hcr

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # CCSBT trend HCR
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=2, k2=2, gamma=0.85, nyears=5, metric=ssb))
  ))

# RUN mp

trend <- mp(om, oem=oem, ctrl=control, args=mseargs)

# PLOT

plot(om, TREND=trend)

# --- RUN every 2 years

trend2y <- mp(om, oem=oem, ctrl=control, args=list(iy=2020, frq=2))

plot(window(om, start=2000), TREND=trend, TREND2Y=trend2y)


# --- RUN mean length indicator + target level HCR

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=len.ind, args=list(indicator="mlc",
    params=FLPar(linf=35, k=0.352, t0=-0.26), cv=0.2)),
  # CCSBT trend HCR
  hcr = mseCtrl(method=target.hcr,
    args=list(lim=15, target=20, metric="mlc"))
  ))

length <- mp(om, oem=oem, ctrl=control, args=mseargs)

plot(om, length)


# --- APPLY ICES HCR and TAC short-term forecast

# control: perfect.sa + ices.hcr + tac.is

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # ICES HCR
  hcr = mseCtrl(method=ices.hcr,
    args=list(ftrg=c(refpts(om)$Fmsy)*0.85, sblim=c(refpts(om)$SBlim),
      sbsafe=c(refpts(om)$Bpa))),
  # One-year forecast for TAC
  isys=mseCtrl(method=tac.is, args=list(dtaclow=0.85, dtacupp=1.15, recyrs=30))
  ))

# RUN mp

ices <- mp(om, oem=oem, ctrl=control, args=mseargs)

# PLOT

plot(om, TREND=trend, ICES=ices)


# --- RUN FLa4a ICES HCR and TAC short-term forecast

library(FLa4a)

# a4a MODEL

# fmodel
fmod <- ~te(replace(age, age > 8, 8), year, k = c(4, 22)) +
  s(replace(age, age > 8, 8), k=4) + s(year, k=22, by=as.numeric(age==1))

# qmodel (BTS, SNS)
qmod <- list(~s(age, k=3), ~s(age, k=3))

# vmodel (catch, BTS, SNS)
vmod <- list(~s(age, k=3), ~s(age, k=3), ~s(age, k=3))

# srmodel
srmod <- ~factor(year)

# control: sca.sa + ices.hcr + tac.is

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=sca.sa, args=list(fmodel=fmod, qmodel=qmod,
    vmodel=vmod, srmodel=srmod)),
  # CCSBT trend HCR
  hcr = mseCtrl(method=ices.hcr,
    args=list(ftrg=c(refpts(om)$Fmsy), sblim=c(refpts(om)$Blim),
      sbsafe=c(refpts(om)$Bpa))),
  isys=mseCtrl(method=tac.is, args=list(dtaclow=0.85, dtacupp=1.15, recyrs=30))
  ))

# RUN mp

scamp <- mp(om, oem=oem, ctrl=control, args=mseargs)

# PLOT

plot(om, TREND=trend, ICES=ices, SCA=scamp)

# TRACKING

tracking(trend)['time', ]
tracking(ices)['time', ]

# COMPARE performance

data(statistics)

perf <- performance(list(TREND=trend, ICES=ices), statistics[c('SBMSY', 'risk1')],
  metrics=list(SB=ssb), years=list(2021:2030))

# PLOT

ggplot(perf, aes(x=mp, y=data)) + geom_boxplot(aes(fill=mp)) +
  facet_wrap(~name, scales="free")

