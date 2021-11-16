# albio.R - DESC
# /albio.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

# LOAD om

load("data/albio.RData", verbose=TRUE)

# metrics: annual summaries

mets <- list(Rec=function(x) seasonSums(rec(x)),
  SSB=function(x) seasonMeans(ssb(x)),
  Catch=function(x) seasonSums(catch(x)),
  F=function(x) seasonMeans(fbar(x)))

# INSPECT OM

plot(om, metrics=mets)

plot(residuals(sr(om)))

# SETUP MSE runs

# 1. BACKTEST: perfect.sa + trend.hcr

# args: iy = intermediate year, start of simulation
mseargs <- list(iy=2000)

# SET ratio of TAC per season, based on F
ratio <- c(iterMeans(fbar(stock(om))[,'2000'] %/%
  seasonSums(fbar(stock(om))[,'2000'])))

# control: perfect.sa + trend.hcr + seasonal.is

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # CCSBT trend HCR
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=0.85, nyears=5, metric=vb)),
  # seasonal to yearly TAC
  isys = mseCtrl(method=seasonal.is, args=list(ratio=ratio))
  ))

# RUN mp backtest

system.time(btest <- mp(om, oem=oem, ctrl=control, args=mseargs))

plot(om, BACKTEST=btest, metrics=mets)

# RUN comparison fwd: F=FMSY, F=0

fmsy <- fwd(om, control=fwdControl(lapply(1999:2017, function(x)
  list(year=x, season=1:4, quant="fbar", value=refpts(om)$FMSY/4))),
  deviances=residuals(sr(om)))

f0 <- fwd(om, control=fwdControl(lapply(1999:2017, function(x)
  list(year=x, season=1:4, quant="fbar", value=0))),
  deviances=residuals(sr(om)))

# PERFORMANCE

data(statistics)

perf <- performance(btest, statistics=statistics[c('SBMSY', 'green', 'red')],
  metrics=mets, years=list(2005:2017))

# OUTPUT

plot(FLStocks(OM=stock(om), BACKTEST=stock(btest), FMSY=stock(fmsy),
  F0=stock(f0)), metrics=mets)

# TRACKING

tracking(btest)

