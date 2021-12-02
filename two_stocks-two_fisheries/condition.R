# condition.R - DESC
# /condition.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(FLasher)
library(FLFishery)
library(patchwork)

load("data/nsflat.RData")


# --- CREATE FLBiols

solb <- as(sol, 'FLBiol')
rec(solb) <- solsr

pleb <- as(ple, 'FLBiol')
rec(pleb) <- plesr

plot(solb)
plot(pleb)

soldevs <- FLQuant(rlnorm(1, rec(sol) %=% 1, 0.2))
pledevs <- FLQuant(rlnorm(1, rec(ple) %=% 1, 0.2))

devs <- FLQuants(ple=pledevs, sol=soldevs)

# --- CREATE Fleets (BT, GN)

dsol <- dimnames(sol)

# BT

solca <- as(sol, 'FLCatch')
pleca <- as(ple, 'FLCatch')

catch.sel(solca) <- FLQuant(c(0.1, 0.6, 0.8, 1, 0.8, 0.8, 0.7, 0.6, 0.6, 0.5, 0.5),
  dimnames=dsol, units="")
catch.sel(pleca) <- FLQuant(c(0.1, 0.5, 0.7, 0.9, 1, 0.8, 0.7, 0.6, 0.6, 0.5, 0.5),
  dimnames=dsol, units="")

bt <- FLFishery(name="BT", effort=FLQuant(100, dimnames=dsol[-1], units="days"),
  capacity=FLQuant(65, dimnames=dsol[-1], units="boats"),
  ple=pleca, sol=solca)

# catch.q$alpha = C / VB / E

catch.q(bt[['sol']])$alpha  <- catch(solca)[,1] /
  vb(solb, sel=catch.sel(solca))[,1] / effort(bt)[,1]
catch.q(bt[['ple']])$alpha  <- catch(pleca)[,1] /
  vb(pleb, sel=catch.sel(pleca))[,1] / effort(bt)[,1]

# GN

solca <- as(sol, 'FLCatch')
pleca <- as(ple, 'FLCatch')

catch.sel(solca) <- FLQuant(c(0.3, 0.8, 1, 0.9, 0.7, 0.5, 0.3, 0.2, 0.1, 0.05, 0.05),
  dimnames=dsol, units="")
catch.sel(pleca) <- FLQuant(c(0.4, 0.9, 1, 0.8, 0.65, 0.45, 0.25, 0.1, 0.05, 0.01, 0.01),
  dimnames=dsol, units="")

gn <- FLFishery(name="GN", effort=FLQuant(220, dimnames=dsol[-1], units="days"),
  capacity=FLQuant(120, dimnames=dsol[-1], units="boats"),
  ple=pleca, sol=solca)

# catch.q$alpha = C / VB / E

catch.q(gn[['sol']])$alpha  <- catch(solca)[,1] /
  vb(solb, sel=catch.sel(solca))[,1] / effort(gn)[,1]
catch.q(bt[['ple']])$alpha  <- catch(pleca)[,1] /
  vb(pleb, sel=catch.sel(pleca))[,1] / effort(gn)[,1]


# TESTS

ctrl <- fwdControl(
  list(year=1958:2020, quant="fbar", value=runif(63, 0.1, 0.2), biol=1,
    minAge=2, maxAge=6),
  list(year=1958:2020, quant="catch", value=39, fishery=2, catch=2,
    relYear=1958:2020, relFishery=1, relCatch=2)
)

ctrl <- fwdControl(
  list(year=1958:2020, quant="effort", value=runif(63, 0.9, 1.5), fishery=1,
    relYear=1957:2019, relFishery=1),
  list(year=1958:2020, quant="effort", value=runif(63, 0.9, 1.5), fishery=2,
    relYear=1957:2019, relFishery=2)
)

ctrl <- fwdControl(
  list(year=1958:2020, quant="catch", value=c(catch(ple)[,-1]), fishery=1, catch=1),
  list(year=1958:2020, quant="catch", value=c(catch(sol)[,-1]), fishery=2, catch=2)
)


tes <- fwd(FLBiols(ple=pleb, sol=solb), FLFisheries(bt=bt, gn=gn), control=ctrl,
  effort_max=1e6, deviances=devs)

plot(tes$biols) + 
  plot(lapply(tes$fisheries, catch)[[1]]) +
  plot(lapply(tes$fisheries, catch)[[2]])

harvest(tes$biols$sol@n, catch.n(tes$fisheries[[1]])$sol, tes$biol$sol@m)

harvest(tes$biols$ple@n, catch.n(tes$fisheries[[1]])$ple, tes$biol$ple@m)


# harvest(biol, catch=FLQuant)
# harvest(biol, catch=FLQuants)
# harvest(biol, catch=FLFishery, n)
# harvest(biol, catch=FLFisheries, n)


# BTB


# ---

data(nsfishery)


