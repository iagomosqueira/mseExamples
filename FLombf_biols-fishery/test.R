# test.R - DESC
# /test.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2



tom <- FLom(stock=stf(ple, end=2040), sr=plesr, projection=mseCtrl(method=fwd.om))

toem <- FLoem(method=perfect.oem, observations=list(stk=window(ple, end=2040)))

mseargs <- list(iy=2020)

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # CCSBT trend HCR
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=0.95, nyears=5, metric=ssb))
  ))


tes <- mp(tom, oem=toem, ctrl=control, args=mseargs)

plot(tom, tes)


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


# ---
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
