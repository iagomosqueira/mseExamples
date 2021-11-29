# mseExamples

Some draft examples on the usage of FLR's mse package:

- FLom: Single stock and single fleet OM, annual timestep.

- FLom_seasonal: Single stock and single fleet OM, seasonal timestep.

- FLombf_biol-fishery: Single stock (as FLBiol) and single fleet OM, annual timestep.

- FLombf_biol-fisheries: Single stock and multiple fleets, annual timestep.

- FLombf_biol-fisheries_seasonal: Single stock and multiple fleets, seasonal timestep.

- FLombf_biols-fisheries: Multiple stocks and multiple fleets, annual timestep.

- FLombf_biols-fisheries_seasonal: Multiple stocks and multiple fleets, seasonal timestep.

NOTE: This is work in progress, aiming at developing fairly clean code for the different OM structures available in [mse](https://github.com/flr/mse).

## INSTALLATION

These examples are being developed to work with the development version of mse. Please install it by calling

```
remotes::install_github(paste("flr", c("FLCore", "FLFishery", "FLasher", "mse"), sep="/"))
```
