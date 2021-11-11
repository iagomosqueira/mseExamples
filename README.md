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

These examples should work with the latest released version of FLR (for R 4.1), installed using

```
install.packages(c("mse"), repos=structure(c(CRAN="https://cloud.r-project.org/",
  FLR="https://flr-project.org/R")))
```

If you find nay problem please install the latest version of packages from github, changes are being made as these examples are developed.

```
remotes::install_github(paste("flr", c("FLCore", "FLFishery", "FLasher", "mse")))
```
