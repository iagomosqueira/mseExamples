# mseExamples

This repository contains a series of examples of use of the [mse](https://github.com/flr/mse) package. The examples cover a range of alternative candidate Management Procedures (MPs), both model-based and model-free, and are used to show the methods available in [mse](https://github.com/flr/mse) for defining and testing them, as well as to summarize and present results. The examples include Operating Models (OMs) with one or more stocks, and single or multiple fisheries. Conditioning of the OMs is only carried out in the simplest of ways, by using the results of a stock assessment and incorporating uncertainty in future projections.


- **single_stock-model_free**: Single stock and single fleet OM, annual timestep. Applies a range of model-free MPs, from 'shortcut' to CPUE trends.

- **single_stock-sca**
- **single_stock-seasonal***
- ***single_stock-spict**
- **single_stock_two-sex**
- **two_stocks-multiple_fisheries**
- **two_stocks-multiple_fisheries-seasonal**
- **two_stocks-single_fishery**
- **two_stocks-two_fisheries**


- FLom_seasonal: Single stock and single fleet OM, seasonal timestep.

- FLombf_biol-fishery: Single stock (as FLBiol) and single fleet OM, annual timestep.

- FLombf_biol-fisheries: Single stock and multiple fleets, annual timestep.

- FLombf_biol-fisheries_seasonal: Single stock and multiple fleets, seasonal timestep.

- FLombf_biols-fisheries: Multiple stocks and multiple fleets, annual timestep.

- FLombf_biols-fisheries_seasonal: Multiple stocks and multiple fleets, seasonal timestep.

NOTE: This is work in progress, aiming at developing fairly clean code for the different OM structures available in .

## INSTALLATION

These examples are being developed to work with the development version of mse. Please install it by calling

```
remotes::install_github(paste("flr", c("FLCore", "FLFishery", "FLasher", "mse"), sep="/"))
```

