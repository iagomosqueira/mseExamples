# utilities.R - DESC
# /utilities.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# perfects.oem {{{

#' @examples
#' perfects.oem(om, deviances=NULL, observations=NULL, args=list(y0=1957, dy=2020), tracking=FLQuant())

perfects.oem <- function(om, deviances, observations, args, tracking,
  biomass=FALSE, ...) {

  # DIMENSIONS
  y0 <- ac(args$y0)
  dy <- ac(args$dy)

  # GET perfect stock
	stk <- window(stock(om), start=y0, end=dy, extend=FALSE)

  # SET perfect FLIndex per stock
  idx <- lapply(stk, function(x) {
    if(biomass) {
    abu <- catch(x) / fbar(x)
    idx <- FLIndexBiomass(index=abu %/% abu[,1],
      sel.pattern=catch.sel(x), index.q=expand(abu[,1],
        year=dimnames(abu)$year, fill=TRUE),
      effort=fbar(x), range=c(startf=0, endf=0))
    } else {
      idx <- FLIndex(index=stock.n(x) * 0.01,
        catch.n=catch.n(x), catch.wt=stock.wt(x),
        sel.pattern=catch.sel(x), index.q=stock.n(x) %=% 1 / 0.01,
        effort=fbar(x), range=c(startf=0, endf=0))
    }
    return(idx)
  })

  # STORE observations
  observations$stk <- stk
  observations$idx <- idx

	list(stk=stk, idx=idx, observations=observations, tracking=tracking)

} # }}}

# perfectS.oem {{{

perfectS.oem <- function(stk, deviances, observations, args, tracking,
  biomass=FALSE, ...) {

  # DIMENSIONS
  y0 <- ac(args$y0)
  dy <- ac(args$dy)

  # GET perfect stock
	# stk <- window(stock(om), start=y0, end=dy, extend=FALSE)
	stk <- window(stk, start=y0, end=dy, extend=FALSE)

  # TODO SIMPLIFY as with observations$stk
  
  # dis <- dim(observations$stk)

  #if(dis[3] == 1)
  #  stk <- nounit(stk)
  #if(dis[4] == 1)
  #  stk <- noseason(stk)

  # SET perfect FLIndex per stock
  if(biomass) {
    abu <- catch(stk) / fbar(stk)
    idx <- FLIndices(A=FLIndexBiomass(index=abu %/% abu[,1],
      sel.pattern=catch.sel(stk), index.q=expand(abu[,1],
        year=dimnames(abu)$year, fill=TRUE),
      effort=fbar(stk), range=c(startf=0, endf=0)))
  } else {
    idx <- FLIndices(A=FLIndex(index=stock.n(stk) * 0.01,
      catch.n=catch.n(stk), catch.wt=stock.wt(stk),
      sel.pattern=catch.sel(stk), index.q=stock.n(stk) %=% 1 / 0.01,
      effort=fbar(stk), range=c(startf=0, endf=0)))
  }

  # STORE observations
  observations$stk <- stk
  observations$idx <- idx

	list(stk=stk, idx=idx, observations=observations, tracking=tracking)

} # }}}
