# debug.R - DESC
# /debug.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


profvis::profvis({hind <- mp(om, oem=oem, ctrl=control, args=list(iy=2000, fy=2001))})

# CLEAN window@oem
library(microbenchmark)

microbenchmark(qapply(stock(om), log))

# qapply		{{{
setMethod('qapply', signature(X='FLStock', FUN='function'),
	function(X, FUN, ..., exclude=missing, simplify=FALSE) {
    
    FUN <- match.fun(FUN)
  
    slots <- c("catch", "catch.n", "catch.wt", "discards", "discards.n",
      "discards.wt", "landings", "landings.n", "landings.wt", "stock",
      "stock.n", "stock.wt", "m",  "mat", "harvest", "harvest.spwn", "m.spwn")

		if(!missing(exclude))
      slots <- slots[!slots %in% exclude]

    res <- lapply(setNames(nm=slots), function(i) do.call(FUN, list(slot(X,i), ...)))

    # RETURN list if not FLQuant elements
    if(is(res[[1]], "FLQuant")) {
      res <- do.call("FLStock", c(res, list(name=X@name, desc=X@desc, range=X@range)))
    }

    if(simplify)
      res <- unlist(res)

		return(res)
	}
)   # }}}

microbenchmark(qapply(stock(om), log))

# qapply		{{{
setMethod('qapply', signature(X='FLStock', FUN='function'),
	function(X, FUN, ..., exclude=missing, simplify=FALSE) {
		
    FUN <- match.fun(FUN)
  
    slots <- c("catch", "catch.n", "catch.wt", "discards", "discards.n",
      "discards.wt", "landings", "landings.n", "landings.wt", "stock",
      "stock.n", "stock.wt", "m",  "mat", "harvest", "harvest.spwn", "m.spwn")

		if(!missing(exclude))
      slots <- slots[!slots %in% exclude]

    res <- setNames(as.list(slots), nm=slots)

    for(i in seq(slots))
      res[[i]] <- do.call(FUN, list(slot(X, slots[i]), ...))
    # RETURN list if not FLQuant elements
    if(is(res[[1]], "FLQuant")) {
    
      dims <- dims(res[[2]])
      range <- c(min=dims$min, max=dims$max, plusgroup=min(dims$max, X@range['plusgroup']),
		    minyear=dims$minyear, maxyear=dims$maxyear, minfbar=dims$min,
        maxfbar=dims$max)

      res <- do.call(new, c(res, list(Class="FLStock", name=X@name,
        desc=X@desc, range=range)))
    }

    if(simplify)
      res <- unlist(res)

		return(res)
	}
)   # }}}

qapply(stock(om), log)
microbenchmark(qapply(stock(om), log))

