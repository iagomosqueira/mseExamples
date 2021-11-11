# utilities.R - DESC
# /utilities.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


setMethod("propagate", signature(object="FLom"),
	function(object, iter, fill.iter=TRUE) {

    stock(object) <- propagate(stock(object), iter=iter, fill.iter=fill.iter)
    sr(object) <- propagate(sr(object), iter=iter, fill.iter=fill.iter)

    return(object)
  }
)
