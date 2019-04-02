# Bidders -----------------------------------------------------------------

#' @name Bidders
#' @title Making Bidder Objects
#'
#' @description These functions create objects that manage alpha-wealth. There
#'   is only one stepwise "bidder" that manages the global wealth (gWealth) but
#'   it can have multiple "offspring" when searching for polynomials. The outer
#'   \code{\link{rai}} function creates one gWealthStep object and one stepwise
#'   bidder at the beginning. The stepwise bidder makes a local modification to
#'   gWealth, though bidAccepted/bidRejected still call gWealth. More stepwise
#'   bidders are created as "scavengers" tied to the global wealth. Defaults are
#'   not set because these are internal functions called by \code{\link{rai}}
#'   and \code{\link{runAuction}} and all arguments are required.
#'
#' @param wealth starting alpha-wealth.
#' @param alg algorithm can be one of "rai", "raiPlus", or "RH" (Revisiting
#'   Holm).
#' @param r RAI rejects tests which increase R^2 by a factor r^s, where s is the
#'   epoch.
#' @param TSS total sum of squares of the response.
#' @param p number of covariates (only used when alg == "RH").
#' @param reuse logical. Should repeated tests of the same covariate be
#'   considered a test of the same hypothsis?
#' @param rmse initial (or independent) estimate of residual standard error
#' @param df degrees of freedom of rmse.
#' @param gWealth a global wealth object; output of gWealthStep.
#' @return A closure containing a list of functions.

#' @export
gWealthStep = function(wealth, alg, r, TSS, p, reuse, rmse, df) {
  # Compute thresholds for p-values and bids for stepwise; either RAI or RH
  if (alg == "RH") {  # Revisiting Holm; need entire rVec
    holm = c(wealth/(p:1), 1:5)  # holm critical values, 1:5 allows termination
    bids = rejL = c(holm/(1+holm), 1)  # holm *payment*; 1 ensures epoch skip
    if (reuse) {  # rejection levels condition on previous test
      for (i in 2:(p+1)) {
        rejL[i] = bids[i] + rejL[i-1] - bids[i]*rejL[i-1]
      }
    }
    rVec = c(qt(rejL/2, df)^2*rmse^2/TSS, 0)  # initial conversion to rS scale
  } else {  # rai/raiPlus
    rVec = c(r^(1:(10/(1-r))), 0)  # 0 prevents NA in epoch skip
  }

  list(
    state = function() {
      list(wealth = wealth,
           rVec = rVec)
    },
    bidAccepted = function(a) { wealth <<- wealth + a },
    bidRejected = function(d) { wealth <<- wealth - d },
    ud_resid = function(resTSS, rmse, df) {  # think of new resid as y
      TSS <<- resTSS; rmse <<- rmse; df   <<- df
      if (alg == "RH") {
        rVec <<- c(qt(rejL/2, df)^2*rmse^2/TSS, 0)  # convert to rS scale
      }
    },
    bid = function(epoch) {  # ud rCrit; return bid; always used -> can't reduce
      2*pt(-sqrt(rVec[epoch])*sqrt(TSS)/rmse, df)  # p-value scale
    }
  )
}

#' @name Bidders
#' @export
makeStepwiseBidder = function(gWealth) {
  epoch = 1
  rCrit = gWealth$state()$rVec[epoch]
  bid = gWealth$bid(epoch)
  cost  = bid/(1-bid)

  bidder = gWealth
  bidder$state   = function() {
    unlist(list(gWealth$state(),
                list(epoch = epoch,
                     bid   = bid,
                     cost  = cost,
                     rCrit = rCrit)), recursive = F)
  }
  bidder$ud_bidder = function(delta=0) {
    epoch <<- epoch + delta
    rCrit <<- gWealth$state()$rVec[epoch]
    bid   <<- gWealth$bid(epoch)
    cost  <<- bid/(1-bid)
  }

  bidder
}
