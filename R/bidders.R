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
#' @param p number of covariates (only used when alg == "RH").
#' @param n number of observations.
#' @param varY variance of the response.
#' @param alg algorithm can be one of "rai", "raiPlus", or "RH" (Revisiting
#'   Holm).
#' @param sigma A character string or numeric value. String "ind" uses the rmse
#'   from the full regression model. String "sand" uses sandwich estimation for
#'   each test.
#' @param df if a numeric sigma value is given, must also provide the degrees of
#'   freedom for the estimate.
#' @param r RAI rejects tests which increase R^2 by a factor r^s, where s is the
#'   epoch.
#' @param reuse logical. Should repeated tests of the same covariate be
#'   considered a test of the same hypothsis?
#' @param wealth starting alpha-wealth.
#' @param gWealth a global wealth object; output of gWealthStep.
#' @return A closure containing a list of functions.

gWealthStep = function(p, n, varY, alg, sigma, df, r, reuse, wealth) {
  normY = (n-1)*varY
  rVec = c(r^(1:(10/(1-r))), 0)  # 0 prevents NA in epoch skip
  bids = pcrits = NA
  # Compute thresholds for p-values and bids for stepwise; either RAI or RH
  if (alg == "RH") {  # Revisiting Holm thresholds
    holm = c(wealth/(p:1), 1:5)  # holm critical values, 1:5 allows termination
    bids = pcrits = c(holm/(1+holm), 1)  # fail -> pay extra -> rescale; 1 prevents NA in epoch skip
    if (reuse) {
      for (i in 2:(p+1)) {
        pcrits[i] = bids[i] + pcrits[i-1] - bids[i]*pcrits[i-1]
      }
    }
    rVec = 1-pcrits  # "rejectionVec"; needed to skip epochs; 1- so reject when rVec < 1-pval
  } else if (is.numeric(sigma) && is.numeric(df)) {  # rai/raiPlus, known sigma/df
    bids = pcrits = 2*pt(-sqrt(c(normY/sigma^2) * rVec), df)  # convert R^2 to p-values
  }

  list(
    state = function() { list(wealth = wealth) },
    bidAccepted = function(a) { wealth <<- wealth + a },
    bidRejected = function(d) { wealth <<- wealth - d },
    rVec        = function() { rVec },
    bids        = function() { bids },
    bid         = function(s, rmse, df) {
      if (sigma == "sand" && alg!="RH") {
        bid = 2*pt(-sqrt(normY * rVec[s]) / rmse, df)
      } else {
        bid = bids[s]
      }
      min(bid, wealth/(1+wealth), .25)
    },
    pcrit = function(bid, s) {
      if (!reuse) {
        bid
      } else {
        if (isTRUE(all.equal(bid, bids[s]))) {
          pcrits[s]
        } else {  # last test with remaining wealth
          ifelse(reuse, bid + pcrits[s-1]-bid*pcrits[s-1], bid)
        }
      }
    }
  )
}

#' @name Bidders
makeStepwiseBidder = function(gWealth) {
  s = 1  # epoch
  bidderBase = gWealth
  bidderBase$state   = function() {
    unlist(list(gWealth$state(), list(epoch = s)), recursive = F)
  }
  bidderBase$udEpoch = function(delta) { s <<- s + delta }
  bidderBase$bid     = function(rmse, df)  { gWealth$bid(s, rmse, df) }
  bidderBase$pcrit   = function(bid)  { gWealth$pcrit(bid, s) }
  bidderBase
}
