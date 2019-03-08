# Experts -----------------------------------------------------------------

#' @name Experts
#' @title Making Expert Objects
#'
#' @description Experts are the "actors" which "bid" to see who conducts the
#'   next test. They contain an object "bidder" that determines bidding strategy
#'   and an object "constructor" that determines which feature it wants to text
#'   next. The \code{\link{runAuction}} function calls functions from experts
#'   and gWealth. The makeExpert function is not called directly, but through
#'   makeStepwiseExpert or makeScavengerExpert. Defaults are not set because
#'   these are internal functions called by \code{\link{rai}} and
#'   \code{\link{runAuction}} and all arguments are required.
#'
#' @param bidder bidder object; output of makeStepwiseBidder.
#' @param constructor constructor object; output of makeRawSource or
#'   makeLocalScavenger.
#' @param alg algorithm can be one of "rai", "raiPlus", or "RH" (Revisiting
#'   Holm).
#' @param gWealth global wealth object, output of gWealthStep.
#' @param ncolumns number of features the constructor should manage, thought of
#'   as columns of the design matrix.
#' @param theModelFeatures list of feature names in the model when the feature
#'   was rejected.
#' @param name name of base feature used in interactions with other features in
#'   the model.
#' @param sigma A character string or numeric value. String "ind" uses the rmse
#'   from the full regression model. String "sand" uses sandwich estimation for
#'   each test.
#' @return A closure containing a list of functions.

makeExpert = function (bidder, constructor, alg, sigma) {
  getPayVec = function(normY, n, p) {
    if (sigma == "sand" && alg!="RH") {  # RAI; sand SE
      df = n-p-1  # df not supplied; current model df
      bidVec = 2*pt(-sqrt(c(normY) * bidder$rVec()[bidder$state()$epoch])/
                      constructor$state()$rmseVec, df)
    } else {  # constant bid; bidVec needs appropriate length and missing values
      bidVec = rep(bidder$bids()[bidder$state()$epoch],
                   length(constructor$state()$active)-1)
      bidVec[is.na(constructor$state()$active[-1])] = NA
    }
    bidVec/(1-bidVec)
  }

  # Tells you where to skip to next epoch, position in constructor:active
  nextEpochInfo   = list(position = 0, epoch = Inf)
  nFailedTests  = 0  # failed tests in current model -> used to skip epochs
  expertBase = unlist(list(bidder, constructor), recursive = F)
  expertBase[which(names(expertBase)=="state")] = NULL  # repeated names
  expertBase$name = paste("Step", constructor$name)

  # New Functions ----------------------------------------------
  expertBase$state = function() {
    unlist(recursive=F, list(
      bidder$state(),
      constructor$state(),
      list(
        nfailed       = nFailedTests,
        nextEpochInfo = nextEpochInfo
      )))
  }
  # move these 3 to constructor after tests are set up
  expertBase$finishedEpoch = function() { constructor$state()$nactive == nFailedTests }
  expertBase$finishedPass  = function() { constructor$state()$position == 0 }
  expertBase$finished      = function() { constructor$state()$nactive == 0 }
  expertBase$udNextInfo    = function(nextPosition, nextEpoch) {
    if (nextEpoch < nextEpochInfo$epoch) {
      nextEpochInfo$position <<- nextPosition
      nextEpochInfo$epoch    <<- nextEpoch
    }
  }
  # moves and pays for update
  expertBase$setNextTest = function(normY, n, p, nextInfo = nextEpochInfo) {
    if (nextInfo$epoch < bidder$state()$epoch) stop("Should not need to move to earlier epoch!")
    if (nextInfo$position == Inf) nextInfo$position = max(constructor$state()$active, na.rm=T)
    cost = 0
    if (bidder$state()$epoch == nextInfo$epoch) { # movement this epoch; possible for rai
      cost = sum(getPayVec(normY, n, p)
                 [nextInfo$position:constructor$state()$position], na.rm=T)
    } else {
      if (constructor$state()$position > 0) {  # current epoch; only if tests remain
        cost = cost + sum(getPayVec(normY, n, p)
                          [1:constructor$state()$position], na.rm=T)
        bidder$udEpoch(1)
      }
      while (bidder$state()$epoch < nextInfo$epoch) { # pay for each epoch skipped
        cost = cost + sum(getPayVec(normY, n, p), na.rm=T)
        bidder$udEpoch(1)
      }
      if (nextInfo$position == 0) {  # final epoch; may be full pass; don't update epoch
        cost = cost + sum(getPayVec(normY, n, p), na.rm=T)
      } else {
        cost = cost + sum(getPayVec(normY, n, p)[-(1:nextInfo$position)], na.rm=T)
      }
    }
    # pay, move position (epoch moved above), reset nfailed and nextEpochInfo
    bidder$bidRejected(cost)
    constructor$udPosition(nextInfo$position)
    nFailedTests <<- sum(constructor$state()$active > nextInfo$position, na.rm=T)
    nextEpochInfo <<- list(position = 0, epoch = Inf)
  }
  expertBase$passTest = function(a) {
    bidder$bidAccepted(a)
    constructor$dropLastFeature()
    constructor$udRmse(NA)
    constructor$resetSigma()
  }
  expertBase$failTest = function(payment, rmse, rChange, pval) {
    bidder$bidRejected(payment)  # payment removed from global
    nFailedTests <<- nFailedTests + 1
    constructor$udRmse(rmse)
    # updating when this expert would next reject a test
    decisionVal = ifelse(alg == "RH", 1-pval, rChange)
    nextEpoch = which.max(bidder$rVec() < decisionVal)
    if (nextEpoch < nextEpochInfo$epoch) {
      nextEpochInfo$position <<- constructor$state()$prevPosition
      nextEpochInfo$epoch    <<- nextEpoch
    }
  }
  # a feature was rejected, so tests are in a new model
  expertBase$newModel = function() {
    nFailedTests           <<- 0
    nextEpochInfo$position <<- 0
    nextEpochInfo$epoch    <<- Inf
  }

  expertBase
}

#' @name Experts
makeStepwiseExpert = function(gWealth, ncolumns, alg, sigma) {
  makeExpert(
    makeStepwiseBidder(gWealth),
    makeRawSource(ncolumns),
    alg, sigma
  )
}

#' @name Experts
makeScavengerExpert = function (gWealth, theModelFeatures, name, alg, sigma) {
  makeExpert(
    makeStepwiseBidder(gWealth),
    makeLocalScavenger(theModelFeatures, name),
    alg, sigma
  )
}
