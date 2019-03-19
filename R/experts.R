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
#' @param gWealth global wealth object, output of gWealthStep.
#' @param ncolumns number of features the constructor should manage, thought of
#'   as columns of the design matrix.
#' @param theModelFeatures list of feature names in the model when the feature
#'   was rejected.
#' @param name name of base feature used in interactions with other features in
#'   the model.
#' @return A closure containing a list of functions.

#' @export
makeExpert = function (bidder, constructor) {
  # Tells you where to skip to next epoch, position in constructor:active
  nextEpochInfo   = list(position = 0, epoch = Inf)
  nFailedTests  = 0  # failed tests in current model -> used to skip epochs
  expertBase = unlist(list(bidder, constructor), recursive = F)
  expertBase[which(names(expertBase)=="state")] = NULL  # repeated names
  expertBase$name = paste0("S", constructor$name)

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
  expertBase$finishedEpoch = function() {
    constructor$state()$nactive == nFailedTests
  }
  expertBase$rejTest = function(a) {
    bidder$bidAccepted(a)
    constructor$dropLastFeature()
  }
  expertBase$failTest = function(cost, rChange) {
    bidder$bidRejected(cost)  # payment removed from global
    nFailedTests <<- nFailedTests + 1
    nextEpoch = which.max(bidder$state()$rVec <= rChange)
    if (nextEpoch < nextEpochInfo$epoch) {  # when next reject a test
      nextEpochInfo$position <<- constructor$state()$prevPosition
      nextEpochInfo$epoch    <<- nextEpoch
    }
  }
  expertBase$newModel = function(alg) {  # tests in a new model
    nFailedTests  <<- 0
    nextEpochInfo <<- list(position = 0, epoch = Inf)
    bidder$ud_bidder()
    if (alg == "raiPlus" && constructor$finishedPass()) {
      constructor$ud_pass()
    }
  }
  expertBase$setNextTest = function(nextInfo = nextEpochInfo) {  # move and pay
    stopifnot(nextInfo$epoch >= bidder$state()$epoch)
    active = constructor$state()$active
    posCur = constructor$state()$position
    posNext = min(nextInfo$position, max(active, na.rm=T))  # nI$pos can be Inf
    cost = 0
    if (bidder$state()$epoch == nextInfo$epoch) { # same epoch
      stopifnot(posNext <= posCur)
      if (posNext < posCur) {  # can be equal, then cost=0
        cost = bidder$state()$cost * sum(!is.na(active[(posNext+1):posCur]))
      }
    } else {
      cost = cost + bidder$state()$cost * sum(active <= posCur, na.rm=T)
      bidder$ud_bidder(1)
      while (bidder$state()$epoch < nextInfo$epoch) { # pay for skipped epochs
        cost = cost + bidder$state()$cost * sum(!is.na(active))
        bidder$ud_bidder(1)
      }
      if (posNext == 0) {  # full pass in final epoch; don't update epoch
        cost = cost + bidder$state()$cost * sum(!is.na(active))
      } else {
        cost = cost + bidder$state()$cost * sum(active > posNext, na.rm=T)
      }
    }
    # pay, move position (epoch moved above)
    bidder$bidRejected(cost)
    constructor$ud_position(posNext)
  }

  expertBase
}

#' @name Experts
#' @export
makeStepwiseExpert = function(gWealth, ncolumns) {
  expert = makeExpert(
    makeStepwiseBidder(gWealth),
    makeRawSource(ncolumns)
  )
}

#' @name Experts
#' @export
makeScavengerExpert = function (gWealth, theModelFeatures, name) {
  makeExpert(
    makeStepwiseBidder(gWealth),
    makeLocalScavenger(theModelFeatures, name)
  )
}
