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

makeExpert = function (bidder, constructor) {
  nFailedTests  = 0  # n failed tests in current model
  expert = unlist(list(bidder, constructor), recursive = F)
  expert[which(names(expert)=="state")] = NULL  # repeated names
  expert$name = paste0("S", constructor$name)

  # New Functions ----------------------------------------------
  expert$state = function() {
    unlist(recursive=F, list(
      bidder$state(),
      constructor$state(),
      nfailed = nFailedTests
      ))
  }
  expert$finishedEp = function() { constructor$state()$nactive == nFailedTests }
  expert$rejTest = function(a) {
    bidder$bidAccepted(a)
    constructor$dropLastFeature()
  }
  expert$failTest = function(cost, vifOut) {
    bidder$bidRejected(cost)  # payment removed from global
    nFailedTests <<- nFailedTests + 1
    constructor$set_vif(vifOut)
  }
  expert$newEpoch = function(delta) {
    nFailedTests <<- 0
    bidder$ud_bidder(delta)
  }
  expert$newModel = function(alg) {  # tests in a new model
    nFailedTests  <<- 0
    bidder$ud_bidder()
    constructor$reset_vif()
  }

  expert
}

#' @name Experts
makeStepwiseExpert = function(gWealth, ncolumns) {
  expert = makeExpert(
    makeStepwiseBidder(gWealth),
    makeRawSource(ncolumns)
  )
}

#' @name Experts
makeScavengerExpert = function (gWealth, theModelFeatures, name) {
  makeExpert(
    makeStepwiseBidder(gWealth),
    makeLocalScavenger(theModelFeatures, name)
  )
}
