# Constructors ------------------------------------------------------------

#' @name Feature-Constructors
#' @title Making Source Objects
#'
#' @description These functions create and manage the features to test. The raw
#'   source only tests marginal features (the covariates in the design matrix)
#'   while the scavenger source tests for interactions between a base feature
#'   and those features already in the model. makeLocalScavenger builds on
#'   makeRawSource. Defaults are not set because these are internal functions
#'   called by \code{\link{rai}} and \code{\link{runAuction}} and all arguments
#'   are required.
#'
#' @param ncolumns number of features this constructor should manage, thought of
#'   as columns of the design matrix.
#' @param theModelFeatures other features currently in the model.
#' @param name name of the base feature with which to create interactions.
#' @return A closure containing a list of functions.

makeRawSource = function(ncolumns) {
  activeColumns = 0:ncolumns
  position      = ncolumns  # position in list, work back to front
  prevPosition  = NA
  nactive       = ncolumns
  vifSaved      = as.list(rep(NA, ncolumns))

  list(
    name = "Marginal",
    state = function() { list(
      position     = position,
      prevPosition = prevPosition,
      active       = activeColumns[-1],  # only shows columns -> correct indexes
      nactive      = nactive
    )},
    finishedPass = function() { position == 0 },
    finished = function() { nactive == 0 },
    # both get/set are called *after* feature(), so used prevPosition
    get_vif = function() { vifSaved[[prevPosition]] },
    set_vif = function(vifOut) { vifSaved[[prevPosition]] <<- vifOut },
    reset_vif = function() { vifSaved <<- as.list(rep(NA, ncolumns)) },
    feature  = function() {
      prevPosition <<- position
      position     <<- max(activeColumns[activeColumns<position],na.rm=T)
      return(prevPosition)
    },
    # +1 corrects for 0 based index. Position is which column, not location in vector
    dropLastFeature = function() {
      activeColumns[prevPosition + 1] <<- NA
      nactive <<- nactive-1
    },
    ud_pass = function() {  # move to beginning of list of features
      position <<- max(activeColumns, na.rm=T)
      prevPosition <<- NA
    }
  )
}

#' @name Feature-Constructors
makeLocalScavenger = function(theModelFeatures, name) {
  baseFeature = theModelFeatures[[length(theModelFeatures)]]
  raw = makeRawSource(length(theModelFeatures))
  raw$name = paste("Poly", name)
  e = environment(raw$state)
  raw$feature = function() {  # don't need deep assignment as given environment
    e$prevPosition = e$position
    e$position     = max(e$activeColumns[e$activeColumns<e$position], na.rm=T)
    return(list(theModelFeatures[[e$prevPosition]], baseFeature))
  }

  raw
}
