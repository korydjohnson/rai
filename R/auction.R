# Running Auction ---------------------------------------------------------

#' @name Auction
#' @title Internal function to manage multiple experts.
#'
#' @description runAuction is the workhorse of the rai package: it takes an
#'   initial expert list and runs the Revisiting Alpha-Investing algorithm to
#'   greedily fit (optional) polynomials and interactions to data.
#'   The term "auction" is the
#'   result of multiple experts bidding to perform the test which determines
#'   stepwise ordering. This function is not intended to be called directly, but
#'   through \code{\link{rai}}.
#'
#' @param experts list of expert objects. Each expert is the output of
#'   makeStepwiseExpert or makeScavengerExpert.
#' @param gWealth global wealth object, output of gWealthStep.
#' @param theData covariate matrix.
#' @param n number of observations.
#' @param p number of predictors in the \emph{current} model.
#' @param y the response as a single column matrix.
#' @param alg algorithm can be one of "rai", "raiPlus", or "RH" (Revisiting
#'   Holm).
#' @param poly logical. Should the algorithm look for higher-order polynomials?
#' @param searchType A character string specifying the prioritization of
#'   higher-order polynomials. One of "breadth" (more base features) or "depth"
#'   (higher order).
#' @param sigma type of error estimate used in gWealthStep; one of "ind" or "step".
#' @param m number of observations used in subsampling for variance inflation
#'   factor estimate of r.squared.
#' @param omega return from rejecting a test in Alpha-Investing.
#' @param reuse logical. Should repeated tests of the same covariate be
#'   considered a test of the same hypothesis? Reusing wealth isn't implemented
#'   for RAI or RAIplus (effect is negligible).
#' @param nMaxTest maximum number of tests
#' @param verbose logical. Should auction output be printed?
#' @param save logical. Should the auction results be saved? If TRUE, returns a
#'   summary matrix.
#' @param res residuals from current model.
#' @param X covariates in the current model.
#' @param x covariate being tested for addition into the model.
#' @param TSS total sum of squares; considering current residuals to be the response.
#' @param lmFit The core function that will be used to estimate linear model fits.
#'   The default is .lm.fit, but other alternatives are possible. Note that it
#'   does not use formula notation as this is costly. Another recommended option is
#'   fastLmPure from RcppEigen or related packages.
#' @param baseModel Features to include as the initial model. When NULL, the base
#'   model only includes the intercept. baseModel must be specified as a list of
#'   desired features. Each list element is a vector of column names or indices,
#'   where vectors of length > 1 specify an interaction term of those
#'   features. Please check the transformed data using \code{\link{prepareData}}
#'   in order to determine the correct column names and indexes of your desired
#'   model.
#' @return A list which includes the following components: \item{formula}{final
#'   model formula.} \item{y}{response.} \item{X}{model matrix from final
#'   model.} \item{features}{list of interactions included in formula.}
#'   \item{summary}{included if save=TRUE; matrix where each row contains the
#'   summary information of a single test.}

vif = function(res, y, X, x, n, p, m, TSS, lmFit) {
  # reg = .lm.fit(cbind(X, x), res)  # avoiding
  beta_yAdj = c(crossprod(x, res)/crossprod(x-mean(x)))
  if (m + p < n) {  # subsample for new x
    sam = sample.int(n, m+p)
    x = x[sam, 1, drop=FALSE]
    X = X[sam,  , drop=FALSE]
    res = res[sam, 1, drop=FALSE]
    y = y[sam, 1, drop=FALSE]
    TSS = c(crossprod(y-mean(y)))
  }
  # xRes  = .lm.fit(X, x)$residuals
  xRes = as.matrix(lmFit(X, x)$residuals)
  rho = c(crossprod(x-mean(x))/crossprod(xRes))
  if (abs(max(x) - min(x)) < .Machine$double.eps || rho > 10^2) {
    rho = 10^2
    rS = c(1 - crossprod(res) / TSS)  # rS still estimated, but irrelevant
  } else {
    rS = c(1 - crossprod(res-xRes*c(beta_yAdj*rho)) / TSS)
  }

  list(rho = rho, rS = rS)
}

featureName = function(indices, colNames) {
  paste0("I(", paste(sort(colNames[indices]), collapse="*"), ")")
}

rowProd = function(mat) {
  out = mat[ , 1, drop=FALSE]
  if (ncol(mat) > 1) {
    for (col in 2:ncol(mat)) {
      out = out*mat[ , col, drop=FALSE]
    }
  }
  out
}

#' @name Auction
runAuction = function(experts, gWealth, theData, y, alg, poly, searchType, m,
                      sigma, omega, reuse, nMaxTest, verbose, save, lmFit, baseModel) {
  # placeholders for results ------------------------
  n = nrow(y)
  p = 0
  res = y - mean(y)
  TSS = c(crossprod(res))
  X = matrix(1, nrow=n); # selected data columns
  theModelFeatures = list()
  rS = 0  # initial R^2; will keep track of incremental change
  colNames = colnames(theData)

  # features, X, expert list, rS, and sd for baseModel -----------
  if (!is.null(baseModel)) {
    p = length(baseModel)
    x = vector(mode = "list", length = p+1)
    x[[1]] = X
    for (i  in 1:p) {  # create features and experts
      if (class(baseModel[[i]]) == "character") {
        theModelFeatures[[i]] = match(baseModel[[i]], colNames)
      } else {
        theModelFeatures[[i]] = baseModel[[i]]
      }
      x[[i+1]] = rowProd(theData[ , theModelFeatures[[i]], drop=FALSE])  # feature
      if (poly) {  # new expert for interactions with rejected feature
        newExp = makeScavengerExpert(gWealth, theModelFeatures,
                                     paste(sort(theModelFeatures[[i]]), collapse="_"))
        if (searchType == "breadth") {
          experts = list(list(newExp), experts)
        } else {  # depth
          experts = list(experts, list(newExp))
        }
        experts = unlist(experts, recursive=FALSE)
      }
    }
    X = do.call(cbind, x)
    res = as.matrix(lmFit(X, y)$residuals)
    cpRes = c(crossprod(res))
    if (sigma != "ind") {gWealth$ud_resid(cpRes, sqrt(cpRes/(n-p-1)), n-p-1)}
    rS = 1 - cpRes/TSS
  }
  if (save) { results = list() }
  if (alg == "raiPlus") { nFinishedPass = nFinishedEp = 0 }

  # start auction ---------------------------
  ntest = 1
  epochsReady = sapply(experts, function(e) e$state()$epoch)
  while (ntest <= nMaxTest) {
    # max: select from end of experts; depth/breath builds list differently
    if (verbose) {
      cat("Test", ntest, "total wealth", experts[[1]]$state()$wealth,
          "nExperts", length(experts), "\n  epochsReady", epochsReady, "\n")
      if (alg == "raiPlus") {
        cat("  nFinishedEp", nFinishedEp, "nFinishedPass", nFinishedPass, "\n")
      }
    }
    eIndex = max(which(epochsReady == min(epochsReady, na.rm=TRUE)))
    iExpert = experts[[eIndex]]
    xIndex  = unlist(iExpert$feature())  # list or vector
    vifOut = iExpert$get_vif()
    if (any(is.na(vifOut))) {
      x = rowProd(theData[ , xIndex, drop=FALSE])  # feature
      vifOut = vif(res, y, X, x, n, p, m, TSS, lmFit)
    }
    state = iExpert$state()
    if (verbose) {
      cat("  expert", iExpert$name, "epoch", state$epoch,
          "\n  bid", state$bid, "cost", state$cost,
          "\n  tests", paste(xIndex, collapse="_"), ":\n")
      if (state$cost > state$wealth) { cat("**Insufficient wealth.") }
    }
    if (state$cost > state$wealth) { break }

    # compute p-value, conduct test, implications of test ------------------
    # look for % reduction in remaining ESS
    rChange = max(0, (vifOut$rS - rS)/(1-rS))
    if (rChange > state$rCrit && m < Inf) {  # retest with full data
      x = rowProd(theData[ , xIndex, drop=FALSE])  # feature
      vifOut = vif(res, y, X, x, n, p, m=Inf, TSS, lmFit)
      rChange = max(0, (vifOut$rS - rS)/(1-rS))
    }
    if (vifOut$rho > 20) {  # could make 20 a user chosen parameter
      if (verbose) {
        cat("**Variable", paste(xIndex, collapse="_"),
            "\n    is collinear; reject w/o penalty.\n")
      }
      iExpert$rejTest(0)  # don't test covariate again
    } else if(rChange < state$rCrit) {  # fail to reject
      if(verbose) { cat("  rChange", rChange, "<", state$rCrit, "rCrit\n") }
      iExpert$failTest(state$cost, vifOut)
    } else {  # reject
      if (verbose) {
        cat("  rChange", rChange, ">", "rCrit", state$rCrit,
            "rCrit\n++Add", featureName(xIndex, colNames),"++\n")
      }
      iExpert$rejTest(omega)  # add to wealth, don't test covariate again
      if (!all(is.na(iExpert$get_vif()))) {  # don't have x when used stored info
        x = rowProd(theData[ , xIndex, drop=FALSE])  # feature
      }
      X = cbind(X, x)  # add covariate and index to features
      p = p+1
      theModelFeatures[[p]] = xIndex
      res = as.matrix(lmFit(X, y)$residuals)
      cpRes = c(crossprod(res))
      if (sigma != "ind") {iExpert$ud_resid(cpRes, sqrt(cpRes/(n-p-1)), n-p-1)}
      rS = 1 - cpRes/TSS
      lapply(experts, function(e) e$newModel(alg))
      if (poly) {  # new expert for interactions with rejected feature
        newExp = makeScavengerExpert(gWealth, theModelFeatures,
                                     paste(sort(xIndex), collapse="_"))
        if (searchType == "breadth") {
          experts = list(list(newExp), experts)
          epochsReady = c(1, epochsReady)
          eIndex = eIndex + 1
        } else {  # depth
          experts = list(experts, list(newExp))
          epochsReady = c(epochsReady, 1)
        }
        experts = unlist(experts, recursive=FALSE)
        if (alg == "raiPlus") {  # finishedEp can set epochsReady=NA; so reset
          nFinishedEp = 0
          epochsReady = sapply(experts, function(e) {
            ifelse(e$finishedEp() || e$finishedPass(), NA, e$state()$epoch)
          })
        }
      }
    }

    # save information before updates -----------------------
    if(save) {
      results[[ntest]] = c(
        ntest   = ntest,
        wealth  = state$wealth,
        expert  = iExpert$name,
        feature = featureName(xIndex, colNames),
        bid     = state$bid,
        epoch   = state$epoch,
        rCrit   = state$rCrit,
        rS      = rS,
        rChange = rChange,
        rej     = rChange > state$rCrit
      )
    }

    # remove or update current expert -----------------------------------
    if (iExpert$finished()) {  # drop this expert
      if (verbose) { cat("**Expert", iExpert$name, "empty.\n") }
      experts[[eIndex]] = NULL
      epochsReady = epochsReady[-eIndex]
      eIndex = NULL
      if(!length(experts) && verbose) {
        cat("No active experts at", ntest+1, "\n")
        break
      }
    } else if (iExpert$finishedPass() || iExpert$finishedEp()) {  # must update
      behind = iExpert$state()$epoch < max(epochsReady, na.rm = TRUE)
      if (alg != "raiPlus" || (iExpert$finishedEp() && behind)) {
        if (iExpert$finishedPass()) { iExpert$ud_pass() }
        iExpert$newEpoch(1)
        epochsReady[eIndex] = epochsReady[eIndex] + 1  # iExpert$state()$epoch
      } else {  # raiPlus & !(finishedEp & behind) -> update counters
        if (iExpert$finishedPass()) { nFinishedPass = nFinishedPass + 1 }
        if (iExpert$finishedEp()) { nFinishedEp = nFinishedEp + 1 }
        epochsReady[eIndex] = NA  # one of previous conditions must be satisfied
      }
    }

    # update many experts simultaneously (raiPlus)
    if (alg == "raiPlus") {
      if (nFinishedPass == length(experts)) {
        lapply(experts, function(e) e$ud_pass())
        nFinishedPass = 0
        epochsReady = sapply(experts, function(e) {
          ifelse(e$finishedEp(), NA, e$state()$epoch)
        })
      }
      if (nFinishedEp == length(experts)) {
        lapply(experts, function(e) { e$newEpoch(1) })
        nFinishedEp = 0
        epochsReady = sapply(experts, function(e) {
          ifelse(e$finishedPass(), NA, e$state()$epoch)
        })
      }
    }
    ntest = ntest+1
  }
  if (ntest == nMaxTest+1) { cat("Reached maximum number of tests.") }

  # prepare output -----------------------------------------
  colnames(y) = "y"
  if (ncol(X) == 0) {
    colnames(X) = "1"
    f = "y ~ 1"
  } else {
    colnames(X) = c("1", mapply(featureName,
                                theModelFeatures, MoreArgs = list(colNames)))
    f = paste("y ~", paste(colnames(X), collapse="+"))
  }
  out = list(formula=f, y=y,  X=X, features=theModelFeatures,
             remainingWealth = gWealth$state()$wealth)
  if (save) { out$summary = do.call(rbind, results) }

  out
}
