# Running Auction ---------------------------------------------------------

#' @name Auction
#' @title Internal function to manage multiple experts.
#'
#' @description runAuction is the workhorse of the rai package: it takes an
#'   initial expert list and runs the Revisiting Alpha-Investing algorithm to
#'   greedily fit (optional) polnomials to data. The term "auction" is the
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
#'   considered a test of the same hypothsis? Reusing wealth isn't implemented
#'   for RAI or RAIplus (effect is negligible)
#' @param nMaxTest maximum number of tests
#' @param verbose logical. Should auction output be prited?
#' @param save logical. Should the auction results be saved? If TRUE, returns a
#'   summary matrix.
#' @param indices list of columns of theData used in an interaction.
#' @param res residuals from current model
#' @param X covariates in the current model.
#' @param x covariate being tested for addition into the model.
#' @param TSS total sum of squares; considering current residuals to be the response.
#' @param epochCap when skipping epochs, cannot skip passed this point.
#' @return A list which includes the following components: \item{formula}{final
#'   model formula.} \item{y}{response.} \item{X}{model matrix from final
#'   model.} \item{experts}{list of experts.} \item{theModelFeatures}{list of
#'   feature names in the final model.} \item{options}{options given to RAI:
#'   algorithm, searchType, poly}

featureName = function(indices, theData) {
  paste(sort(colnames(theData)[indices]), collapse="_")
}

#' @name Auction
vif = function(res, y, X, x, n, p, m, TSS) {
  # reg = .lm.fit(cbind(X, x), res)  # avoiding
  beta_yAdj = c(crossprod(x, res)/crossprod(x-mean(x)))
  if (m + p < n) {  # subsample for new x
    sam = sample.int(n, m+p)
    x = x[sam, 1, drop=F]
    X = X[sam,  , drop=F]
    res = res[sam, 1, drop=F]
    y = y[sam, 1, drop=F]
    TSS = c(crossprod(y-mean(y)))
  }
  xRes  = .lm.fit(X, x)$residuals
  rho   = c(crossprod(x-mean(x))/crossprod(xRes))
  if (length(unique(x))==1 || rho > 10^2) {  # rS unstable for huge rho
    list(rho = 10^2, rS = c(1 - crossprod(res) / TSS))
  } else {
    list(rho = rho, rS = c(1 - crossprod(res-xRes*c(beta_yAdj*rho)) / TSS))
  }
}

#' @name Auction
skipEpochs = function(experts, verbose, alg, epochCap) {
  goalEpochs = sapply(experts, function(e) e$state()$nextEpochInfo$epoch)
  nIndex = max(which(goalEpochs == min(goalEpochs)))  # max: start at end
  nEpoch = min(goalEpochs, epochCap)
  if (verbose) {
    cat("**Moving Experts**\n")
    epochs = sapply(experts, function(e) e$state()$epoch)
    position = sapply(experts, function(e) e$state()$position)
    goalPos = sapply(experts, function(e) e$state()$nextEpochInfo$position)
    names = sapply(experts, function(e) e$name)
    df = rbind(position, epochs, goalPos, goalEpochs, nEpoch)
    colnames(df) = names
    print(df)
  }
  if (nIndex > 1) {  # later experts set to beginning
    nInfo = list("position"=Inf, "epoch"=nEpoch)
    sapply(experts[1:(nIndex-1)], function(e) e$setNextTest(nInfo))
  }
  if (nIndex < length(experts)) {  # earlier experts set to end
    nInfo = list("position" = 0, "epoch" = nEpoch+(alg!="raiPlus"))
    sapply(experts[(nIndex+1):length(experts)],function(e) e$setNextTest(nInfo))
  }
  if (nEpoch < experts[[nIndex]]$state()$nextEpochInfo$epoch) {
    nInfo = list("position" = Inf, "epoch" = nEpoch+(alg!="raiPlus"))
    experts[[nIndex]]$setNextTest(nInfo)  # Inf so doesn't skip others
  } else {
    experts[[nIndex]]$setNextTest()  # will start testing here
  }
  experts[[nIndex]]$newModel(alg)  # resets nFailed (to ensure it tests)
}

#' @name Auction
runAuction = function(experts, gWealth, theData, y, alg, poly, searchType, m,
                      sigma, omega, reuse, nMaxTest, verbose, save) {
  # placeholders for results ------------------------
  n = nrow(y)
  p = 0
  theModelFeatures = list()
  X = matrix(1, nrow=n); # selected data columns
  res = y - mean(y)
  TSS = c(crossprod(res))
  if(save) { results = list() }
  # start auction ---------------------------
  ntest = 1
  rS = 0  # initial R^2; will keep track of incremental change
  while (ntest <= nMaxTest) {
    # identify test information   -----------------------------------------
    # if final expert being tested, can have strange effects, so update
    finishedPass = sapply(experts, function(e) e$finishedPass())
    if (all(finishedPass)) {
      sapply(experts, function(e) {
        e$ud_pass()
        if (alg!="raiPlus") e$ud_bidder(1)
      })
    }
    finishedEp = sapply(experts, function(e) e$finishedEp())
    epochs = sapply(experts, function(e) e$state()$epoch)
    epochsReady = ifelse(finishedPass | finishedEp, NA, epochs)
    # max: select from end of experts; depth/breath builds list differently
    eIndex = max(which(epochsReady == min(epochsReady, na.rm=T)))
    iExpert = experts[[eIndex]]
    xIndex  = unlist(iExpert$feature())  # list or vector
    x = as.matrix(apply(theData[ , xIndex, drop=F], 1, prod))  # feature vector
    rCrit = iExpert$state()$rCrit
    cost  = iExpert$state()$cost
    epoch = iExpert$state()$epoch
    if(verbose) {
      cat("Test", ntest, "total wealth", gWealth$state()$wealth,
          "\n  expert", iExpert$name, "epoch", epoch,
          "\n  bid", iExpert$state()$bid, "cost", cost,
          "\n  tests", paste(xIndex, collapse="_"), ":\n")
      if (cost > gWealth$state()$wealth) { cat("**Insufficient wealth.") }
    }
    if (cost > gWealth$state()$wealth) { break }

    # compute p-value, conduct test, implications of test ------------------
    vifOut = vif(res, y, X, x, n, p, m, TSS)
    # look for % reduction in remaining ESS; sub vif rS can be < rS
    rChange = max(0, (vifOut$rS - rS)/(1-rS))
    if (vifOut$rho > 20) {  # could make 20 a user chosen parameter
      if (verbose) {
        cat("**Variable", paste(xIndex, collapse="_"),
            "\n    is collinear; reject w/o penalty.\n")
      }
      iExpert$rejTest(0)  # don't test covariate again
    } else if(rChange < rCrit) {  # fail to reject
      if(verbose) { cat("  rChange", rChange, "<", rCrit, "rCrit\n") }
      iExpert$failTest(cost, rChange)
    } else {  # reject
      if (verbose) {
        cat("  rChange", rChange, ">", "rCrit", rCrit,
            "rCrit\n++Add", featureName(xIndex, theData),"++\n")
      }
      iExpert$rejTest(omega)  # add to wealth, don't test covariate again
      X = cbind(X,x)  # add covariate and index to features
      p = p+1
      theModelFeatures[[p]] = xIndex
      res = .lm.fit(X, y)$residuals
      cpRes = c(crossprod(res))
      if (sigma != "ind") {gWealth$ud_resid(cpRes, sqrt(cpRes/(n-p-1)), n-p-1)}
      rS = 1 - cpRes/TSS
      if (poly) {  # new expert for interactions with rejected feature
        newExp = makeScavengerExpert(gWealth, theModelFeatures,
                                     paste(sort(xIndex), collapse="_"))
        switch(searchType,
               "breadth" = { experts = list(list(newExp), experts) },
               "depth"   = { experts = list(experts, list(newExp)) }
        )
        experts = unlist(experts, recursive=F)
        if (searchType == "breadth") { eIndex = eIndex + 1 }
      }
      sapply(experts, function(e) e$newModel(alg)) # may ud_pass
    }

    # save information before updates -----------------------
    if(save) {
      results[[ntest]] = c(
        ntest   = ntest,
        wealth  = gWealth$state()$wealth,
        expert  = iExpert$name,
        feature = featureName(xIndex, theData),
        bid     = iExpert$state()$bid,
        epoch   = epoch,
        rCrit   = rCrit,
        rS      = rS,
        rChange = rChange,
        rej     = rChange > rCrit
      )
    }

    # remove or update current expert -----------------------------------
    if (iExpert$finished()) {  # drop this expert
      if (verbose) { cat("**Expert", iExpert$name, "finished.\n") }
      experts[[eIndex]] = NULL
      eIndex = NULL
      if(!length(experts)) { cat("No active experts at", ntest+1, "\n"); break }
    } else if (iExpert$finishedPass() && !iExpert$finishedEpoch()) {  # ud pass
      iExpert$ud_pass()
      if (alg != "raiPlus") { iExpert$ud_bidder(1) }  # updates epoch/bid/crit
    }
    # update epochs ------------------------------
    finishedEp = sapply(experts, function(e) e$finishedEpoch())
    if (any(finishedEp)) {
      epochs = sapply(experts, function(e) e$state()$epoch)
      epochCap = ifelse(any(!finishedEp), min(epochs[!finishedEp]), Inf)
      if (alg == "raiPlus") {  # most will be at cap
        expertsMove = experts[which(epochs<=epochCap & finishedEp)]
      } else {
        expertsMove = experts[which(epochs<epochCap & finishedEp)]
      }
      if (length(expertsMove)) {skipEpochs(expertsMove, verbose, alg, epochCap)}
    }
    ntest = ntest+1
  }
  if (ntest == nMaxTest) { cat("Reached maximum number of tests.") }

  # prepare output -----------------------------------------
  colnames(y) = "y"
  if (ncol(X)==0) {
    f = "y ~ 1"
  } else {
    colnames(X) = c("1", mapply(featureName,
                                theModelFeatures, MoreArgs = list(theData)))
    f = paste("y ~", paste(colnames(X),collapse="+"))
  }
  out = list(formula=f, y=y,  X=X, features=theModelFeatures,
             options = list(alg=alg, searchType=searchType, poly=poly))
  if (save) { out$summary = do.call(rbind, results)}

  out
}
