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
#' @param sigma A character string or numeric value. String "ind" uses the rmse
#'   from the full regression model. String "sand" uses sandwich estimation for
#'   each test.
#' @param df if a numeric sigma value is given, must also provide the degrees of
#'   freedom for the estimate. df is only overwritten locally if sigma=="sand".
#' @param omega return from rejecting a test in Alpha-Investing.
#' @param reuse logical. Should repeated tests of the same covariate be
#'   considered a test of the same hypothsis? Reusing wealth isn't implemented
#'   for RAI or RAIplus (effect is negligible)
#' @param nMaxTest maximum number of tests
#' @param verbose logical. Should auction output be prited?
#' @param save logical. Should the auction results be saved? If TRUE, returns a
#'   summary matrix.
#' @param indices list of columns of theData used in an interaction.
#' @param X covariates in the current model.
#' @param x covariate being tested for addition into the model.
#' @return A list which includes the following components: \item{formula}{final
#'   model formula.} \item{y}{response.} \item{X}{model matrix from final
#'   model.} \item{experts}{list of experts.} \item{theModelFeatures}{list of
#'   feature names in the final model.} \item{options}{options given to RAI:
#'   algorithm, searchType, poly}

featureName = function(indices, theData) {
  paste(sort(colnames(theData)[unlist(indices)]), collapse="_")
}

#' @name Auction
selectExpert = function(experts, verbose) {
  active  = sapply(experts, function(e) e$finished()) == F
  if (verbose & sum(!active) > 0) {
    cat("Expert ", experts[[which(!active)]]$name, " finished. \n")
  }
  if (sum(active)==0) { list(expertIndex=NULL) }
  epochs = sapply(experts[active],
                  function(e) ifelse(!e$finishedPass(), e$state()$epoch, Inf))
  # max: select from end of experts list; depth/breath builds list differently
  # bid depends on rmse; stepwise prioritizes epoch
  list(expertIndex = max(which(epochs==min(epochs))),  # location among active
       active = active)
}

#' @name Auction
# could replace this with a call to VIF, need all 3 output pieces though
computePval = function(y, X, x, sigma, df) {  # takes a-priori known df
  if(ncol(X)>0) regAdd = lm(y ~ X + x) else regAdd = lm(y ~ x)
  regSum = summary(regAdd)
  if (sigma == "sand") {	# Sandwhich estimation
    df = regAdd$df.residual  # overwrites df *locally*
    sigX = sqrt(sandwich::vcovHC(regAdd)["x", "x"])
  } else {  # compute sigX with known resid se; unscaled is diag of (X'X)^{-1}
    sigX = sigma*sqrt(regSum$cov.unscaled["x", "x"])
  }
  pval = 2*pt(-abs(regAdd$coefficients["x"]/sigX), df)

  list(pval = as.vector(pval),
       rmse = sigX/sqrt(regSum$cov.unscaled["x", "x"]),
       rS   = regSum$r.squared)
}

#' @name Auction
skipEpochs = function(experts, verbose, y, n, p, alg) {
  epochs = sapply(experts, function(e) e$state()$nextEpochInfo$epoch)
  nextEpoch = min(epochs)
  if (verbose) {
    cat("*** Moving experts", sapply(experts, function(e) e$name),
        "to epoch", nextEpoch, "***\n")
  }
  nextExpertIndex = max(which(epochs == nextEpoch))  # max: start at end
  normY = crossprod(y-mean(y))
  # set all to same epoch, but later experts to start of pass, earlier to end
  # if would have been tested and aiPlus, set epoch + 1
  if (nextExpertIndex > 1) {
    nextEpochInfo = list("position"=Inf, "epoch"=nextEpoch)
    sapply(experts[1:(nextExpertIndex-1)],
           function(e) e$setNextTest(normY, n, p, nextEpochInfo))
  }
  if (nextExpertIndex < length(experts)) {
    nextEpochInfo = list("position"=0, "epoch"=nextEpoch+(alg!="raiPlus"))
    sapply(experts[(nextExpertIndex+1):length(experts)],
           function(e) e$setNextTest(normY, n, p, nextEpochInfo))
  }
  experts[[nextExpertIndex]]$setNextTest(normY, n, p)
}

#' @name Auction
runAuction = function(experts, gWealth, theData, y, alg, poly, searchType,
                      sigma, df, omega, reuse, nMaxTest, verbose, save) {
  # placeholders for results ------------------------
  n = nrow(y)
  p = 0
  theModelFeatures = list()
  X = matrix(nrow=n, ncol=p); # selected data columns
  if(save) {
    results = data.frame(matrix(NA, nrow=1000, ncol=9))
    colnames(results) = c("ntest", "Name", "Wealth", "Bidder", "Epoch", "Bid",
                          "Pval", "Rej", "rS")
  }
  # start auction ---------------------------
  ntest = 1
  rS = 0  # initial R^2; will keep track of incremental change
  while (ntest <= nMaxTest) {
    if(verbose) {
      cat("--- Starting ntest ", ntest, " with total wealth ",
          gWealth$state()$wealth, " ---\n")
    }
    if (gWealth$state()$wealth <= 0) {
      if (verbose) cat("Experts have no more wealth. ntest: ", ntest-1, "\n")
      break
    }

    # identify test information   -----------------------------------------
    expertInfo  = selectExpert(experts, verbose)
    experts[!expertInfo$active] = NULL
    expertIndex = expertInfo$expertIndex
    if(is.null(expertIndex)) { cat("No active experts. ntest: ",ntest,"\n"); break }
    iExpert = experts[[expertIndex]]
    xIndex  = iExpert$feature()  # list or vector
    x = apply(theData[ , unlist(xIndex), drop=F], 1, prod)  # feature vector

    # compute p-value, conduct test, implications of test ------------------
    pvalOut = computePval(y, X, x, sigma, df)  # takes a-priori known df
    if(is.null(pvalOut$pval)) { cat("Error: null p-value.\n"); break }

    # can't get bid before pvalue if rmse unknown
    bid     = iExpert$bid(pvalOut$rmse, n-p-1)
    pcrit   = iExpert$pcrit(bid)
    if(verbose) {
      cat("     ", iExpert$name, "epoch", iExpert$state()$epoch,
                    " tests ", unlist(xIndex), "pcrit", pcrit)
    }
    if(is.na(pvalOut$pval)) {  # should really do a check for VIF > 10/20 etc
      if (verbose) {
        cat("\nVariable ", featureName(xIndex, theData),
            " is collinear; reject without penalty.\n")
      }
      iExpert$passTest(0)  # added to global, don't test covariate again
    } else if(pvalOut$pval > pcrit) {  # fail to reject
      if(verbose) cat( " < pval", pvalOut$pval, "\n")
      payment = bid/(1-bid)
      iExpert$failTest(payment, pvalOut$rmse, pvalOut$rS - rS, pvalOut$pval)
    } else {  # reject
      if (verbose) {
        cat( " > pval", pvalOut$pval, " +++ Add ", featureName(xIndex, theData),"\n")
      }
      iExpert$passTest(omega)  # added to global, don't test covariate again
      rS = pvalOut$rS
      sapply(experts, function(e) e$newModel()) # reset all model counters
      X = cbind(X,x)  # add covariate and index to features
      p = p+1
      theModelFeatures[[p]] = unlist(xIndex)
      # if poly=T, new expert for interactions
      if (poly) {
        newExp = makeScavengerExpert(gWealth, theModelFeatures,
                                     featureName(xIndex, theData), alg, sigma)
        switch(searchType,
               "breadth" = { experts = list(list(newExp), experts) },
               "depth"   = { experts = list(experts, list(newExp)) }
        )
        experts = unlist(experts, recursive=F)
      }
    }

    # cave information before updates -----------------------
    if(save) {
      results[ntest, "ntest"]  = ntest
      results[ntest, "Name"]   = featureName(xIndex, theData)
      results[ntest, "Wealth"] = gWealth$state()$wealth
      results[ntest, "Bidder"] = iExpert$name
      results[ntest, "Epoch"]  = iExpert$state()$epoch
      results[ntest, "Bid"]    = bid
      results[ntest, "Pval"]   = pvalOut$pval
      results[ntest, "Rej"]    = pcrit > pvalOut$pval
      results[ntest, "rS"]     = rS
    }

    # update passes, epochs, and skips ----------------------
    if (iExpert$finishedPass()) {
      if (alg!="raiPlus") {  # rai and RH
        iExpert$udPass()
        iExpert$udEpoch(1)
      } else {  # raiPlus
        epochs = sapply(experts, function(e) e$state()$epoch)
        if (iExpert$state()$epoch < max(epochs)) {  # expert is "behind"
          iExpert$udPass()  # cannot update epoch unless finished
        } else {  # if udPass above, can't have all finished
          nFinishedPass = sum(sapply(experts, function(e) e$finishedPass()))
          if (nFinishedPass == length(experts)) {
            sapply(experts, function(e) e$udPass())
          }
        }
      }
    }

    if (iExpert$finishedEpoch()) {  # don't check all experts if this one fails
      if (length(experts) > 1) {  # move to max b/c others may also need to move
        curEpoch = max(sapply(experts[-expertIndex], function(e) e$state()$epoch))
        if (iExpert$state()$epoch < curEpoch) {  # if behind
          if (curEpoch < iExpert$state()$nextEpochInfo$epoch) { # match others
            iExpert$udNextInfo(max(iExpert$state()$active, na.rm=T), curEpoch)
          }
          skipEpochs(list(iExpert), verbose, y, n, p, alg)
        }
      }
      if (all(sapply(experts, function(e) e$finishedEpoch()))) {
        skipEpochs(experts, verbose, y, n, p, alg)
      }
    }
    ntest = ntest+1
  }
  if (ntest == nMaxTest) cat("Reached maximum number of tests.")

  # prepare output -----------------------------------------
  colnames(y) = "y"
  if (ncol(X)==0) {
    f = "y ~ 1"
  } else {
    colnames(X) = mapply(featureName, theModelFeatures, MoreArgs = list(theData))
    f = paste("y ~", paste(colnames(X),collapse="+"))
  }
  out = list(formula=f, y=y,  X=X, experts=experts, features=theModelFeatures,
             options = list(alg=alg, searchType=searchType, poly=poly))
  if (save) { out$summary=results[1:(ntest-1),] }
  out
}
