# RAI: Outermost Function; Only Exported Function ------------------------------

#' @name RAI
#' @title Main function for Revisiting Alpha-Investing (RAI) regression.
#'
#' @description The function rai is a wrapper that creates and manages the
#'   inputs and ouptuts of the \code{\link{runAuction}} function. Using poly=F
#'   is an efficient and statistically valid way to run and terminate stepwise
#'   regression. The function prepareData is provided in order to make
#'   generating predictions on test data easier: it is used by rai to process
#'   the data prior to running, and is necessary to make column names and
#'   information match in order to use the model object returned by rai.
#'
#' @details Missing values are treated as follows: all observations with missing
#'   values in theResponse are removed; numeric columns in theData have missing
#'   values imputed by the mean of the column and an indicator column is added
#'   to note missingness; missing values in factor or binary columns are given
#'   the value "NA", which creates an additional group for missing values. Note
#'   that as rai is run using the output of model.matrix, it is not guaranteed
#'   that all categories from a factor are included in the regression. Column
#'   names may also be modified to be syntactically valid. The model object can
#'   be used to generate predictions on test data. Note that if default
#'   conversions were used when running rai, then they must be used again with
#'   prepareData for the test data prior to producing predictions.
#'
#' @param theData matrix of covariates.
#' @param theResponse response vector or single column matrix.
#' @param alpha level of procedure.
#' @param alg algorithm can be one of "rai", "raiPlus", or "RH" (Revisiting
#'   Holm).
#' @param r RAI rejects tests which increase R^2 by a factor r^s, where s is the
#'   epoch.
#' @param poly logical. Should the algorithm look for higher-order polynomials?
#' @param startDeg This is the starting degree for polynomial regression. It
#'   allows the search to start with lower order polynomials such as square
#'   roots. This alleviates some problems with high-dimensional polynomials as a
#'   4th degree polynomial where startDeg=1/2 is only a quadratic on the
#'   original scale.
#' @param searchType A character string specifying the prioritization of
#'   higher-order polynomials. One of "breadth" (more base features) or "depth"
#'   (higher orders).
#' @param m number of observations used in subsampling for variance inflation
#'   factor estimate of r.squared. Set m=Inf to use full data.
#' @param sigma type of error estimate used; one of "ind" or "step". If "ind",
#'   you must provide a numeric value for rmse and df.
#' @param rmse user provided value for rmse. Must be used with sigma="ind".
#' @param df degrees of freedom for user specified rmse. Must be used with
#'   sigma="ind".
#' @param omega return from rejecting a test in Alpha-Investing (<= alpha).
#' @param reuse logical. Should repeated tests of the same covariate be
#'   considered a test of the same hypothsis? reusing wealth isn't implemented
#'   for RAI or RAIplus as the effect is negligible.
#' @param maxTest maximum number of tests.
#' @param verbose logical. Should auction output be prited?
#' @param save logical. Should the auction results be saved? If TRUE, returns a
#'   summary matrix.
#' @param lmFit The core fuction that will be used to estimate linear model
#'   fits. The default is .lm.fit, but other alternatives are possible. Note
#'   that it does not use formula notation as this is costly. Another
#'   recommended option is fastLmPure from RcppEigen or related packages.
#' @param x an R object.
#' @return A list which includes the following components: \item{formula}{final
#'   model formula.} \item{y}{response.} \item{X}{model matrix from final
#'   model.} \item{features}{list of interactions included in formula.}
#'   \item{summary}{list of feature names in the final model.} \item{time}{run
#'   time.} \item{options}{options given to RAI: alg, searchType, poly, r.}
#'   \item{model}{linear model object using selected model} A summary method is
#'   provided in order to generate futher output and graphics. This summary
#'   method requires the tidyverse package.
#' @examples
#'   data("CO2")
#'   theResponse = CO2$uptake
#'   theData = CO2[ ,-5]
#'   rai_out = rai(theData, theResponse)
#'   summary(rai_out)
#'   raiPlus_out = rai(theData, theResponse, alg="raiPlus")
#'   summary(raiPlus_out)
#' @importFrom stats lm model.matrix pt qt resid var sd .lm.fit

#' @export
prepareData = function(theData, theResponse, poly=T, startDeg=1) {
  if (any(is.na(theData)) || any(is.na(theResponse))) {
    warning("Missing values deteted; applying default conversions and exclusions.
            See documentation for details or remove missing values manually.")
    theData = modMissingData(as.data.frame(theData))
    missResponse = is.na(theResponse)
    theData = theData[!missResponse, ]
    theResponse = theResponse[!missResponse]
  }

  theData = model.matrix(~. - 1, data=as.data.frame(theData))
  colnames(theData) = make.names(colnames(theData))
  if (poly && startDeg!=1) {
    theData = apply(theData, 2, function(col) col^startDeg)
  }
  theResponse = as.matrix(theResponse, ncol=1)
  list(theData = theData, theResponse = theResponse)
}

modMissingCol = function(col) {
  if (is.numeric(col) && length(unique(col)) > 2) {  # numeric
    missing = is.na(col)
    col[missing] = mean(col, na.rm=T)
    list(col, missing)
  } else {  # categorical
    col[is.na(col)] = "NA"
    list(col)
  }
}

modMissingData = function(dataList) {
  out = lapply(dataList,
               function(el) if (any(is.na(el))) modMissingCol(el) else list(el))
  lengths = lapply(out, length)
  dataMod = as.data.frame(out)
  dfNames = mapply(
    function(len, name) if (len==1) name else c(name, paste0(name, "_NA")),
    lengths, names(dataList)
  )
  names(dataMod) = unlist(dfNames)
  dataMod
}

#' @name RAI
#' @export
is.rai = function(x) inherits(x, "rai")

#' @name RAI
#' @export
rai = function(theData, theResponse, alpha=.1, alg="rai", r=.8, poly=alg!="RH",
               startDeg=1, searchType="breadth", m=500, sigma="step", rmse = NA,
               df=NA, omega=alpha, reuse=(alg=="RH"), maxTest=Inf, verbose=F,
               save=T, lmFit = .lm.fit) {
  stopifnot(searchType %in% c("breadth", "depth") ||
              sigma %in% c("ind", "step") ||
              alg %in% c("rai", "raiPlus", "RH"))
  if (poly && alg == "RH") { stop("Cannot do polynomial regression with RH.") }
  if (reuse && alg != "RH") { stop("RAI does not reuse wealth.") }

  dataList = prepareData(theData, theResponse, poly, startDeg)
  theData = dataList$theData; theResponse = dataList$theResponse
  if (sigma == "ind") {
    stopifnot(is.numeric(rmse) && is.numeric(df))
  } else {
    rmse = sd(theResponse)
    df = nrow(theResponse) - 1
  }
  gWealth = gWealthStep(alpha, alg, r, var(theResponse)*(nrow(theResponse)-1),
                        ncol(theData), reuse, rmse, df)
  experts = list(makeStepwiseExpert(gWealth, ncol(theData)))
  timeStart = Sys.time()
  aucOut = runAuction(experts, gWealth, theData, theResponse,
                      alg, poly, searchType, m, sigma, omega, reuse, maxTest,
                      verbose, save, lmFit)
  aucOut$time = Sys.time() - timeStart
  aucOut$options = list(alg=alg, searchType=searchType, poly=poly, r=r)
  aucOut$model = lm(aucOut$formula, data.frame(y=theResponse, theData))
  class(aucOut) = "rai"
  aucOut
}
