# RAI: Outermost Function; Only Exported Function ------------------------------

#' @name RAI
#' @title Main function for RAI regression.
#'
#' @description RAI is a wrapper function to create the required objects and
#'   manage the input/ouptut of the runAuction function. Using poly=F is an
#'   efficient and statistically valid way to run stepwise regression.
#'
#' @param theData matrix of covariates.
#' @param theResponse response vector stored as a single column matrix.
#' @param alpha level of procedure.
#' @param alg algorithm can be one of "rai", "raiPlus", or "RH" (Revisiting
#'   Holm).
#' @param r RAI rejects tests which increase R^2 by a factor r^s, where s is the
#'   epoch.
#' @param poly logical. Should the algorithm look for higher-order polynomials?
#' @param searchType A character string specifying the prioritization of
#'   higher-order polynomials. One of "breadth" (more base features) or "depth"
#'   (higher orders).
#' @param sigma A character string or numeric value. String "ind" uses the rmse
#'   from the full regression model. String "sand" uses sandwich estimation for
#'   each test.
#' @param df if a numeric sigma value is given, must also provide the degrees of
#'   freedom for the estimate. In this case, it is only used initially by the
#'   gWealth object to determine bids and rejection thresholds.
#' @param omega return from rejecting a test in Alpha-Investing (<= alpha).
#' @param reuse logical. Should repeated tests of the same covariate be
#'   considered a test of the same hypothsis? reusing wealth isn't implemented
#'   for RAI or RAIplus as the effect is negligible.
#' @param nMaxTest maximum number of tests.
#' @param verbose logical. Should auction output be prited?
#' @param save logical. Should the auction results be saved? If TRUE, returns a
#'   summary matrix.
#' @return A list which includes the following components: \item{formula}{final
#'   model formula.} \item{y}{response.} \item{X}{model matrix from final
#'   model.} \item{experts}{list of experts.} \item{theModelFeatures}{list of
#'   feature names in the final model.} \item{options}{options given to RAI:
#'   algorithm, searchType, poly}
#' @examples
#' # rai(theData, theResponse)
#' # rai(theData, theResponse, alg="raiPlus")
#' # rai(theData, theResponse, alg="raiPlus", poly=F, verbose=T)  # valid stepwise
#'
#' @export
#' @importFrom stats lm model.matrix pt resid var

rai = function(theData, theResponse, alpha=.05, alg="rai", r=.8, poly=(alg!="RH"),
               searchType="breadth", sigma="sand", df=NA, omega=alpha,
               reuse=(alg=="RH"), nMaxTest=Inf, verbose=F, save=T) {
  stopifnot(searchType %in% c("breadth", "depth") ||
              (sigma %in% c("sand", "ind") | is.numeric(sigma)) ||
              alg %in% c("rai", "raiPlus", "RH"))
  if (poly && alg == "RH") stop("Cannot do polynomial regression with RH.")
  if (reuse && alg != "RH") stop("RAI does not reuse wealth.")
  if (sigma == "sand" && !requireNamespace("sandwich", quietly = TRUE)) {
    stop("Package \"sandwich\" needed for 'sand' SE option.", call. = FALSE)
  }
  if (sigma == "ind" && ncol(theData)>=nrow(theData)) {
    stop("Independent SE fits the full linear model: need n > p.")
  }

  theData = model.matrix(~. - 1, data=as.data.frame(theData))
  theResponse = as.matrix(theResponse, ncol=1)
  if (sigma == "ind") {
    lmFull = lm(theResponse ~ theData, model=F, x=F, qr=F)
    df = lmFull$df.resid
    sigma = sqrt(crossprod(resid(lmFull))/df)
  }
  gWealth = gWealthStep(p=ncol(theData), n=nrow(theData), varY=var(theResponse),
                        alg, sigma, df, r, reuse, alpha)
  experts = list(makeStepwiseExpert(gWealth, ncol(theData), alg, sigma))

  runAuction(experts, gWealth, theData, theResponse, alg, poly, searchType,
             sigma, df, omega, reuse, nMaxTest, verbose, save)
}
