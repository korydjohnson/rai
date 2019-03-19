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
#' @param m number of observations used in subsampling for variance inflation
#'   factor estimate of r.squared.
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
#' @return A list which includes the following components: \item{formula}{final
#'   model formula.} \item{y}{response.} \item{X}{model matrix from final
#'   model.} \item{experts}{list of experts.} \item{theModelFeatures}{list of
#'   feature names in the final model.} \item{options}{options given to RAI:
#'   algorithm, searchType, r, poly}
#' @examples
#'   data("CO2")
#'   theResponse = CO2$uptake
#'   theData = CO2[ ,-5]
#'   rai_out = rai(theData, theResponse)
#'   summarise_rai(rai_out)
#'   raiPlus_out = rai(theData, theResponse, alg="raiPlus")
#'   summarise_rai(raiPlus_out)$cost_raiPlus
#' @export
#' @importFrom stats lm model.matrix pt qt resid var sd .lm.fit

rai = function(theData, theResponse, alpha=.05, alg="rai", r=.8, poly=(alg!="RH"),
               searchType="breadth", m=500, sigma="step", rmse = NA, df=NA,
               omega=alpha, reuse=(alg=="RH"), maxTest=Inf, verbose=F, save=T) {
  stopifnot(searchType %in% c("breadth", "depth") ||
              sigma %in% c("ind", "step") ||
              alg %in% c("rai", "raiPlus", "RH"))
  theData = model.matrix(~. - 1, data=as.data.frame(theData))
  theResponse = as.matrix(theResponse, ncol=1)
  if (sigma == "ind") {
    stopifnot(is.numeric(rmse) && is.numeric(df))
  } else {
    rmse = sd(theResponse)
    df = nrow(theResponse) - 1
  }
  if (poly && alg == "RH") { stop("Cannot do polynomial regression with RH.") }
  if (reuse && alg != "RH") { stop("RAI does not reuse wealth.") }

  gWealth = gWealthStep(alpha, alg, r, var(theResponse)*(nrow(theResponse)-1),
                        ncol(theData), reuse, rmse, df)
  experts = list(makeStepwiseExpert(gWealth, ncol(theData)))
  aucOut = runAuction(experts, gWealth, theData, theResponse, alg, poly,
                      searchType, m, sigma, omega, reuse, maxTest, verbose, save)
  aucOut$options$r = r
  aucOut
}
