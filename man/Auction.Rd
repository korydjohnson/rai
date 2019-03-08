% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auction.R
\name{Auction}
\alias{Auction}
\alias{featureName}
\alias{selectExpert}
\alias{computePval}
\alias{skipEpochs}
\alias{runAuction}
\title{Internal function to manage multiple experts.}
\usage{
featureName(indices, theData)

selectExpert(experts, verbose)

computePval(y, X, x, sigma, df)

skipEpochs(experts, verbose, y, n, p, alg)

runAuction(experts, gWealth, theData, y, alg, poly, searchType, sigma, df,
  omega, reuse, nMaxTest, verbose, save)
}
\arguments{
\item{indices}{list of columns of theData used in an interaction.}

\item{theData}{covariate matrix.}

\item{experts}{list of expert objects. Each expert is the output of
makeStepwiseExpert or makeScavengerExpert.}

\item{verbose}{logical. Should auction output be prited?}

\item{y}{the response as a single column matrix.}

\item{X}{covariates in the current model.}

\item{x}{covariate being tested for addition into the model.}

\item{sigma}{A character string or numeric value. String "ind" uses the rmse
from the full regression model. String "sand" uses sandwich estimation for
each test.}

\item{df}{if a numeric sigma value is given, must also provide the degrees of
freedom for the estimate. df is only overwritten locally if sigma=="sand".}

\item{n}{number of observations.}

\item{p}{number of predictors in the \emph{current} model.}

\item{alg}{algorithm can be one of "rai", "raiPlus", or "RH" (Revisiting
Holm).}

\item{gWealth}{global wealth object, output of gWealthStep.}

\item{poly}{logical. Should the algorithm look for higher-order polynomials?}

\item{searchType}{A character string specifying the prioritization of
higher-order polynomials. One of "breadth" (more base features) or "depth"
(higher order).}

\item{omega}{return from rejecting a test in Alpha-Investing.}

\item{reuse}{logical. Should repeated tests of the same covariate be
considered a test of the same hypothsis? Reusing wealth isn't implemented
for RAI or RAIplus (effect is negligible)}

\item{nMaxTest}{maximum number of tests}

\item{save}{logical. Should the auction results be saved? If TRUE, returns a
summary matrix.}
}
\value{
A list which includes the following components: \item{formula}{final
  model formula.} \item{y}{response.} \item{X}{model matrix from final
  model.} \item{experts}{list of experts.} \item{theModelFeatures}{list of
  feature names in the final model.} \item{options}{options given to RAI:
  algorithm, searchType, poly}
}
\description{
runAuction is the workhorse of the rai package: it takes an
  initial expert list and runs the Revisiting Alpha-Investing algorithm to
  greedily fit (optional) polnomials to data. The term "auction" is the
  result of multiple experts bidding to perform the test which determines
  stepwise ordering. This function is not intended to be called directly, but
  through \code{\link{rai}}.
}