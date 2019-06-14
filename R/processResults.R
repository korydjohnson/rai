# Functions for Processing Auction Ouptut ---------------------------------
#' @name ProcessRAI
#' @title Summarising RAI Output
#'
#' @description Processes the output from the \code{\link{rai}} function.
#'   Requires dplyr, tibble, and ggplot2 packages.
#'
#' @param object an object of class rai; expected to be the list output from the
#' \code{\link{rai}} function.
#' @param rawSum processed version of rai summary stored as a tibble with
#'   correct column parsing.
#' @param newdata an optional data frame in which to look for variables with
#'   which to predict. If omitted, the fitted values are used.
#' @param ... additional arguments affecting the summary or predict methods.
#' @return A list which includes the following components: \item{plot_rS}{plot
#'   of the change in r.squared over time (number of tests conducted).}
#'   \item{plot_wealth}{plot of the change in r.squared over time (number of
#'   tests conducted).} \item{experts}{summary of expert performance: number of
#'   features, number of rejections, order in which they were added to the expert
#'   list.} \item{tests}{table of number of times features were tested: how many
#'   features tested k times; which expert(s) conducted tests.} \item{epochs}{in
#'   which epochs were tests rejected and the corresponding rejection
#'   thresholds.} \item{stats}{summary statistics: number of tests, number of
#'   epochs, bound on percentage reduction in ESS by adding a single feature,
#'   number of passes through to features, final r.squared, cost of raiPlus (0
#'   for rai).}\item{options}{options given to RAI: algorithm, searchType, poly,
#'   startDegree, r.}

#' @examples
#'   data("CO2")
#'   theResponse = CO2$uptake
#'   theData = CO2[ ,-5]
#'   rai_out = rai(theData, theResponse)
#'   summary(rai_out)  # summary information including graphs
#'   predict(rai_out)  # fitted values from selected model

#' @importFrom dplyr as_tibble %>% mutate_all summarise group_by mutate arrange
#' @importFrom dplyr ungroup pull select filter desc n
#' @importFrom readr parse_guess
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_line aes_string scale_y_continuous labs
#' @importFrom stats predict

plot_ntest_rS = function(rawSum) {
  ggplot(rawSum) +
    geom_line(aes_string("ntest", "rS")) +
    scale_y_continuous(limits=c(0,1)) +
    labs(title =  paste("Improvement in", expression(R^2)),
         x = "Number of Tests", y = expression(R^2))
}

#' @name ProcessRAI
plot_ntest_wealth = function(rawSum) {
  ggplot(rawSum) +
    geom_line(aes_string("ntest", "wealth")) +
    labs(title =  paste("Change in Wealth"),
         x = "Number of Tests", y = "Wealth")
}

#' @name ProcessRAI
#' @export
predict.rai = function(object, newdata=NULL, ...) {
  if (!is.null(newdata)) {
    newdata = prepareData(newdata, object$options$poly, object$options$startDeg)
    predict(object$model, as.data.frame(newdata), ...)
  } else {
    predict(object$model, ...)
  }
}

#' @name ProcessRAI
#' @export
summary.rai = function(object, ...) {
  stats = list()
  rawSummary = object$summary %>%
    as_tibble() %>%
    mutate_all(parse_guess)

  expertSum = rawSummary %>%
    group_by(.data$expert) %>%
    summarise(nRej = sum(.data$rej),
              nFeatures = length(unique(.data$feature))) %>%
    mutate(order = c(1, rank(.data$nFeatures[-1])+1)) %>%
    arrange(.data$order)
  testSum = rawSummary %>%
    group_by(.data$feature) %>%
    summarise(timesTested = n(),
              nExperts = length(unique(.data$expert)),
              expert = unique(.data$expert)[1]) %>%
    group_by(.data$timesTested) %>%
    summarise(count = n(),
              nExperts = length(unique(.data$expert)),
              expert = unique(.data$expert)[1]) %>%
    arrange(desc(.data$timesTested))
  epochSum = rawSummary %>%
    group_by(.data$epoch) %>%
    summarise(rCrit = unique(.data$rCrit),
              nRej = sum(.data$rej),
              max_rS = max(.data$rS)) %>%
    arrange(.data$epoch)
  maxEp = max(rawSummary$epoch)
  stats$maxPotentialIncrease_raiPlus = object$options$r^(maxEp-1) *
    (1 - max(filter(rawSummary, .data$epoch==maxEp-1)$rS))
  stats$nTests = max(rawSummary$ntest)
  stats$nEpochs = max(rawSummary$epoch)
  stats$nFeatures = length(object$features)
  degree = sapply(object$features, length)
  nUniqueFeatures = sapply(object$features, function(vec) length(unique(vec)))
  stats$poly = list(tableDegrees = as.data.frame(table(degree)),
                    tableInteraction = as.data.frame(table(nUniqueFeatures)))
  stats$rS = max(rawSummary$rS)
  stats$nFeaturesTested = length(unique(object$summary[ ,"feature"]))
  list(plot_rS  = plot_ntest_rS(rawSummary),
       plot_wealth = plot_ntest_wealth(rawSummary),
       experts  = expertSum,
       tests    = testSum,
       epochs   = select(epochSum, -.data$max_rS),
       stats    = stats)
}
