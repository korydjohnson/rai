# Functions for Processing Auction Ouptut ---------------------------------
#' @name ProcessRAI
#' @title Summarising RAI Output
#'
#' @description Processes the output from the \code{\link{rai}} function.
#'   Requires dplyr, tibble, and ggplot2 packages.
#'
#' @param rai_out list output from \code{\link{rai}} function.
#' @param rawSum processed version of rai summary stored as a tibble with
#'   correct column parsing.
#' @return A list which includes the following components: \item{plot_rS}{plot
#'   of the change in r.squared over time (number of tests conducted).}
#'   \item{plot_wealth}{plot of the change in r.squared over time (number of
#'   tests conducted).} \item{experts}{summary of expert performance: number of
#'   features, number of rejections, order in which they were addedto the expert
#'   list.} \item{tests}{table of number of times features were tested: how many
#'   features tested k times; which expert(s) conducted tests.} \item{epochs}{in
#'   which epochs were tests rejected and the corresponding rejection
#'   thresholds.} \item{stats}{summary statistics: number of tests, number of
#'   epochs, bound on percentage reduction in ESS by adding a single feature,
#'   number of passes through to features, final r.squared, cost of raiPlus (0
#'   for rai).}\item{options}{options given to RAI: algorithm, searchType, r, poly.}

#' @examples
#'   data("CO2")
#'   theResponse = CO2$uptake
#'   theData = CO2[ ,-5]
#'   rai_out = rai(theData, theResponse)
#'   summarise_rai(rai_out)
#'   raiPlus_out = rai(theData, theResponse, alg="raiPlus")
#'   summarise_rai(raiPlus_out)$cost_raiPlus

#' @importFrom dplyr as_tibble %>% mutate_all summarise group_by mutate arrange
#' @importFrom dplyr ungroup pull
#' @importFrom readr parse_guess
#' @importFrom ggplot2 ggplot geom_line aes scale_y_continuous labs

plot_ntest_rS = function(rawSum) {
  ggplot(rawSum) +
    geom_line(aes(ntest, rS)) +
    scale_y_continuous(limits=c(0,1)) +
    labs(title =  paste("Improvement in", expression(R^2)),
         x = "Number of Tests", y = expression(R^2))
}

#' @name ProcessRAI
plot_ntest_wealth = function(rawSum) {
  ggplot(rawSum) +
    geom_line(aes(ntest, wealth)) +
    labs(title =  paste("Change in Wealth"),
         x = "Number of Tests", y = "Wealth")
}

#' @name ProcessRAI
#' @export
summarise_rai = function(rai_out) {
  stats = list()
  rawSummary = rai_out$summary %>%
    as_tibble() %>%
    mutate_all(parse_guess)

  expertSum = rawSummary %>%
    group_by(expert) %>%
    summarise(nRej = sum(rej),
              nFeatures = length(unique(feature))) %>%
    mutate(order = c(1, rank(nFeatures[-1])+1)) %>%
    arrange(order)
  testSum = rawSummary %>%
    group_by(feature) %>%
    summarise(timesTested = n(),
              nExperts = length(unique(expert)),
              expert = unique(expert)[1]) %>%
    group_by(timesTested) %>%
    summarise(count = n(),
              nExperts = length(unique(expert)),
              expert = unique(expert)[1]) %>%
    arrange(desc(timesTested))
  epochSum = rawSummary %>%
    group_by(epoch) %>%
    summarise(rCrit = unique(rCrit),
              nRej = sum(rej)) %>%
    arrange(epoch)
  cost_raiPlus = rawSummary %>%
    # group by expert b/c can have two experts test same feature
    # possible for 3rd order: 1,2;  2,3: both test 1,2,3
    group_by(epoch, expert, feature) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    summarise(sum(count)-n()) %>%
    pull()
  stats$nTests = max(rawSummary$ntest)
  stats$nEpochs = max(rawSummary$epoch)
  stats$nPasses = max(testSum$timesTested)
  stats$maxImprovement = rai_out$options$r^(stats$nEpochs-1)
  stats$nFeatures = length(rai_out$theModelFeatures)
  stats$rS = max(rawSummary$rS)
  stats$cost_raiPlus =
    list(extraTests = cost_raiPlus,
         prop_rai   = cost_raiPlus/(stats$nTests-cost_raiPlus))
  list(plot_rS  = plot_ntest_rS(rawSummary),
       plot_wealth = plot_ntest_wealth(rawSummary),
       experts  = expertSum,
       tests    = testSum,
       epochs   = epochSum,
       stats    = stats)
}
