# Functions for Processing Auction Ouptut ---------------------------------
# Need to run rai with save=T to have required input

simpleModel = function() {
  load("tests/testthat/theData.rda")
  load("tests/testthat/theResponsePoly.rda")
  auc = rai(theData, theResponse, alg = "raiPlus", save=T)
}

# questions
# how does rS increase over time: number of tests, passes, number of rejections
# where do tests occur, how many of them
# which epochs do they occur in
# how often do epochs skip etc
# what does wealth look like over time
plot1 = function(auc) {
  ggplot(auc$summary) +
    geom_line(aes(ntest, rS)) +
    labs(title =  paste("Improvement in", expression(R^2)),
         x = "Number of Tests", y = expression(R^2))
  # can't group by epoch (scavengers start at 1)
  auc$summary %>%
    group_by(Epoch) %>%
    summarize(rS = max(rS)) %>%
    ggplot() +
    geom_line(aes(Epoch, rS)) +
    labs(title =  paste("Improvement in", expression(R^2)),
         x = "Epoch (Pass Through the Covariates)", y = expression(R^2))

}

plot2 = function(auc) {
  auc$summary %>%
    mutate(nrej = cumsum(Rej))
}
