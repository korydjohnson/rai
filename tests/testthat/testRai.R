context("rai Wrapper")
library(rai)

# set up ------------------------------------------------------------------

# tests -------------------------------------------------------------------
test_that("no errors", {
  # happens on step 12 of the following data example (colinearity)
  data("mtcars")
  theResponse = mtcars$mpg
  theData = mtcars[ ,-1]
  auc1 = rai(theData, theResponse)
  auc2 = rai(theData, theResponse, alg="raiPlus")
})

test_that("categorical data converted; pass & epoch", {
  # test 51: epoch is over, but others not, only ud pass
  # test 106: move expert to current test (cost 0)
  data("CO2")
  theResponse = CO2$uptake
  theData = CO2[ ,-5]
  auc_rai <<- rai(theData, theResponse, save=T)
  auc_raiPlus <<- rai(theData, theResponse, alg="raiPlus", save=T)
})

test_that("rai/raiPlus test differently", {
  raiSum = summarise_rai(auc_rai)
  raiPlusSum = summarise_rai(auc_raiPlus)
  expect_equal(raiSum$stats$cost_raiPlus,
               list(extraTests = 0, prop_rai = 0))
  expect_equal(raiPlusSum$stats$cost_raiPlus$extraTests, 39)
})

