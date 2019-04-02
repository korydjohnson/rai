context("rai Wrapper")
library(rai)

# set up ------------------------------------------------------------------

# tests -------------------------------------------------------------------
test_that("no errors", {
  # happens on step 12 of the following data example (colinearity)
  data("mtcars")
  theResponse = mtcars$mpg
  theData = mtcars[ ,-1]
  rai_out = rai(theData, theResponse)
  rai_out = rai(theData, theResponse, alg="raiPlus")
})

test_that("categorical data converted; pass & epoch", {
  data("CO2")
  theResponse = CO2$uptake
  theData = CO2[ ,-5]
  rai_out = rai(theData, theResponse)
  rai_out = rai(theData, theResponse, alg="raiPlus")
})
