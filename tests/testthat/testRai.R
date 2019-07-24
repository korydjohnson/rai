context("rai Wrapper")
library(rai)

# set up ------------------------------------------------------------------
data("CO2")

# tests -------------------------------------------------------------------
test_that("no errors", {
  # happens on step 12 of the following data example (colinearity)
  data("mtcars")
  theResponse = mtcars$mpg
  theData = mtcars[ ,-1]
  rai_out = rai(theData, theResponse)
  rai_out = rai(theData, theResponse, alg="raiPlus")
})

test_that("prepareData for numeric matrix", {
  theData = matrix(1:10, nrow=2)
  theData_mm = prepareData(theData)
  attributes(theData_mm)$dimnames = NULL
  expect_identical(theData, theData_mm)
})

test_that("prepareData for data.frame", {
  theData = head(CO2)
  theData_mm = prepareData(theData)
  theData_mm2 = model.matrix(~.-1, data=theData)
  theData_mm2 = cbind(theData_mm2[,c(15,16)], theData_mm2[,-c(15,16)])
  expect_identical(theData_mm, theData_mm2)
})

test_that("modMissing works", {
  theData = head(CO2)
  theData[1,1] = NA
  theData[1,5] = NA
  theData_mm = prepareData(theData)
  expect_equal(ncol(theData_mm), 18)
  expect_equal(theData_mm[1,"uptake"], mean(theData[-1,"uptake"]))
  expect_equal(sum(abs(theData_mm[,"PlantNA"])-c(1,rep(0,5))), 0)
})

test_that("categorical data converted; pass & epoch", {
  theResponse = CO2$uptake
  theData = CO2[ ,-5]
  rai_out = rai(theData, theResponse)
  rai_out = rai(theData, theResponse, alg="raiPlus", verbose=T)
})
