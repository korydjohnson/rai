context("processResults")
library(rai)

# set up ------------------------------------------------------------------
data("CO2")
theResponse = CO2$uptake
theData = CO2[ ,-5]
rai_out = rai(theData, theResponse)

# tests -------------------------------------------------------------------
test_that("summary method", {
  expect_equal(length(summary(rai_out)), 6)  # summary information including graphs
})

test_that("predict method", {
  expect_identical(predict(rai_out), predict(rai_out$model))
  newdata = theData[sample.int(nrow(theData)),]
  expect_identical(predict(rai_out, newdata),
                   predict(rai_out$model, as.data.frame(prepareData(newdata))))
})

test_that("predict method with alpha", {
  rai.01 = rai(theData, theResponse, alpha=.01)
  expect_identical(predict(rai_out, alpha=.01), predict(rai.01))
})
