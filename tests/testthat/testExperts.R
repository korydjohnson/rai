context("Expert Objects")
library(rai)

# set up ------------------------------------------------------------------
data("mtcars")
theResponse = mtcars$mpg
theData = mtcars[ ,-1]
lmSum = summary(lm(theResponse~theData[,1]))
rmse = lmSum$sigma
df = nrow(theData)-2
TSS = var(theResponse)*(nrow(theData)-1)
p = ncol(theData)

# helper function ---------------------------------------------------------
checkWealth = function(object, value) {
  expect_equal(object$state()$wealth, value)
}

# tests -------------------------------------------------------------------
test_that("failTest/rejTest pay/receive correct amounts; set correct tests", {
  gWealth = gWealthStep(.05, "rai", .8, TSS, p, F, rmse, df)
  vifOut = list(1, 1)
  expert = makeStepwiseExpert(gWealth, p)
  checkWealth(gWealth, .05)
  expect_equal(expert$feature(), p)
  expert$failTest(cost=.03, vifOut)
  checkWealth(gWealth, .02)
  expect_equal(expert$feature(), p-1)
  expert$failTest(.02, vifOut)
  checkWealth(gWealth, 0)
  expect_equal(expert$feature(), p-2)
  expert$rejTest(.05)
  checkWealth(gWealth, .05)
  expect_equal(expert$feature(), p-3)
  expert$failTest(.02, vifOut)
})
