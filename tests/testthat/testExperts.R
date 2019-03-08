context("Expert Objects")
library(rai)

# set up ------------------------------------------------------------------
load("theResponse.rda")
load("theData.rda")
full.lm = lm(theResponse ~ theData)
df = full.lm$df.resid; sigma = sqrt(crossprod(resid(full.lm))/df)
n = nrow(theData); p = ncol(theData)

# helper function ---------------------------------------------------------
checkWealth = function(object, value) {
  expect_equal(object$state()$wealth, value)
}

# tests -------------------------------------------------------------------
test_that("failTest/passTest pay/receive correct amounts; set correct tests", {
  gWealth = gWealthStep(ncol(theData), nrow(theData), var(theResponse),
                        alg="rai", sigma, df, r=.8, reuse=F, .05)
  expert = makeStepwiseExpert(gWealth, dim(theData)[2], alg="rai", sigma)
  checkWealth(gWealth, .05)
  expect_equal(expert$feature(), 30)
  expert$failTest(payment=.03, 1, rChange=.1, pval=.3)
  checkWealth(gWealth, .02)
  expect_equal(expert$state()$nextEpochInfo, list(position = 30, epoch = 11))
  expect_equal(expert$feature(), 29)
  expert$failTest(.02, 1, .1, .3)
  checkWealth(gWealth, 0)
  expect_equal(expert$state()$nextEpochInfo, list(position = 30, epoch = 11))
  expect_equal(expert$feature(), 28)
  expert$passTest(.05)
  checkWealth(gWealth, .05)
  expect_equal(expert$feature(), 27)
  expert$failTest(.02, 1, .11, .3)
  expect_equal(expert$state()$nextEpochInfo, list(position = 27, epoch = 10))
})

test_that("setNextTest pays appropriate amount", {
  gWealth = gWealthStep(p=ncol(theData), n=nrow(theData), varY=var(theResponse),
                        sigma=sigma, df=df, r=.8, alg="RH", reuse=F, wealth=.05)
  expert = makeStepwiseExpert(gWealth, ncol(theData), alg, sigma)
  checkWealth(gWealth, .05)
  expert$udNextInfo(30, 2)
  expect_equal(expert$state()$nextEpochInfo, list(position=30, epoch=2))
  expert$setNextTest(1, n, p)
  expect_equal(expert$state()$position, 30)
  expect_equal(expert$state()$epoch, 2)
  checkWealth(gWealth, 0)  # 30 tests failed at bonferroni level -> no wealth with RH
  expert$udNextInfo(1, 3)
  expect_equal(expert$state()$nextEpochInfo, list(position=1, epoch=3))
  expert$setNextTest(1, n, p)
  expect_equal(expert$state()$position, 1)
  expect_equal(expert$state()$epoch, 3)
  # paid for all of epoch 2, and 29 tests of epoch 3
  cost = as.vector(crossprod(c(30, 29), c(.05/29, .05/28)))
  checkWealth(gWealth, -cost)
  # now skipping epochs; set wealth back to 0
  expert$bidAccepted(-gWealth$state()$wealth)
  checkWealth(gWealth, 0)
  expert$udNextInfo(1, 5)
  expect_equal(expert$state()$nextEpochInfo, list(position=1, epoch=5))
  expert$setNextTest(1, n, p)
  expert$state()
  # paid for 1 test in 3, 30 in epoch 4, 29 tests in epoch 5
  cost = as.vector(crossprod(c(1, 30, 29), .05/((p-2):(p-4))))
  checkWealth(gWealth, -cost)
})
