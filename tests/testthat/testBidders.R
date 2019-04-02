context("Bidder Objects")
library(rai)

# set up ------------------------------------------------------------------
data("mtcars")
theResponse = mtcars$mpg
theData = mtcars[ ,-1]
lmSum = summary(lm(theResponse~theData[,1]))
rmse = lmSum$sigma
df = nrow(theData)-2
TSS = var(theResponse)*(nrow(theData)-1)

# helper function ---------------------------------------------------------
checkWealth = function(object, value) {
  expect_equal(object$state()$wealth, value)
}

# tests -------------------------------------------------------------------
gWealth = gWealthStep(.05, "rai", .8, TSS, ncol(theData), F, rmse, df)
test_that("wealth initialized and manipulated properly", {
  checkWealth(gWealth, .05)
  gWealth$bidAccepted(.05)
  checkWealth(gWealth, .1)
  gWealth$bidRejected(.05)
  checkWealth(gWealth, .05)
})

stepBid = makeStepwiseBidder(gWealth)
stepBid2 = makeStepwiseBidder(gWealth)
test_that("stepwise bidder manages global wealth object", {
  checkWealth(stepBid, .05)
  stepBid$bidAccepted(.05)
  checkWealth(gWealth, .1)
  checkWealth(gWealth, stepBid$state()$wealth)
  stepBid$bidRejected(.07)
  checkWealth(gWealth, .03)
  checkWealth(gWealth, stepBid$state()$wealth)
  stepBid2$bidAccepted(.1)
  checkWealth(gWealth, .13)
  checkWealth(stepBid, .13)
  checkWealth(stepBid2, .13)
})

test_that("stepwise bidder and global wealth bid/pcrit correctly", {
  expect_equal(stepBid$state()$epoch, 1)
  stepBid$ud_bidder()
  expect_equal(stepBid$state()$rCrit, stepBid$state()$rVec[1])
  stepBid$ud_bidder(5)
  expect_equal(stepBid$state()$rCrit, gWealth$state()$rVec[6])
  bid = 2*pt(-sqrt(lmSum$r.squared)*sqrt(TSS)/rmse, df)
  rCrit = qt(lmSum$coefficients[2, 4]/2, df)^2*rmse^2/TSS
  expect_equal(bid, lmSum$coefficients[2, 4])
  expect_equal(rCrit, lmSum$r.squared)
})
