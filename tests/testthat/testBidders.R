context("Bidder Objects")
library(rai)

# set up ------------------------------------------------------------------
load("theResponse.rda")
load("theData.rda")
full.lm = lm(theResponse ~ theData)
df = full.lm$df.resid; sigma = sqrt(crossprod(resid(full.lm))/df)

# helper function ---------------------------------------------------------
checkWealth = function(object, value) {
  expect_equal(object$state()$wealth, value)
}

# tests -------------------------------------------------------------------
gWealth = gWealthStep(ncol(theData), nrow(theData), var(theResponse),
                      alg="rai", sigma, df, r=.8, reuse=F, .05)
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
  bid = stepBid$bid(1, 1)
  expect_equal(gWealth$bid(1, 1, 1), bid)
  pcrit = stepBid$pcrit(bid)
  expect_equal(gWealth$pcrit(bid, 1), pcrit)
  stepBid$udEpoch(10)
  expect_equal(stepBid$state()$epoch, 11)
  bid = stepBid$bid(1, 1)
  expect_equal(gWealth$bid(11, 1, 1), bid)
  pcrit = stepBid$pcrit(bid)
  expect_equal(gWealth$pcrit(bid, 11), pcrit)
})
