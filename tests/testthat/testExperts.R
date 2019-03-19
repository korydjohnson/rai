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
  expert = makeStepwiseExpert(gWealth, p)
  checkWealth(gWealth, .05)
  expect_equal(expert$feature(), p)
  expert$failTest(cost=.03, rChange=.1)
  checkWealth(gWealth, .02)
  expect_equal(expert$state()$nextEpochInfo, list(position = p, epoch = 11))
  expect_equal(expert$feature(), p-1)
  expert$failTest(.02, .1)
  checkWealth(gWealth, 0)
  expect_equal(expert$state()$nextEpochInfo, list(position = p, epoch = 11))
  expect_equal(expert$feature(), p-2)
  expert$rejTest(.05)
  checkWealth(gWealth, .05)
  expect_equal(expert$feature(), p-3)
  expert$failTest(.02, .11)
  expect_equal(expert$state()$nextEpochInfo, list(position = p-3, epoch = 10))
})

test_that("setNextTest pays appropriate amount", {
  gWealth = gWealthStep(.05, "RH", .8, TSS, p, F, rmse, df)
  expert = makeStepwiseExpert(gWealth, p)
  feature = expert$feature()
  feature = expert$feature()
  expect_equal(expert$state()$position, feature-1)
  expect_equal(expert$state()$epoch, 1)
  expert$failTest(.05, .08)
  checkWealth(gWealth, 0)
  expect_equal(expert$state()$nextEpochInfo, list(position=p-1, epoch=3))
  expert$setNextTest()
  expect_equal(expert$state()$position, p-1)
  expect_equal(expert$state()$epoch, 3)
  # paid for (p-2) epoch 1, all of epoch 2, and 1 tests of epoch 3
  cost = as.vector(crossprod(c(p-2, p, 1), c(.05/p, .05/(p-1), .05/(p-2))))
  checkWealth(gWealth, -cost)
  expert$bidAccepted(cost)
  checkWealth(gWealth, 0)
  # movement in same epoch
  expert$newModel(alg="rai")
  feature = expert$feature()
  feature = expert$feature()
  feature = expert$feature()
  expect_equal(expert$state()$position, feature-1)
  expect_equal(expert$state()$epoch, 3)
  expert$failTest(0, .08)
  expert$ud_pass()
  expect_equal(expert$state()$nextEpochInfo, list(position=feature, epoch=3))
  expect_equal(expert$state()$position, p)
  expect_equal(expert$state()$epoch, 3)
  expert$setNextTest()
  # 3 tests in 3rd pass
  cost = (p-feature)*.05/(p-2)
  checkWealth(gWealth, -cost)
})
