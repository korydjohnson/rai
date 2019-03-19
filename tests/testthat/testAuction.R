context("Internal Auction Functions")
library(rai)

# set up ------------------------------------------------------------------
set_up = function(alg) {
  data("mtcars")
  theResponse = mtcars$mpg
  theData = mtcars[ ,-1]
  lmFull = lm(theResponse ~ ., data=theData)
  df = lmFull$df.resid; rmse = summary(lmFull)$sigma
  n <<- nrow(theData); p <<- ncol(theData)
  TSS <<- var(theResponse)*(n-1)
  gWealth <<- gWealthStep(.05, alg, .8, TSS, p, F, rmse, df)
  theModelFeatures = list(1)
  # first scavenger tests interactions with 1
  experts = list(
    makeScavengerExpert(gWealth, theModelFeatures, "test"),
    makeStepwiseExpert(gWealth, p)
  )
  theModelFeatures[1+length(theModelFeatures)] = 7
  # second tests interactions with 7
  newExpert = makeScavengerExpert(gWealth, theModelFeatures, "test2")
  experts = unlist(list(list(newExpert), experts), recursive=F)
  experts[[2]]$ud_bidder(3)
  experts[[3]]$ud_bidder(3)
  experts <<- experts
}

# tests -------------------------------------------------------------------
test_that("skipEpochs skips correct sets for rai", {
  alg="rai"
  set_up(alg)
  env = environment(experts[[2]]$newModel)
  env$nextEpochInfo = list(position = 1, epoch = 7)
  baseSoln = sapply(experts, function(e) e$state()$nextEpochInfo)
  skipEpochs(experts, F, alg, Inf)
  baseSoln[1:6] = rep(c(0, Inf), 3)
  expect_equal(sapply(experts, function(e) e$state()$nextEpochInfo), baseSoln)
  expect_equal(sapply(experts, function(e) e$state()$position), c(2, 1, 0))
  expect_equal(sapply(experts, function(e) e$state()$epoch), c(7, 7, 8))
})

test_that("skipEpochs skips correct sets for raiPlus", {
  alg="raiPlus"
  set_up(alg)
  env = environment(experts[[2]]$newModel)
  env$nextEpochInfo = list(position = 1, epoch = 7)
  baseSoln = sapply(experts, function(e) e$state()$nextEpochInfo)
  skipEpochs(experts, F, alg="raiPlus", Inf)
  baseSoln[1:6] = rep(c(0, Inf), 3)
  expect_equal(sapply(experts, function(e) e$state()$nextEpochInfo), baseSoln)
  expect_equal(sapply(experts, function(e) e$state()$position), c(2, 1, 0))
  expect_equal(sapply(experts, function(e) e$state()$epoch), c(7, 7, 7))
})


# VIF setup ---------------------------------------------------------------

n = m = 10; p = 1
X = cbind(1, as.matrix(rnorm(n)))
x = as.matrix(rnorm(n))
y = as.matrix(X[,2] + rnorm(n))
res = as.matrix(resid(lm(y~X)))
TSS = var(y)*(n-1)

# VIF tests ---------------------------------------------------------------

test_that("vif returns appropriate values, including under multicollinearity", {
  vifOut = vif(res, y, X, x, n, p, m, TSS)
  expect_equal(c(vifOut$rS), c(summary(lm(y~X+x))$r.squared))
  expect_equal(c(vifOut$rho), 1/(1 - summary(lm(x~X))$r.squared))
  x = X[,2]
  vifOut = vif(res, y, X, x, n, p, m, TSS)
  expect_equal(c(vifOut$rS), c(summary(lm(y~X))$r.squared))
  expect_equal(vifOut$rho, 10^2)
  m = 5
  # has to subsample here
  vifOut = vif(res, y, X, x, n, p, m, TSS)
})

# should have test for rS>=0; actually, left negative, put bound elsewhere
