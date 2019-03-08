context("Internal Auction Functions")
library(rai)

# set up ------------------------------------------------------------------
load("theResponse.rda")
load("theData.rda")
set_up = function(alg) {
  full.lm = lm(theResponse ~ theData)
  df = full.lm$df.resid; sigma = sqrt(crossprod(resid(full.lm))/df)
  n <<- nrow(theData); p <<- ncol(theData)
  gWealth <<- gWealthStep(ncol(theData), nrow(theData), var(theResponse),
                        alg=alg, sigma, df, r=.8, reuse=F, .05)
  theModelFeatures = list(1)
  # first scavenger tests interactions with 1
  experts = list(
    makeScavengerExpert(gWealth, theModelFeatures, "test", alg="rai", sigma),
    makeStepwiseExpert(gWealth, dim(theData)[2], alg="rai", sigma)
  )
  theModelFeatures[1+length(theModelFeatures)] = 7
  # second tests interactions with 7
  newExpert = makeScavengerExpert(gWealth, theModelFeatures, "test2", alg="rai", sigma)
  experts = unlist(list(list(newExpert), experts), recursive=F)
  experts[[2]]$udEpoch(3)
  experts[[3]]$udEpoch(3)
  experts <<- experts
}

# tests -------------------------------------------------------------------
test_that("selectExperts selects/removes appropriate experts", {
  set_up("rai")
  expect_equal(sapply(experts, function(e) e$state()$epoch), c(1, 4, 4))
  expect_equal(selectExpert(experts, verbose=F),
               list(expertIndex=1, active=c(T, T, T)))
  experts[[1]]$feature()
  experts[[1]]$passTest(0)
  expect_equal(selectExpert(experts, verbose=F),
               list(expertIndex=1, active=c(T, T, T)))
  experts[[1]]$feature()
  experts[[1]]$passTest(0)
  expertInfo = selectExpert(experts, verbose=F)
  expect_equal(expertInfo,
               list(expertIndex=2, active=c(F, T, T)))
  experts[!expertInfo$active] = NULL
  iExpert = experts[[expertInfo$expertIndex]]
  expect_equal(experts[[2]]$name, "Step Marginal")
  experts[[2]]$setNextTest(1, n, p, list("position"=1,"epoch"=5))
  expect_equal(selectExpert(experts, verbose=F),
               list(expertIndex=1, active=c(T, T)))
})

test_that("skipEpochs skips correct sets for rai", {
  alg="rai"
  set_up(alg)
  experts[[2]]$udNextInfo(1, 7)
  baseSoln = sapply(experts, function(e) e$state()$nextEpochInfo)
  skipEpochs(experts, F, theResponse, n, p, alg)
  baseSoln[1:6] = rep(c(0, Inf), 3)
  expect_equal(sapply(experts, function(e) e$state()$nextEpochInfo), baseSoln)
  expect_equal(sapply(experts, function(e) e$state()$position), c(2, 1, 0))
  expect_equal(sapply(experts, function(e) e$state()$epoch), c(7, 7, 8))
})

test_that("skipEpochs skips correct sets for raiPlus", {
  alg="raiPlus"
  set_up(alg)
  experts[[2]]$udNextInfo(1, 7)
  baseSoln = sapply(experts, function(e) e$state()$nextEpochInfo)
  skipEpochs(experts, F, theResponse, n, p, alg="raiPlus")
  baseSoln[1:6] = rep(c(0, Inf), 3)
  expect_equal(sapply(experts, function(e) e$state()$nextEpochInfo), baseSoln)
  expect_equal(sapply(experts, function(e) e$state()$position), c(2, 1, 0))
  expect_equal(sapply(experts, function(e) e$state()$epoch), c(7, 7, 7))
})
