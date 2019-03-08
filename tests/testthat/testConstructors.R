context("Constructor Objects")
library(rai)


# helper functions --------------------------------------------------------
checkPosition = function(object, value) {
  expect_equal(object$state()$position, value)
}
checkPrevPosition = function(object, value) {
  expect_equal(object$state()$prevPosition, value)
}

# tests -------------------------------------------------------------------
rawSource = makeRawSource(10)
test_that("extracting/dropping features works; moving position", {
  checkPosition(rawSource, 10)
  expect_equal(rawSource$feature(), 10)
  checkPosition(rawSource, 9)
  checkPrevPosition(rawSource, 10)
  rawSource$dropLastFeature()
  expect_true(is.na(rawSource$state()$active[11]))
  expect_equal(rawSource$feature(), 9)
  checkPosition(rawSource, 8)
  checkPrevPosition(rawSource, 9)
  rawSource$udPass()
  checkPosition(rawSource, 9)
  expect_true(is.na(rawSource$state()$prevPosition))
  rawSource$udPosition(3)
  checkPosition(rawSource, 3)
  expect_true(is.na(rawSource$state()$prevPosition))
})

# test if can merely add to this for new constructor
TheModelFeatures = list(1,2,3,4,5,6)
scavSource = makeLocalScavenger(TheModelFeatures, "test")
test_that("scavenger tests interactions", {
  checkPosition(scavSource, 6)
  expect_equal(scavSource$feature(), list(6,6))
  checkPosition(scavSource, 5)
  checkPrevPosition(scavSource, 6)
  scavSource$dropLastFeature()
  expect_true(is.na(scavSource$state()$active[7]))
  expect_equal(scavSource$feature(), list(5, 6))
  checkPosition(scavSource, 4)
  checkPrevPosition(scavSource, 5)
  scavSource$udPass()
  checkPosition(scavSource, 5)
  expect_true(is.na(scavSource$state()$prevPosition))
  scavSource$udPosition(3)
  checkPosition(scavSource, 3)
  expect_true(is.na(scavSource$state()$prevPosition))
})
