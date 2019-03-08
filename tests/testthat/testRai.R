context("rai Wrapper")
library(rai)

# set up ------------------------------------------------------------------
load("theData.rda")

# helper function ---------------------------------------------------------
checkAuc = function(alg, poly, sigma) {
  if (poly) {
    load("theResponsePoly.rda")
  } else {
    load("theResponse.rda")
  }
  file = paste0(alg, "_", if (poly) "poly_", sigma, ".rda")
  load(file)
  auc = rai(theData, theResponse, alg=alg, poly=poly, sigma=sigma, save=T)
  expect_identical(auc$summary, summary)
}

# tests -------------------------------------------------------------------

test_that("rai wrapper gives identical output as previous version", {
  checkAuc("rai", F, "ind")
  checkAuc("rai", F, "sand")
  checkAuc("rai", T, "sand")
  checkAuc("raiPlus", F, "ind")
  checkAuc("raiPlus", F, "sand")
  checkAuc("raiPlus", T, "sand")
  checkAuc("RH", F, "ind")
  checkAuc("RH", F, "sand")
})
