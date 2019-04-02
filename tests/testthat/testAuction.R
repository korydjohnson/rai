context("Internal Auction Functions")
library(rai)

# VIF setup ---------------------------------------------------------------
n = m = 10; p = 1
X = cbind(1, as.matrix(rnorm(n)))
x = as.matrix(rnorm(n))
y = as.matrix(X[,2] + rnorm(n))
res = as.matrix(resid(lm(y~X)))
TSS = var(y)*(n-1)

# VIF tests ---------------------------------------------------------------

test_that("vif returns appropriate values, including under multicollinearity", {
  vifOut = vif(res, y, X, x, n, p, m, TSS, lmFit = .lm.fit)
  expect_equal(c(vifOut$rS), c(summary(lm(y~X+x))$r.squared))
  expect_equal(c(vifOut$rho), 1/(1 - summary(lm(x~X))$r.squared))
  x = X[ , 2, drop=F]
  vifOut = vif(res, y, X, x, n, p, m, TSS, lmFit = .lm.fit)
  expect_equal(c(vifOut$rS), c(summary(lm(y~X))$r.squared))
  expect_equal(vifOut$rho, 10^2)
  m = 5
  # has to subsample here
  vifOut = vif(res, y, X, x, n, p, m, TSS, lmFit = .lm.fit)
})

# should have test for rS>=0; actually, left negative, put bound elsewhere
