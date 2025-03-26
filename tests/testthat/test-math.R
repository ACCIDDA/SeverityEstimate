describe("logit", {
  it("yields real values when p between 0 and 1", {
    p <- rbeta(100L, 1.0, 1.0)
    x <- logit(p)
    expect_true(all(is.numeric(x) & is.finite(x)))
  })
  it("yields -Inf when p is 0", {
    expect_identical(logit(0.0), -Inf)
  })
  it("yields Inf when p is 1", {
    expect_identical(logit(1.0), Inf)
  })
  it("yields NaN when p is", {
    it("greater than 1", {
      p <- seq(1.1, 10.0, length.out = 10L)
      expect_warning(x <- logit(p))
      expect_true(all(is.nan(x)))
    })
    it("less than 0", {
      p <- seq(-0.1, -10.0, length.out = 10L)
      expect_warning(x <- logit(p))
      expect_true(all(is.nan(x)))
    })
  })
  it("is monotonic", {
    p <- seq(0.001, 0.999, length.out = 100L)
    x <- logit(p)
    expect_true(all(diff(x) >= 0.0))
  })
})

describe("inv_logit", {
  it("yields values between 0 and 1 for real values", {
    x <- rnorm(100L, sd = 10.0)
    p <- inv_logit(x)
    expect_true(all(is.numeric(p) & is.finite(p) & 0.0 <= p & p <= 1.0))
  })
  it("yields 0 when x is -Inf", {
    expect_identical(inv_logit(-Inf), 0.0)
  })
  it("yields 1 when x is Inf", {
    expect_identical(inv_logit(Inf), 1.0)
  })
  it("is monotonic", {
    x <- seq(-10.0, 10.0, length.out = 100L)
    p <- inv_logit(x)
    expect_true(all(diff(x) >= 0.0))
  })
})

test_that("logit is the inverse of inv_logit", {
  x <- rnorm(100L, sd = 10.0)
  y <- logit(inv_logit(x))
  expect_equal(y, x, tolerance = sqrt(testthat_tolerance()))
})

test_that("inv_logit is the inverse of logit", {
  p <- rbeta(100L, 1.0, 1.0)
  q <- inv_logit(logit(p))
  expect_equal(q, p, tolerance = sqrt(testthat_tolerance()))
})
