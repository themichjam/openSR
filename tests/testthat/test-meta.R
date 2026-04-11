test_that("generic effect sizes and random meta run", {
  d <- tibble::tibble(yi = c(0.1, 0.2, 0.4), vi = c(0.01, 0.02, 0.03))
  es <- sr_es_generic(d, "yi", "vi")
  fit <- sr_meta_random(es)

  expect_s3_class(es, "sr_effects")
  expect_s3_class(fit, "sr_meta")
})

test_that("meta helpers return expected structures", {
  d <- tibble::tibble(yi = c(0.1, 0.2, 0.4, 0.5), vi = c(0.01, 0.02, 0.03, 0.02), grp = c("a", "a", "b", "b"))
  es <- sr_es_generic(d, "yi", "vi")

  loo <- sr_meta_leave1out(es)
  bias <- sr_meta_bias(es)
  sub <- sr_meta_subgroup(d, subgroup = "grp")

  expect_true(nrow(loo) == nrow(d))
  expect_true("k" %in% names(bias))
  expect_s3_class(sub, "sr_meta")
})
