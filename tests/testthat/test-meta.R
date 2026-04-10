test_that("generic effect sizes and random meta run", {
  d <- tibble::tibble(yi = c(0.1, 0.2, 0.4), vi = c(0.01, 0.02, 0.03))
  es <- sr_es_generic(d, "yi", "vi")
  fit <- sr_meta_random(es)
  expect_s3_class(es, "sr_effects")
  expect_s3_class(fit, "sr_meta")
})
