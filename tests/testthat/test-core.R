test_that("new_sr_object creates invariant fields", {
  obj <- new_sr_object(data = tibble::tibble(x = 1), class_name = "sr_project")
  expect_s3_class(obj, "sr_object")
  expect_true(all(c("data", "metadata", "provenance", "validation", "version") %in% names(obj)))
  expect_invisible(validate_sr_object(obj))
})

test_that("as.sr_object errors on missing fields", {
  expect_error(as.sr_object(list(data = 1), "sr_project"))
})
