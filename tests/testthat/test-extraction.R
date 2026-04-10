test_that("extraction template and validators run", {
  rec <- new_sr_object(data = tibble::tibble(record_id = 1:2, title = c("a", "b")), class_name = "sr_records")
  ex <- sr_extract_template(rec)
  expect_s3_class(ex, "sr_extraction")

  ex$data$study_id <- c("s1", "s2")
  ex$data$outcome_type <- c("continuous", "binary")
  exv <- sr_extract_validate(ex)
  expect_true(is.logical(exv$validation$valid))
})

test_that("extraction diagnostics detect missing fields", {
  rec <- new_sr_object(data = tibble::tibble(record_id = 1, title = "a", year = 2020), class_name = "sr_records")
  ex <- sr_extract_template(rec)
  ex$data$study_id <- "s1"
  ex$data$outcome_type <- "continuous"
  miss <- sr_detect_missing_effectsize_fields(ex)
  expect_true(nrow(miss) >= 1)
})

