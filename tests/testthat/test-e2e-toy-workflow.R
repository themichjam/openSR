test_that("end-to-end toy workflow runs", {
  source(file.path("inst", "scripts", "end_to_end_toy_review.R"), local = TRUE)
  out <- run_opensr_toy_workflow(file.path(tempdir(), "opensr-e2e-test"))

  expect_s3_class(out$project, "sr_project")
  expect_s3_class(out$search, "sr_search_log")
  expect_s3_class(out$records, "sr_records")
  expect_s3_class(out$screen, "sr_screen_log")
  expect_s3_class(out$effects, "sr_effects")
  expect_s3_class(out$meta, "sr_meta")
  expect_s3_class(out$prisma, "sr_report")
  expect_s3_class(out$archive, "sr_archive")
  expect_s3_class(out$repro, "sr_report")

  expect_true(nrow(out$prisma$data) == 1)
  expect_true(file.exists(out$archive$data$file[[1]]))
})
