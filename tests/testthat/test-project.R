test_that("project creation scaffolds directories", {
  p <- file.path(tempdir(), "opensr-test-review")
  if (dir.exists(p)) unlink(p, recursive = TRUE)
  obj <- sr_project_create(p, name = "demo")
  expect_s3_class(obj, "sr_project")
  expect_true(file.exists(file.path(p, "config", "opensr.yml")))
  expect_true(file.exists(file.path(p, "logs", "events.csv")))
})

