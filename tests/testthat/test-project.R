test_that("project creation scaffolds directories and logs", {
  p <- file.path(tempdir(), "opensr-test-review")
  if (dir.exists(p)) unlink(p, recursive = TRUE)

  obj <- sr_project_create(p, name = "demo")
  expect_s3_class(obj, "sr_project")

  expected <- c("protocol", "search", "screening", "fulltext", "extraction", "rob", "synthesis", "reports", "archive", "data-raw", "data", "logs", "config")
  expect_true(all(file.exists(file.path(p, expected))))
  expect_true(file.exists(file.path(p, "config", "opensr.yml")))
  expect_true(file.exists(file.path(p, "logs", "events.csv")))
  expect_true(file.exists(file.path(p, "logs", "events.ndjson")))
})

test_that("project load works", {
  p <- file.path(tempdir(), "opensr-test-load")
  if (dir.exists(p)) unlink(p, recursive = TRUE)
  sr_project_create(p, name = "demo-load")

  obj <- sr_project_load(p)
  expect_s3_class(obj, "sr_project")
  expect_equal(obj$metadata$name, "demo-load")
})
