test_that("targets and renv helpers create files", {
  p <- file.path(tempdir(), "opensr-repro")
  if (dir.exists(p)) unlink(p, recursive = TRUE)
  sr_project_create(p, name = "repro")

  tfile <- sr_init_targets(p)
  rfile <- sr_init_renv(p)

  expect_true(file.exists(tfile))
  expect_true(file.exists(rfile))
})

test_that("osf push supports dry-run", {
  p <- file.path(tempdir(), "opensr-osf")
  if (dir.exists(p)) unlink(p, recursive = TRUE)
  sr_project_create(p, name = "osf")

  out <- sr_osf_push(p, project = "abc123", dry_run = TRUE)
  expect_true(is.list(out))
  expect_true(out$dry_run)
})
