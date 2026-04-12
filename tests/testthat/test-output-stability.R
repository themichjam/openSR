test_that("printed S3 outputs are stable", {
  obj <- new_sr_object(data = list(), metadata = list(name = "demo", root = "/tmp/review"), class_name = "sr_project")
  txt <- capture.output(print(obj))
  expect_equal(txt, c("<sr_project> demo", "Root: /tmp/review"))
})

test_that("PRISMA flow output schema is stable", {
  rec <- new_sr_object(data = tibble::tibble(record_id = 1:3, title = c("a", "a", "b"), is_duplicate = c(FALSE, TRUE, FALSE)), class_name = "sr_records")
  flow <- sr_prisma_flow(sr_flow_data(rec))
  expect_equal(names(flow$data), c("identified", "duplicates_removed", "records_after_dedup", "screened", "fulltext_assessed", "fulltext_excluded", "included"))
})

test_that("PRISMA checklist scaffold is stable", {
  chk <- sr_prisma_checklist("PRISMA-S")
  expect_equal(names(chk$data), c("standard", "item", "status", "location"))
  expect_equal(unique(chk$data$standard), "PRISMA-S")
})

test_that("reproducibility report schema is stable", {
  p <- file.path(tempdir(), "opensr-repro-stability")
  if (dir.exists(p)) unlink(p, recursive = TRUE)
  sr_project_create(p, name = "repro")

  rep <- sr_report_reproducibility(p)
  expect_equal(sort(names(rep$data)), sort(c("generated_at", "session_info", "package_versions", "hashes")))
  expect_true(is.data.frame(rep$data$hashes))
})

test_that("archive manifest output schema is stable", {
  p <- file.path(tempdir(), "opensr-manifest-stability")
  if (dir.exists(p)) unlink(p, recursive = TRUE)
  sr_project_create(p, name = "man")
  writeLines("x", file.path(p, "data", "x.txt"))

  man <- sr_osf_manifest(p)
  expect_equal(names(man$data), c("file", "rel_file", "size", "sha256"))
  expect_true(all(nchar(man$data$sha256) == 64L))
})
