test_that("flow/prisma/checklist/report stubs work", {
  rec <- new_sr_object(data = tibble::tibble(record_id = 1:3, title = c("a", "a", "b"), is_duplicate = c(FALSE, TRUE, FALSE)), class_name = "sr_records")
  scr <- new_sr_object(data = tibble::tibble(record_id = 1:2, final_decision = c("include", "exclude")), class_name = "sr_screen_log")
  ft <- sr_fulltext_log(tibble::tibble(record_id = 2, reason = "wrong population"))

  flow <- sr_flow_data(rec, scr, ft)
  prisma <- sr_prisma_flow(flow)
  chk <- sr_prisma_checklist("PRISMA 2020")

  expect_s3_class(flow, "sr_report")
  expect_s3_class(prisma, "sr_report")
  expect_s3_class(chk, "sr_report")
})

test_that("archive, manifest and reproducibility report work", {
  p <- file.path(tempdir(), "opensr-archive")
  if (dir.exists(p)) unlink(p, recursive = TRUE)
  sr_project_create(p, name = "archive")
  writeLines("x", file.path(p, "data", "x.txt"))

  manifest <- sr_osf_manifest(p)
  expect_s3_class(manifest, "sr_archive")
  expect_true(sr_archive_verify(manifest))

  bundle <- sr_archive_bundle(p)
  expect_s3_class(bundle, "sr_archive")

  rep <- sr_report_reproducibility(p)
  expect_s3_class(rep, "sr_report")
})
