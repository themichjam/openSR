test_that("csv import and dedupe work", {
  f <- file.path(tempdir(), "records.csv")
  readr::write_csv(tibble::tibble(title = c("A", "A"), doi = c("x", "x")), f)
  rec <- sr_import_csv(f)
  rec2 <- sr_deduplicate(rec)
  expect_true("is_duplicate" %in% names(rec2$data))
  expect_equal(sum(rec2$data$is_duplicate), 1)
})

test_that("flow data produces expected columns", {
  rec <- new_sr_object(
    data = tibble::tibble(record_id = 1:2, title = c("a", "b"), is_duplicate = c(FALSE, TRUE)),
    class_name = "sr_records"
  )
  sc <- new_sr_object(data = tibble::tibble(record_id = 1:1, reviewer_1 = "include", reviewer_2 = "include"), class_name = "sr_screen_log")
  ft <- sr_fulltext_log(tibble::tibble(record_id = 1, reason = "wrong population"))
  out <- sr_flow_data(rec, sc, ft)
  expect_true(all(c("identified", "included") %in% names(out)))
})
