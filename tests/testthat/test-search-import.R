test_that("search log initializes and appends", {
  s <- sr_search_log_init()
  s <- sr_search_add(s, source = "PubMed", query = "abc", n_results = 10)

  expect_s3_class(s, "sr_search_log")
  expect_equal(nrow(s$data), 1)
  expect_true(nchar(s$data$query_hash[[1]]) > 0)
})

test_that("csv import and validation work", {
  f <- file.path(tempdir(), "records.csv")
  readr::write_csv(tibble::tibble(title = c("A", "A"), doi = c("x", "x")), f)

  rec <- sr_import_csv(f)
  rec <- sr_records_validate(rec)

  expect_s3_class(rec, "sr_records")
  expect_true("record_id" %in% names(rec$data))
})

test_that("ris import reads TI lines", {
  f <- file.path(tempdir(), "records.ris")
  writeLines(c("TY  - JOUR", "TI  - A Trial", "ER  -"), f)
  rec <- sr_import_ris(f)
  expect_equal(rec$data$title[[1]], "A Trial")
})

test_that("bib import reads title fields", {
  f <- file.path(tempdir(), "records.bib")
  writeLines(c("@article{a,", "title={My Study},", "}"), f)
  rec <- sr_import_bib(f)
  expect_equal(rec$data$title[[1]], "My Study")
})


test_that("ris and bib invalid variants error clearly", {
  fr <- file.path(tempdir(), "bad.ris")
  writeLines(c("TI  - Missing TY"), fr)
  expect_error(sr_import_ris(fr), "TY")

  fb <- file.path(tempdir(), "bad.bib")
  writeLines(c("title={No entry}"), fb)
  expect_error(sr_import_bib(fb), "entry header")
})
