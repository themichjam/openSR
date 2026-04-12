test_that("deduplication sets stable ids and duplicate flags", {
  rec <- new_sr_object(
    data = tibble::tibble(record_id = 1:3, title = c("A", "A", "B"), doi = c("x", "x", "y")),
    class_name = "sr_records"
  )

  ded <- sr_deduplicate(rec)
  expect_true("record_uid" %in% names(ded$data))
  expect_equal(sum(ded$data$is_duplicate), 1)
})

test_that("screen dual builds conflict queue", {
  rec <- new_sr_object(data = tibble::tibble(record_id = 1:2, title = c("a", "b")), class_name = "sr_records")
  scr <- sr_screen_template(rec)
  scr$data$decision_1 <- c("include", "exclude")
  scr$data$decision_2 <- c("include", "include")

  out <- sr_screen_dual(scr)
  expect_true(any(out$data$queue == "disagreement"))
})

test_that("screen resolve applies final decisions", {
  rec <- new_sr_object(data = tibble::tibble(record_id = 1:2, title = c("a", "b")), class_name = "sr_records")
  scr <- sr_screen_template(rec)
  scr$data$decision_1 <- c("exclude", "exclude")
  scr$data$decision_2 <- c("include", "exclude")
  scr <- sr_screen_dual(scr)

  out <- sr_screen_resolve(scr, tibble::tibble(record_id = 1, final_decision = "include"))
  expect_equal(out$data$final_decision[out$data$record_id == 1], "include")
})

test_that("fulltext reason vocabulary enforced", {
  ok <- sr_fulltext_log(tibble::tibble(record_id = 1, reason = "wrong population"))
  expect_s3_class(ok, "sr_fulltext_log")
  expect_error(sr_fulltext_log(tibble::tibble(record_id = 1, reason = "made up reason")))
})
