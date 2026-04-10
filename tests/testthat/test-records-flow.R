test_that("flow data returns expected columns", {
  rec <- new_sr_object(
    data = tibble::tibble(record_id = 1:2, title = c("a", "b"), is_duplicate = c(FALSE, TRUE)),
    class_name = "sr_records"
  )
  out <- sr_flow_data(rec)
  expect_true(all(c("identified", "duplicates_removed", "included") %in% names(out$data)))
})
