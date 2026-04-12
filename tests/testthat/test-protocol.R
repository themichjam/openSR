test_that("protocol init and validation behave", {
  p <- sr_protocol_init("Protocol", "Question")
  expect_s3_class(p, "sr_protocol")

  pv <- sr_protocol_validate(p)
  expect_s3_class(pv, "sr_protocol")
  expect_true(is.logical(pv$validation$valid))
})

test_that("protocol export writes yaml", {
  p <- sr_protocol_init("Protocol", "Question")
  f <- file.path(tempdir(), "protocol.yml")
  out <- sr_protocol_export(p, f)
  expect_true(file.exists(f))
  expect_s3_class(out, "sr_protocol")
})
