test_that("login returns credentials", {
  credentials <- login(username=Sys.getenv("GISAIDR_USERNAME"), password=Sys.getenv("GISAIDR_PASSWORD"))
  expect_true(is.list(credentials))
  print(credentials)
})
