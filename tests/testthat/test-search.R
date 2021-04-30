username = Sys.getenv("GISAIDR_USERNAME")
password = Sys.getenv("GISAIDR_PASSWORD")
credentials <- login(username = username, password = password)
# will break if log in fails...

test_that("basic query works", {
  df <- search(credentials = credentials)
  expect_true(is.data.frame(df))
})

test_that("can change number of rows", {
  df <- search(credentials = credentials, nrows = 100)
  expect_equal(nrow(df), 100)
})

test_that("can change index", {
  df1 <- search(credentials = credentials,
                 nrows = 100,
                 start_index = 0)
  df2 <- search(credentials = credentials,
                 nrows = 1,
                 start_index = 99)
  expect_true(df2[1, 1] == df1[100, 1])
})

test_that("expried session fails", {
  expect_error(search(
    credentials = list(
      pid = credentials$pid,
      wid = credentials$wid,
      sid = "890C95496CO0CQ007Z8HDSG34GF1JZ3Z"
    )
  ),
  "The session has expired. Please login again.")
})

# test_that("download all", {
#   df <- search(credentials = credentials, download_all = TRUE)
# })
