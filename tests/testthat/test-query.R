username = Sys.getenv("GISAIDR_USERNAME")
password = Sys.getenv("GISAIDR_PASSWORD")
credentials <- login(username = username, password = password)
# will break if log in fails...

test_that("basic query works", {
  df <- getData(credentials=credentials)
  expect_true(is.data.frame(df))
})

test_that("can change number of rows", {
  df <- getData(credentials=credentials, nrows=100)
  expect_equal(nrow(df), 100)
})

test_that("can change index", {
  df1 <- getData(credentials=credentials, nrows=50, start_index=0)
  df2 <- getData(credentials=credentials, nrows=1, start_index=49)
  expect_equal(df2[1,1], df1[50,1])
})

test_that("expried session fails", {
  expect_error(
    getData(credentials=list(pid=credentials$pid, wid=credentials$wid, sid="890C95496CO0CQ007Z8HDSG34GF1JZ3Z")),
    "The session has expired. Please login again.")
})
