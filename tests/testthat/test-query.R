username = Sys.getenv("GISAIDR_USERNAME")
password = Sys.getenv("GISAIDR_PASSWORD")
credentials <- login(username = username, password = password)
# will break if log in fails...

test_that("basic query works", {
  df <- query(credentials = credentials)
  expect_true(is.data.frame(df))
})

test_that("can change number of rows", {
  df <- query(credentials = credentials, nrows = 100)
  expect_equal(nrow(df), 100)
})

test_that("can change index", {
  df1 <- query(credentials = credentials,
               start_index = 0)
  df2 <- query(credentials = credentials,
               start_index = 49)
  expect_true(df2[1, 1] == df1[50, 1])
})

test_that("expried session fails", {
  expect_error(query(
    credentials = list(
      pid = credentials$pid,
      wid = credentials$wid,
      sid = "890C95496CO0CQ007Z8HDSG34GF1JZ3Z"
    )
  ),
  "The session has expired. Please login again.")
})

test_that("location search works", {
  df <- query(credentials = credentials,
              location = 'Australia')
  expect_true(all(lapply(df$location,
                         function(x)
                           grepl("Australia", x, fixed = TRUE))))
})

test_that("lineage search works", {
  df <- query(credentials = credentials,
              lineage = 'W.1')
  # need a better way to test this...
  expect_true(is.data.frame(df))
})

test_that("combination search works", {
  df <- query(credentials = credentials,
              location = 'Australia',
              lineage = 'W.1')
  expect_true(df$id[1] == "EPI_ISL_678350")
})

test_that("load all works", {
  df <- query(credentials = credentials,
              lineage = 'W.1',
              load_all = TRUE)
  expect_true(nrow(df) > 50)
})

test_that("date search works", {
  df <-
    query(credentials = credentials,
          from = '2021-04-05',
          to = '2021-04-05')
  expect_true(all(df$collection_date == "2021-04-05"))
})

test_that("low_coverage_excl works", {
  df <- query(credentials = credentials, low_coverage_excl = TRUE)
  expect_true(length(grep("Long stretches of NNNs", df$information)) == 0)
})
