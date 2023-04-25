username = Sys.getenv("GISAIDR_USERNAME")
password = Sys.getenv("GISAIDR_PASSWORD")
credentials <- login(username = username, password = password)
# will break if log in fails...

test_that("basic query works", {
  df <- query(credentials = credentials)
  expect_true(is.data.frame(df))
})

test_that("can change number of rows", {
  df <- query(credentials = credentials, nrows = 1000, location = "Africa / ...")
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
  sid <- credentials$sid
  credentials$sid = "890C95496CO0CQ007Z8HDSG34GF1JZ3Z"
  expect_error(query(
    credentials = credentials
  ),
  "The session has expired. Please login again.")
  credentials$sid <- sid
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
  expect_true(nrow(df) == 50)
})

test_that("combination search works", {
  df <- query(credentials = credentials,
              location = 'Australia',
              lineage = 'W.1')
  expect_true(df$accession_id[1] == "EPI_ISL_678350")
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
  expect_true(nrow(df) == 50)
  expect_true(all(df$collection_date == "2021-04-05"))
})

test_that("low_coverage_excl works", {
  df <- query(credentials = credentials, low_coverage_excl = TRUE)
  expect_true(nrow(df) == 50)
  expect_true(length(grep("Long stretches of NNNs", df$information)) == 0)
})

test_that("complete works", {
  df <- query(credentials = credentials, complete = TRUE)
  expect_true(nrow(df) == 50)
  expect_true(all(df$length > 29000))
})

test_that("submission date search works", {
  df <-
    query(credentials = credentials,
          from_subm = '2021-04-05',
          to_subm = '2021-04-05')
  expect_true(nrow(df) == 50)
  expect_true(all(df$submission_date == "2021-04-05"))
})

test_that("collection date complete works", {
  df <-
    query(
      credentials = credentials,
      lineage = 'BA.1',
      location = 'Australia',
      collection_date_complete = T
    )
  expect_true(nrow(df) == 50)
  expect_true(all(nchar(df$collection_date) == 10))
})

test_that("high coverage works", {
  df <-
    query(credentials = credentials, high_coverage = T)
  expect_true(nrow(df) == 50)
  expect_true(length(grep("warn_sign", df$information)) == 0)
})

test_that("total returns total", {
  total <-
    query(credentials = credentials, total=T)
  expect_true(is.numeric(total))
})

test_that("variant search works", {
  df <-
    query(credentials = credentials, variant='omicron')
  # variant information is not returned from query or download...
  # need a better way to test this...
  expect_true(is.data.frame(df))
  expect_true(nrow(df) == 50)
})

test_that("virus name search works", {
  df <-
    query(credentials = credentials, virus_name='hCoV-19/Ireland/D-BHTEST/2022')
  expect_true(is.data.frame(df))
  expect_true(nrow(df) == 1)
  expect_true(df[,'virus_name'] == 'hCoV-19/Ireland/D-BHTEST/2022')
})

test_that("fast works", {
  df <- query(credentials = credentials,
              lineage = 'W.1',
              fast = TRUE)
  expect_true(nrow(df) > 50)
})

test_that("order_by works", {
  df <- query(credentials = credentials, order_by = 'submission_date')
  expect_true(df$submission_date[1] == "2020-01-10")
})

test_that("aa_substitution works", {
  df <- query(credentials = credentials,
      aa_substitution = 'Spike_E484Q, Spike_H69del, -N_P13L',
      to_subm =  '2023-02-22',
      load_all = TRUE,
      order_by='submission_date')
  expect_true(is.data.frame(df))
  expect_equal(df$submission_date[1], "2021-01-25")
  expect_equal(nrow(df),576)
  ## to test accuracy - set of 4 rarely co-existing mutations to verify Spike_H69del, Spike_A222V, Spike_G476S, -N_P13L
})

test_that("nucl_mutation works", {
  df <- query(credentials = credentials, 
    nucl_mutation = '-T23599G, -C10029T, -C14408T, -A23403G, T22679C, G28881A, A24424T',
    to_subm = '2023-02-22',
    load_all = TRUE,
    order_by='submission_date')
  expect_true(is.data.frame(df))
  expect_equal(df$submission_date[1],"2021-12-29")
  expect_equal(nrow(df),55)
})

test_that("text search works", {
  accession_ids = c("EPI_ISL_17398411", "EPI_ISL_17199001", "EPI_ISL_17409201", "EPI_ISL_17243716")
  df <- query(credentials = credentials, text = paste(accession_ids, collapse = "\n"))
  expect_true(nrow(df) == 4)
})
