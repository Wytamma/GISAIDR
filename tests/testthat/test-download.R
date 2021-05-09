username = Sys.getenv("GISAIDR_USERNAME")
password = Sys.getenv("GISAIDR_PASSWORD")
credentials <- login(username = username, password = password)

test_that("download returns data.frame", {
  df <- query(credentials = credentials)
  list_of_accession_ids <- df$accession_id
  full_df <- download(credentials, df$accession_id)
  expect_true(is.data.frame(full_df))
})
