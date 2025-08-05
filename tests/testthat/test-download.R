username = Sys.getenv("GISAIDR_USERNAME")
password = Sys.getenv("GISAIDR_PASSWORD")
credentials <- login(username = username, password = password)

test_that("download returns data.frame", {
  df <- query(credentials = credentials, nrows = 2)
  list_of_accession_ids <- df$accession_id
  full_df <- download(credentials, list_of_accession_ids)
  expect_true(is.data.frame(full_df))
})

test_that("get sequence works", {
  df <- query(credentials = credentials, nrows = 2)
  list_of_accession_ids <- df$accession_id
  full_df <- download(credentials, list_of_accession_ids, get_sequence=TRUE)
  expect_true(hasName(full_df, "sequence"))
})

test_that("download_files works for EpiCoV", {
  df <- query(credentials = credentials, nrows = 2)
  list_of_accession_ids <- df$accession_id
  download_results <- download_files(credentials, list_of_accession_ids, dates_and_location=TRUE, patient_status=TRUE, sequencing_technology=TRUE, augur_input=TRUE)
  dates_and_location <- download_results$dates_and_location
  patient_status <- download_results$patient_status
  sequencing_technology <- download_results$sequencing_technology
  augur_input_metadata <- download_results$augur_input$metadata
  augur_input_sequences <- download_results$augur_input$sequences

  download_results <- download_files(credentials, list_of_accession_ids, sequences=TRUE)
  sequences = download_results$sequences

  expect_true(
    is.data.frame(dates_and_location) && is.data.frame(patient_status) && is.data.frame(sequencing_technology) && is.data.frame(augur_input_metadata) &&
      !is.null(augur_input_sequences) && !is.null(sequences)
  )
})

credentials <- login(username = username, password = password, database="EpiPox")

test_that("download_files works for EpiPox", {
  df <- query(credentials = credentials, nrows = 2)
  list_of_accession_ids <- df$accession_id
  download_results <- download_files(credentials, list_of_accession_ids, dates_and_location=TRUE, patient_status=TRUE, sequencing_technology=TRUE, sequences=TRUE)
  dates_and_location <- download_results$dates_and_location
  patient_status <- download_results$patient_status
  sequencing_technology <- download_results$sequencing_technology
  sequences = download_results$sequences

  expect_true(
    is.data.frame(dates_and_location) && is.data.frame(patient_status) && is.data.frame(sequencing_technology) &&
      !is.null(sequences)
  )
})

credentials <- login(username = username, password = password, database="EpiRSV")

test_that("download_files works for EpiRSV", {
  df <- query(credentials = credentials, nrows = 2)
  list_of_accession_ids <- df$accession_id
  download_results <- download_files(credentials, list_of_accession_ids, dates_and_location=TRUE, patient_status=TRUE, sequencing_technology=TRUE, sequences=TRUE)
  dates_and_location <- download_results$dates_and_location
  patient_status <- download_results$patient_status
  sequencing_technology <- download_results$sequencing_technology
  sequences = download_results$sequences

  expect_true(
    is.data.frame(dates_and_location) && is.data.frame(patient_status) && is.data.frame(sequencing_technology) &&
      !is.null(sequences)
  )
})
