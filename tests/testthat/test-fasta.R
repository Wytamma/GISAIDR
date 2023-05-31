test_that("read_fasta works", {
  fa_df <- read_fasta('data/test.fa')
  expect_equal(names(fa_df), c('strain', 'sequence'))
  expect_equal(fa_df$strain, c('A', 'B', 'C', 'D'))
  expect_equal(fa_df$sequence[4], 'TTCGTCCGTGTTGCAGCCGATCATCAGCACATCTAG')
})


test_that("export_fasta works", {

  # Define a temporary file for testing
  temp_file <- tempfile(fileext = ".fasta")

  # Mock seqs data frame
  seqs <- data.frame(country = c("Country1", "Country2"),
                     pangolin_lineage = c("Lineage1", "Lineage2"),
                     accession_id = c("Accession1", "Accession2"),
                     date = as.Date(c("2023-01-01", "2023-02-01")),
                     description = c("Description1", "Description2"),
                     sequence = c("AGCT", "TCGA"))

  # Apply the function
  export_fasta(seqs, temp_file, columns = c("country", "pangolin_lineage", "accession_id", "date"), date_format = "%Y-%m")

  # Check if file exists
  expect_true(file.exists(temp_file))

  # Read the generated file
  fasta_content <- readLines(temp_file)

  # Check the format of the FASTA headers
  fasta_headers <- fasta_content[seq(1, length(fasta_content), 2)]
  expected_headers <- c(">Country1@Lineage1@Accession1@2023-01",
                        ">Country2@Lineage2@Accession2@2023-02")
  expect_equal(fasta_headers, expected_headers)

  # Check the sequences
  fasta_sequences <- trimws(fasta_content[seq(2, length(fasta_content), 2)])
  expected_sequences <- c("AGCT", "TCGA")
  expect_equal(fasta_sequences, expected_sequences)

  # Remove temporary file
  unlink(temp_file)
})
