#' Read FASTA amino acids file into a dataframe
#'
#' This function reads a FASTA amino acids file into a dataframe
#'
#' TAKEN FROM AMPIR: https://github.com/Legana/ampir


read_fasta <- function (file = NULL, get_sequence=TRUE) {
  fasta_lines <- readLines(file)

  ### get sequence names
  seq_name_index <- grep(">", fasta_lines)
  strain <- gsub(">", "", fasta_lines[seq_name_index])

  if (get_sequence) {
    ### get sequence
    seq_aa_start_index <- seq_name_index + 1
    seq_aa_end_index <- c(seq_name_index, length(fasta_lines)+1)[-1]-1

    sequence <- rep(NA, length(seq_name_index))

    ### replace NA content with actual sequence content, and concatenate the lines
    for(i in seq_along(seq_name_index)){
      seq_aa_start <- seq_aa_start_index[i]
      seq_aa_end   <- seq_aa_end_index[i]
      sequence[i] <- gsub("[[:space:]]", "",
                          paste(fasta_lines[seq_aa_start:seq_aa_end],
                                collapse = ""))
    }
    return(data.frame(strain, sequence, stringsAsFactors = FALSE))
  }
  return(data.frame(strain, stringsAsFactors = FALSE))

}
