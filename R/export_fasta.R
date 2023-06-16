#' Export Fasta
#'
#' @param seqs dataframe containing sequence information, output of function `query`
#' @param out_file_name output file name
#' @param export_dated_only Should entries without dates be exported? Set to TRUE to exlude sequences without a date (default)
#' @param delimiter fasta sequence delimiter. Default `@`
#' @param data_format Optional, specify date format eg for ISO format use '%Y-%m-%d'
#' @param columns Specify columns in the input dataframe to use as fasta headers
#' @return data.frame of complete data
export_fasta <- function(
    seqs,
    out_file_name,
    export_dated_only = T,
    delimiter='@',
    date_format=NULL,
    columns=c('country', 'pangolin_lineage', 'accession_id', 'date')
){
  if (all(columns %in% names(seqs))) {
    if('date' %in% columns){
      if (is.null(date_format)) {
        dates <- round(lubridate::decimal_date(lubridate::ymd(seqs$date)), 3)
      } else {
        dates <- format(as.Date(seqs$date), date_format)
      }
      if(export_dated_only){
        seqs <- seqs[!is.na(dates), ]
        dates <- dates[!is.na(dates)]
      }
      seqs$date <- dates
    }
    newnames <- do.call(paste, c(lapply(columns, function(x) {seqs[,x]}), sep = delimiter))
  } else {
    missing_columns <- setdiff(columns, names(seqs))
    message(sprintf("Missing columns %s. Defaulting to `description`", missing_columns))
    newnames <- paste(seqs$description, sep = delimiter)
  }

  cat('', file = out_file_name)
  for(i in 1:nrow(seqs)){
    cat('>', newnames[i], '\n', sep= '', file = out_file_name, append = T)
    cat(seqs$sequence[i], '\n', file = out_file_name, append = T)
  }
}
