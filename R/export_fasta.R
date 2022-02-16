export_fasta <- function(seqs, out_file_name, export_dated_only = T, delimiter='@', date_format=NULL){
    require(lubridate)
    if (!is.null(date_format)) {
        dates <- format(ymd(seqs$date),date_format)
    } else {

        dates <- round(decimal_date(ymd(seqs$date)), 3)
    }
    if(export_dated_only){
        seqs <- seqs[!is.na(dates), ]
        dates <- dates[!is.na(dates)]
    }
    newnames <- paste(seqs$country, seqs$pangolin_lineage,
                      seqs$accession_id, dates, sep = delimiter)
    cat('', file = out_file_name)
    for(i in 1:nrow(seqs)){
        cat('>', newnames[i], '\n', sep= '', file = out_file_name, append = T)
        cat(seqs$sequence[i], '\n', file = out_file_name, append = T)
    }
}
