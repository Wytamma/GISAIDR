export_fasta <- function(seqs, out_file_name, export_dated_only = T){
    require(lubridate)
    if(export_dated_only){
        dates <- round(decimal_date(ymd(seqs$date)), 3)
        seqs <- seqs[!is.na(dates), ]
    } 
    newnames <- paste(seqs$country, seqs$pangolin_lineage,
                      seqs$accession_id, dates, sep = '@')
    cat('', file = out_file_name)
    for(i in 1:nrow(seqs)){
        cat('>', newnames[i], '\n', sep= '', file = out_file_name, append = T)
        cat(seqs$sequence[i], '\n', file = out_file_name, append = T)
    }
}
