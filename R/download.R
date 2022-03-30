#' Download from GISAID
#'
#' @param credentials GISAID credentials.
#' @param list_of_accession_ids list of accession_id from GISAID
#' @param get_sequence load sequences into data.frame after download
#' @param clean_up delete downloaded files (e.g. fasta files) after download
#' @return data.frame of complete data
download <- function(credentials, list_of_accession_ids, get_sequence=FALSE, clean_up=TRUE) {
  if (length(list_of_accession_ids) > 5000) {
    stop('Can only download a maxium of 5000 samples at a time.')
  } else if (length(list_of_accession_ids) == 0) {
    stop('Select at least one sequence!')
  }
  # select
  message('Selecting entries...')

  response <- select_entries(credentials = credentials, list_of_accession_ids=list_of_accession_ids)

  download_cmd <- 'ToolDownload'
  download_pid_wid <- list(pid=credentials$pid, wid=credentials$wid)
  if (credentials$database == 'EpiRSV') {
    download_cmd <- 'Download'
    download_pid_wid <- get_download_panel(credentials$sid, credentials$wid, credentials$pid, credentials$query_cid)
    #load panel
    downloal_page <-
      send_request(paste0('sid=', credentials$sid, '&pid=', download_pid_wid$pid))

    download_page_text = httr::content(downloal_page, as = 'text')
    credentials$download_panel_cid <- extract_first_match("'(.{5,20})','RSVDownloadSelectionComponent", download_page_text)
    #send_back_cmd(credentials$sid, download_pid_wid$wid, download_pid_wid$pid, credentials$download_panel_cid)
  }

  ev <- createCommand(
    wid = download_pid_wid$wid,
    pid = download_pid_wid$pid,
    cid = credentials$download_panel_cid,
    cmd = download_cmd,
    params = setNames(list(), character(0)) #hack for empty {}
  )
  json_queue <- list(queue = list(ev))
  data <- formatDataForRequest(credentials$sid, download_pid_wid$wid, download_pid_wid$pid, json_queue, timestamp())
  message('Compressing data. Please wait...')
  res <-
    httr::POST(GISAID_URL, httr::add_headers(.headers = headers), body = data)
  j <- parseResponse(res)
  # extract check_async
  check_async_id = strsplit(j$responses[[1]]$data, "'")[[1]][4]

  # while generateDownloadDone not ready
  is_ready = FALSE
  while (!is_ready) {
    res <- httr::GET(paste0('https://www.epicov.org/epi3/check_async/', check_async_id, '?_=', timestamp()))
    j <- parseResponse(res)
    is_ready <- j$is_ready
    if (!is_ready) {
      Sys.sleep(1)
    }
  }
  log.debug(j)
  # get download link
  message('Data ready.')
  ev <- createCommand(
    wid = credentials$wid,
    pid = credentials$pid,
    cid = credentials$query_cid,
    cmd = "generateDownloadDone",
    params = setNames(list(), character(0)) #hack for empty {}
  )
  json_queue <- list(queue = list(ev))
  data <- formatDataForRequest(credentials$sid, credentials$wid, credentials$pid, json_queue, timestamp())
  res <-
    send_request(method = 'POST', data=data)
  j <- parseResponse(res)

  # extract download url
  download_url <- paste0("https://www.epicov.org",strsplit(j$responses[[1]]$data, '"')[[1]][2])
  # download zip
  tryCatch({
    message('Downloading...')

    if (credentials$database == "EpiCoV") {
      tmpTarFile <- "gisaidr_data_tmp.tar"
      download.file(download_url, tmpTarFile, quiet = TRUE, method = 'auto', )
      message(paste(list.files()))
      # check if the downloaded file is a folder
      if (dir.exists(tmpTarFile)) {
        # already unziped so rename
        file.rename(from = tmpTarFile, to = "gisaidr_data_tmp")
      } else {
        # try to unzip
        message("try to unziping")
        untar(tmpTarFile, exdir="gisaidr_data_tmp", restore_times = FALSE)
      }

      # load into df
      metadataFile <- list.files("gisaidr_data_tmp", pattern = "*.metadata.tsv")[1]
      if (is.na(metadataFile)) {
        message("gisaid_data files:")
        message(list.files("gisaidr_data_tmp"))
        stop("Could not find metadata file.")
      }
      if(.Platform$OS.type == "unix") {
        metadataFilePath <- paste0("gisaidr_data_tmp/", metadataFile)
      } else {
        metadataFilePath <- paste0("gisaidr_data_tmp\\", metadataFile)
      }
      df <- read.csv(metadataFilePath, sep="\t", quote="")
      df <- df[order(df$gisaid_epi_isl, decreasing = TRUE),]
      colnames(df)[3] <- "accession_id"
      if (get_sequence) {
        # join sequence
        sequencesFile <- list.files("gisaidr_data_tmp", pattern = "*.sequences.fasta")[1]
        if (is.na(sequencesFile)) {
          stop("Could not find sequences file.")
        }
        if(.Platform$OS.type == "unix") {
          sequencesFilePath <- paste0("gisaidr_data_tmp/", sequencesFile)
        } else {
          sequencesFilePath <- paste0("gisaidr_data_tmp\\", sequencesFile)
        }
        seq_df <- read_fasta(sequencesFilePath)
        df <- merge(x = df, y = seq_df, by = "strain", all = TRUE)
      }
    } else {
      sequencesFile <- "gisaidr_data_tmp.fasta"
      download.file(download_url, sequencesFile, quiet = TRUE, method = 'auto')
      fdf <- read_fasta(sequencesFile, get_sequence)
      df <- merge(data.frame(do.call('rbind', strsplit(as.character(fdf$strain), '|', fixed=TRUE))), fdf, by = 0)
      df <- df[-c(1)]
      if (get_sequence) {
        names(df) <- c('strain', 'accession_id', ' collection_date', 'description', 'sequence')
      } else {
        names(df) <- c('strain', 'accession_id', ' collection_date', 'description')
      }
    }

  }, finally = {
    # clean up
    if (clean_up){
      if (file.exists(tmpTarFile)) {
        file.remove(tmpTarFile)
      }
      if (file.exists("gisaidr_data_tmp")) {
        unlink("gisaidr_data_tmp", recursive = TRUE)
      }
    }
  })
  # replace ? with NA
  df[ df == "?" ] <- NA
  return(df)
}
