#' Download from GISAID
#'
#' @param credentials GISAID credentials.
#' @param list_of_accession_ids list of accession_id from GISAID
#' @param get_sequence load sequences into data.frame after download
#' @param clean_up delete downloaded files (e.g. fasta files) after download
#' @return data.frame of complete data
download <- function(credentials, list_of_accession_ids, get_sequence=TRUE, clean_up=TRUE) {
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
  download_cmd <- 'Download'
  download_pid_wid <- get_download_panel(credentials$sid, credentials$wid, credentials$pid, credentials$query_cid)
  #load panel
  downloal_page <-
    send_request(paste0('sid=', credentials$sid, '&pid=', download_pid_wid$pid))

  download_page_text = httr::content(downloal_page, as = 'text')
  if (credentials$database == 'EpiRSV') {
    credentials$download_panel_cid <- extract_first_match("'(.{5,20})','RSVDownloadSelectionComponent", download_page_text)
    #send_back_cmd(credentials$sid, download_pid_wid$wid, download_pid_wid$pid, credentials$download_panel_cid)
  } else if (credentials$database == 'EpiPox') {
    credentials$download_panel_cid <- extract_first_match("'(.{5,20})','MPoxDownloadSelectionComponent", download_page_text)
  } else {
    credentials$download_panel_cid <- extract_first_match("'(.{5,20})','DownloadSelectionComponent", download_page_text)
    radio_button_widget_cid <- extract_first_match("'(.{5,20})','RadiobuttonWidget", download_page_text)
    # augur
    queue = list()
    command <- createCommand(
      wid = download_pid_wid$wid,
      pid = download_pid_wid$pid,
      cid = credentials$download_panel_cid,
      cmd = 'setTarget',
      params = list(cvalue="augur_input", ceid=radio_button_widget_cid)
    )
    queue <- append(queue, list(command))
    command <- createCommand(
      wid = download_pid_wid$wid,
      pid = download_pid_wid$pid,
      cid = credentials$download_panel_cid,
      cmd = 'ChangeValue',
      params = list(cvalue="augur_input", ceid=radio_button_widget_cid)
    )
    queue <- append(queue, list(command))
    command <- createCommand(
      wid = download_pid_wid$wid,
      pid = download_pid_wid$pid,
      cid = credentials$download_panel_cid,
      cmd = 'FormatChange',
      params = list(ceid=radio_button_widget_cid)
    )
    queue <- append(queue, list(command))
    command_queue <- list(queue = queue)
    data <- formatDataForRequest(credentials$sid, download_pid_wid$wid, download_pid_wid$pid, command_queue, timestamp())
    response <- send_request(method = 'POST', data=data)
    response_data <- parseResponse(response)
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
      download.file(download_url, tmpTarFile, quiet = TRUE, method = 'auto', mode = "wb")
      # unzip
      untar(tmpTarFile, exdir="gisaidr_data_tmp", restore_times = FALSE, verbose=FALSE)
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
        names(df) <- c('strain', 'accession_id', 'collection_date', 'description', 'sequence')
      } else {
        names(df) <- c('strain', 'accession_id', 'collection_date', 'description')
      }
    }

  }, finally = {
    # clean up
    if (clean_up){
      if (credentials$database == "EpiCoV") {
        if (file.exists(tmpTarFile)) {
          file.remove(tmpTarFile)
        }
        if (file.exists("gisaidr_data_tmp")) {
          unlink("gisaidr_data_tmp", recursive = TRUE)
        }
      } else {
        if (file.exists(sequencesFile)) {
          file.remove(sequencesFile)
        }
      }
    }
  })
  # replace ? with NA
  df[ df == "?" ] <- NA
  return(df)
}
