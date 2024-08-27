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

    # accept agreement
    ev <- createCommand(
      wid = download_pid_wid$wid,
      pid = download_pid_wid$pid,
      cid = credentials$download_panel_cid,
      cmd = "DownloadReminder",
      params = setNames(list(), character(0)) #hack for empty {}
    )
    json_queue <- list(queue = list(ev))
    data <- formatDataForRequest(credentials$sid, download_pid_wid$wid, download_pid_wid$pid, json_queue, timestamp())
    res <-
      httr::POST(GISAID_URL, httr::add_headers(.headers = headers), body = data)
    j <- parseResponse(res) #

    download_pid_wid$wid <- extract_first_match("sys.openOverlay\\('(.{5,20})',", j$responses[[3]]$data)

    download_pid_wid$pid <- extract_first_match(",'(.{5,20})',new Object", j$responses[[3]]$data)

    agreement_page <- send_request(paste0('sid=', credentials$sid, '&pid=', download_pid_wid$pid, '&wid=', download_pid_wid$wid, '&mode=page') , method = "POST")
    agreement_page_text = httr::content(agreement_page, as = 'text')
    # extract the cid for download button
    credentials$download_panel_cid <- extract_first_match("'(.{5,20})','Corona2020DownloadReminderButtonsComponent", agreement_page_text)
    # accept the agreement
    agree_check_box_ceid <-  extract_first_match("createFI\\('(.{5,20})','CheckboxWidget'", agreement_page_text)
    queue = list()
    command <- createCommand(
      wid = download_pid_wid$wid,
      pid = download_pid_wid$pid,
      cid = credentials$download_panel_cid,
      cmd = 'setTarget',
      params = list(cvalue=list("agreed"), ceid=agree_check_box_ceid)
    )
    queue <- append(queue, list(command))
    command <- createCommand(
      wid = download_pid_wid$wid,
      pid = download_pid_wid$pid,
      cid = credentials$download_panel_cid,
      cmd = 'ChangeValue',
      params = list(cvalue=list("agreed"), ceid=agree_check_box_ceid)
    )
    queue <- append(queue, list(command))
    command <- createCommand(
      wid = download_pid_wid$wid,
      pid = download_pid_wid$pid,
      cid = credentials$download_panel_cid,
      cmd = 'Agreed',
      params = list(ceid=agree_check_box_ceid)
    )
    queue <- append(queue, list(command))
    command_queue <- list(queue = queue)
    data <- formatDataForRequest(credentials$sid, download_pid_wid$wid, download_pid_wid$pid, command_queue, timestamp())
    response <- send_request(method = 'POST', data=data)
    response_data <- parseResponse(response)

  }
  log.debug(response_data)
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

#' Download files from GISAID.
#'
#' @param credentials GISAID credentials.
#' @param list_of_accession_ids list of accession_id from GISAID
#' @param dates_and_location TRUE if Date and Location metadata should be downloaded
#' @param patient_status TRUE if Patient Status metadata should be downloaded
#' @param sequencing_and_technology TRUE if Sequencing Technology metadata should be downloaded
#' @param sequences TRUE if Nucleotide sequences should be downloaded
#' @param augur_input TRUE if Augur Input should be downloaded (metadata + sequences)
#' @param clean_up delete downloaded files (e.g. fasta files) after download
#' @return named list of 'metadata' df and/or 'sequences' CHAR str
download2 <- function(
  credentials,
  list_of_accession_ids,
  dates_and_location    = NULL,
  patient_status        = NULL,
  sequencing_technology = NULL,
  sequences             = NULL,
  augur_input           = NULL,
  clean_up              = TRUE
  ) {

  # Store the possible download type results
  download_results <- list(
    dates_and_location    = dates_and_location,
    patient_status        = patient_status,
    sequencing_technology = sequencing_technology,
    augur_input           = augur_input,
    sequences             = sequences
  )

  # ---------------------------------------------------------------------------
  # Select Accession

  if (length(list_of_accession_ids) > 5000) {
    stop(paste0(Sys.time(), "\tERROR. Can only download a maxium of 5000 samples at a time."))
  } else if (length(list_of_accession_ids) == 0) {
    stop(paste0(Sys.time(), "\tERROR. Select at least one sequence!"))
  }

  log.info(paste0(Sys.time(), " \tSelecting entries."))
  response <- select_entries(credentials = credentials, list_of_accession_ids=list_of_accession_ids)

  # ---------------------------------------------------------------------------
  # Download Type Loop

  for (download_type in names(download_results)){
    if (is.null(download_results[[download_type]])) { next }
    if (download_type == "augur_input" && credentials$database != "EpiCoV"){
      stop(paste0(Sys.time(), " \tThe download type augur_input is only available for EpiCoV"))
    }
    log.info(paste0(Sys.time(), " \tDownload type: ", download_type))

    # ---------------------------------------------------------------------------
    # Open Download Panel

    log.info(paste0(Sys.time(), " \t\tOpening download panel."))
    download_cmd       <- 'Download'
    download_pid_wid   <- get_download_panel(credentials$sid, credentials$wid, credentials$pid, credentials$query_cid)
    download_page      <- send_request(paste0('sid=', credentials$sid, '&pid=', download_pid_wid$pid))
    download_page_text <- httr::content(download_page, as = 'text')

    # Identify the download panel CID basedon which database we're in
    if (credentials$database == 'EpiRSV') {
      credentials$download_panel_cid <- extract_first_match("'(.{5,20})','RSVDownloadSelectionComponent", download_page_text)
    } else if (credentials$database == 'EpiPox') {
      credentials$download_panel_cid <- extract_first_match("'(.{5,20})','MPoxDownloadSelectionComponent", download_page_text)
    } else {
      credentials$download_panel_cid <- extract_first_match("'(.{5,20})','DownloadSelectionComponent", download_page_text)
    }

    # Store separate variables for the initial download (tmpFile) and
    # any subsequent extraction, such as metadataFile and sequencesFile
    # which come from the augur_input option.
    if (download_type == 'dates_and_location'){
      radio_button_value <- 'dateloc'
      tmpFile            <- 'gisaidr_dates_and_location_tmp.tsv'
      metadataFile       <- tmpFile
      sequencesFile      <- NULL
    } else if (download_type == 'patient_status'){
      radio_button_value <- 'meta_epi'
      tmpFile            <- 'gisaidr_patient_status_tmp.tsv'
      metadataFile       <- tmpFile
      sequencesFile      <- NULL
    } else if (download_type == 'sequencing_technology'){
      radio_button_value <- 'meta_tech'
      tmpFile            <- 'gisaidr_sequencing_technology_tmp.tsv'
      metadataFile       <- tmpFile
      sequencesFile      <- NULL
    } else if (download_type == 'sequences'){
      radio_button_value <- 'fasta'
      tmpFile            <- 'gisaidr_sequences_tmp.fasta'
      metadataFile       <- NULL
      sequencesFile      <- tmpFile
    } else if (download_type == 'augur_input'){
      radio_button_value <- 'augur_input'
      tmpFile            <- 'gisaidr_augur_input_tmp.tar'
      metadataFile       <- 'gisaidr_augur_input_metadata_tmp.tsv'
      sequencesFile      <- 'gisaidr_augur_input_sequences_tmp.fasta'
    }

    # ---------------------------------------------------------------------------
    # Select Download Type

    log.info(paste0(Sys.time(), " \t\tConfiguring download options."))
    radio_button_widget_cid <- extract_first_match("'(.{5,20})','RadiobuttonWidget", download_page_text)

    queue = list()
    command <- createCommand(
      wid = download_pid_wid$wid,
      pid = download_pid_wid$pid,
      cid = credentials$download_panel_cid,
      cmd = 'setTarget',
      params = list(cvalue=radio_button_value, ceid=radio_button_widget_cid)
    )
    queue <- append(queue, list(command))
    command <- createCommand(
      wid = download_pid_wid$wid,
      pid = download_pid_wid$pid,
      cid = credentials$download_panel_cid,
      cmd = 'ChangeValue',
      params = list(cvalue=radio_button_value, ceid=radio_button_widget_cid)
    )
    queue <- append(queue, list(command))
    command <- createCommand(
      wid = download_pid_wid$wid,
      pid = download_pid_wid$pid,
      cid = credentials$download_panel_cid,
      cmd = 'FormatChange',
      params = list(ceid=radio_button_widget_cid)
    )
    queue         <- append(queue, list(command))
    command_queue <- list(queue = queue)
    data          <- formatDataForRequest(credentials$sid, download_pid_wid$wid, download_pid_wid$pid, command_queue, timestamp())
    response      <- send_request(method = 'POST', data=data)
    response_data <- parseResponse(response)

    # ---------------------------------------------------------------------------
    # Accept EpiCoV Agreement

    if (credentials$database == 'EpiCoV') {
      log.info(paste0(Sys.time(), " \t\tAccepting EpiCoV agreement."))
      ev <- createCommand(
        wid = download_pid_wid$wid,
        pid = download_pid_wid$pid,
        cid = credentials$download_panel_cid,
        cmd = "DownloadReminder",
        params = setNames(list(), character(0)) #hack for empty {}
      )
      json_queue    <- list(queue = list(ev))
      data          <- formatDataForRequest(credentials$sid, download_pid_wid$wid, download_pid_wid$pid, json_queue, timestamp())
      response      <- httr::POST(GISAID_URL, httr::add_headers(.headers = headers), body = data)
      response_data <- parseResponse(response)

      download_pid_wid$wid <- extract_first_match("sys.openOverlay\\('(.{5,20})',", response_data$responses[[3]]$data)
      download_pid_wid$pid <- extract_first_match(",'(.{5,20})',new Object", response_data$responses[[3]]$data)

      agreement_page      <- send_request(paste0('sid=', credentials$sid, '&pid=', download_pid_wid$pid, '&wid=', download_pid_wid$wid, '&mode=page') , method = "POST")
      agreement_page_text <- httr::content(agreement_page, as = 'text')
      # extract the cid for download button
      credentials$download_panel_cid <- extract_first_match("'(.{5,20})','Corona2020DownloadReminderButtonsComponent", agreement_page_text)
      # accept the agreement
      agree_check_box_ceid <-  extract_first_match("createFI\\('(.{5,20})','CheckboxWidget'", agreement_page_text)
      queue = list()
      command <- createCommand(
        wid = download_pid_wid$wid,
        pid = download_pid_wid$pid,
        cid = credentials$download_panel_cid,
        cmd = 'setTarget',
        params = list(cvalue=list("agreed"), ceid=agree_check_box_ceid)
      )
      queue <- append(queue, list(command))
      command <- createCommand(
        wid = download_pid_wid$wid,
        pid = download_pid_wid$pid,
        cid = credentials$download_panel_cid,
        cmd = 'ChangeValue',
        params = list(cvalue=list("agreed"), ceid=agree_check_box_ceid)
      )
      queue <- append(queue, list(command))
      command <- createCommand(
        wid = download_pid_wid$wid,
        pid = download_pid_wid$pid,
        cid = credentials$download_panel_cid,
        cmd = 'Agreed',
        params = list(ceid=agree_check_box_ceid)
      )
      queue         <- append(queue, list(command))
      command_queue <- list(queue = queue)
      data          <- formatDataForRequest(credentials$sid, download_pid_wid$wid, download_pid_wid$pid, command_queue, timestamp())
      response      <- send_request(method = 'POST', data=data)
      response_data <- parseResponse(response)
    }

    # ---------------------------------------------------------------------------
    # Generate Download URL

    log.info(paste0(Sys.time(), " \t\tGenerating download URL."))
    ev <- createCommand(
      wid = download_pid_wid$wid,
      pid = download_pid_wid$pid,
      cid = credentials$download_panel_cid,
      cmd = download_cmd,
      params = setNames(list(), character(0)) #hack for empty {}
    )
    json_queue <- list(queue = list(ev))
    data       <- formatDataForRequest(credentials$sid, download_pid_wid$wid, download_pid_wid$pid, json_queue, timestamp())

    log.info(paste0(Sys.time(), " \t\tCompressing data."))
    response <- httr::POST(GISAID_URL, httr::add_headers(.headers = headers), body = data)
    response_data   <- parseResponse(response)

    # extract check_async
    check_async_id = strsplit(response_data$responses[[1]]$data, "'")[[1]][4]

    # while generateDownloadDone not ready
    is_ready = FALSE
    while (!is_ready) {
      response      <- httr::GET(paste0('https://www.epicov.org/epi3/check_async/', check_async_id, '?_=', timestamp()))
      response_data <- parseResponse(response)
      is_ready      <- response_data$is_ready
      if (!is_ready) { Sys.sleep(1) }
    }
    log.debug(response_data)

    # ---------------------------------------------------------------------------
    # Get Download URL

    log.info(paste0(Sys.time(), " \t\tData ready."))
    ev <- createCommand(
      wid = credentials$wid,
      pid = credentials$pid,
      cid = credentials$query_cid,
      cmd = "generateDownloadDone",
      params = setNames(list(), character(0)) #hack for empty {}
    )
    json_queue    <- list(queue = list(ev))
    data          <- formatDataForRequest(credentials$sid, credentials$wid, credentials$pid, json_queue, timestamp())
    response      <- send_request(method = 'POST', data=data)
    response_data <- parseResponse(response)

    # extract download url
    download_url <- paste0("https://www.epicov.org",strsplit(response_data$responses[[1]]$data, '"')[[1]][2])
    log.info(paste0(Sys.time(), " \t\tDownload url: ", download_url))

    # ---------------------------------------------------------------------------
    # Download File

    tryCatch({
      log.info(paste0(Sys.time(), " \t\tDownloading file: ", tmpFile))
      download.file(download_url, tmpFile, quiet = TRUE, method = 'auto')

      # Augur Input
      if (grepl("\\.tar$", tmpFile)){
        # Decompress tar archive
        untar(tmpFile, exdir="gisaidr_augur_input", restore_times = FALSE, verbose=FALSE)
        # Extract Sequences
        sequencesFile <- list.files("gisaidr_augur_input", pattern = "*.sequences.fasta")[1]
        if (is.na(sequencesFile)) { stop("Could not find sequences file.") }
        sequencesFile <- file.path("gisaidr_data_tmp", sequencesFile)
        # Extract Metadata
        metadataFile <- list.files("gisaidr_augur_input", pattern = "*.metadata.tsv")[1]
        if (is.na(metadataFile)) { stop("Could not find metadata file.") }
        metadataFile <- file.path("gisaidr_data_tmp", metadataFile)
      }

      if (!is.null(metadataFile) && file.exists(metadataFile)){
        metadata <- read.csv(metadataFile, sep="\t", quote="", check.names=FALSE)
        # Replace "?" with "NA" in the augur_input metadata
        if ( download_type == "augur_input" ) { df[ df == "?" ] <- NA }
      }
      if (!is.null(sequencesFile) && file.exists(sequencesFile)){
        sequences <- readChar(sequencesFile, file.info(tmpFile)$size)
      }

      # Decide on how we want to organize the download results based on type
      if (download_type == "augur_input"){
        download_results[[download_type]] <- list(metadata=metadata, sequences=sequences)
      } else if (download_type == "sequences"){
        download_results[[download_type]] <- sequences
      } else {
        download_results[[download_type]] <- metadata
      }

    }, finally = {
      if (clean_up){
        log.info(paste0(Sys.time(), " \t\tCleaning up temporary files."))
        if (!is.null(tmpFile)       && file.exists(tmpFile))       { file.remove(tmpFile) }
        if (!is.null(metadataFile)  && file.exists(metadataFile))  { file.remove(metadataFile) }
        if (!is.null(sequencesFile) && file.exists(sequencesFile)) { file.remove(sequencesFile) }
      }
    })
  }

  return(download_results)
}
