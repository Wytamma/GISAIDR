


#' Download from GISAID
#'
#' @param list_of_accession_ids list of accession_id from GISAID
#' @return data.frame of complete data
download <- function(credentials, list_of_accession_ids, get_sequence=FALSE) {
  # select rows
  # POST
  # {"queue":[{"wid":"wid_qsbxxp_1q6r","pid":"pid_qsbxxp_1q6s","cid":"c_qsbxxp_wu","cmd":"setTarget","params":{"cvalue":"EPI_ISL_1804387, EPI_ISL_1804911, EPI_ISL_1804916, EPI_ISL_1804923, EPI_ISL_1804970","ceid":"ce_qsbxxp_cf"},"equiv":"STce_qsbxxp_cf"},{"wid":"wid_qsbxxp_1q6r","pid":"pid_qsbxxp_1q6s","cid":"c_qsbxxp_wu","cmd":"ChangeValue","params":{"cvalue":"EPI_ISL_1804387, EPI_ISL_1804911, EPI_ISL_1804916, EPI_ISL_1804923, EPI_ISL_1804970","ceid":"ce_qsbxxp_cf"},"equiv":"CVce_qsbxxp_cf"},{"wid":"wid_qsbxxp_1q6r","pid":"pid_qsbxxp_1q6s","cid":"c_qsbxxp_wu","cmd":"OK","params":{},"equiv":null}]}
  # download
  # {"queue":[{"wid":"wid_qsbxxp_1pe8","pid":"pid_qsbxxp_1pe9","cid":"c_qsbxxp_yq","cmd":"SetTargetColumn","params":{"col":"m"},"equiv":null},{"wid":"wid_qsbxxp_1pe8","pid":"pid_qsbxxp_1pe9","cid":"c_qsbxxp_yq","cmd":"ToolDownload","params":{},"equiv":null}]}
  # Wait for downlaod
  # async
  #  https://www.epicov.org/epi3/check_async/_qsbxxp_1qc3?_=1619760721443
  # _qsbxxp_1qc3 from download response
  # select
  accession_ids_string <- paste(list_of_accession_ids, collapse=", ")

  ev1 <- createCommand(
    wid = credentials$wid,
    pid = credentials$selection_PID,
    cid = credentials$panel_CID,
    cmd = 'setTarget',
    params = list(cvalue=accession_ids_string, ceid=credentials$selection_CID), #hack for empty {}
    equiv = paste0("ST", credentials$selection_CID)
  )

  ev2 <- createCommand(
    wid = credentials$wid,
    pid = credentials$selection_PID,
    cid = credentials$panel_CID,
    cmd = 'ChangeValue',
    params = list(cvalue=accession_ids_string, ceid=credentials$selection_CID), #hack for empty {}
    equiv = paste0("CV", credentials$selection_CID)
  )

  ev3 <- createCommand(
    wid = credentials$wid,
    pid = credentials$selection_PID,
    cid = credentials$panel_CID,
    cmd = 'OK',
    params = setNames(list(), character(0)) #hack for empty {}
  )
  json_queue <- list(queue = list(ev1, ev2, ev3))
  data <- createUrlData(credentials$sid, credentials$wid, credentials$selection_PID, json_queue, timestamp())
  res <-
    httr::POST(GISAID_URL, httr::add_headers(.headers = headers), body = data)
  #{"queue":[{"wid":"wid_qsdwp3_ra2","pid":"pid_qsdwp3_ra3","cid":"c_qsdwp3_yg","cmd":"ToolDownload","params":{},"equiv":null}]}
  # maybe need to do all this?
  # {"wid":"wid_qstszr_wqj","pid":"pid_qstszr_wqk","cid":"c_qstszr_y4","cmd":"SetTargetColumn","params":{"col":"n"},"equiv":null},{"wid":"wid_qstszr_wqj","pid":"pid_qstszr_wqk","cid":"c_qstszr_y4","cmd":"ToolDownload","params":{},"equiv":null}
  ev <- createCommand(
    wid = credentials$wid,
    pid = credentials$pid,
    cid = credentials$download_cid,
    cmd = 'ToolDownload',
    params = setNames(list(), character(0)) #hack for empty {}
  )
  json_queue <- list(queue = list(ev))
  data <- createUrlData(credentials$sid, credentials$wid, credentials$pid, json_queue, timestamp())
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
  }
  # get download link
  ev$cmd <- "generateDownloadDone"
  json_queue <- list(queue = list(ev))
  data <- createUrlData(credentials$sid, credentials$wid, credentials$pid, json_queue, timestamp())
  res <-
    httr::POST(GISAID_URL, httr::add_headers(.headers = headers), body = data)
  j <- parseResponse(res)
  # reset <- httr::GET(paste0(GISAID_URL, '?sid=', credentials$SID, '&pid=', strsplit(j$responses[[4]]$data, "'")[[1]][2]))
  # extract download url
  download_url <- paste0("https://www.epicov.org",strsplit(j$responses[[1]]$data, '"')[[1]][2])
  # download zip
  tryCatch({
    download.file(download_url, "gisaid_data.tar", quiet = TRUE)
    # unzip
    untar("gisaid_data.tar", exdir="gisaid_data")
    # load into df
    con <- xzfile(paste0("gisaid_data/", list.files("gisaid_data", pattern = "*.metadata.tsv.xz")[1]), open = 'r')
    df <- read.csv(con, sep="\t")
    close(con)
    if (get_sequence) {
      # join sequence
      con <- xzfile(paste0("gisaid_data/", list.files("gisaid_data", pattern = "*.sequences.fasta.xz")[1]), open = 'r')
      seq_df <- read_fasta(con)
      close(con)
      df <- merge(x = df, y = seq_df, by = "strain", all = TRUE)
    }
  }, finally = {
    # clean up
    if (file.exists("gisaid_data.tar")) {
      file.remove("gisaid_data.tar")
    }
    if (file.exists("gisaid_data/")) {
      unlink("gisaid_data/", recursive = TRUE)
    }
  })


  return(df)
}
