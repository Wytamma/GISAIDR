setColumnNames <- function(df) {
  colnames(df)[colnames(df) %in% c("b", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n")] <-
    c(
      "id",
      "virus_name",
      "passage_details_history",
      "accession_id",
      "collection_date",
      "submission_date",
      "information",
      "length",
      "host",
      "location",
      "originating_lab",
      "submitting_lab"
    )
  return(df)
}

setDataTypes <- function(df) {
  # date
  return(df)
}

#' Query GISAID Database
#'
#' @param credentials GISAID credentials.
#' @return Dataframe.
query <-
  function(credentials = credentials,
           location = NULL,
           start_index = 0,
           nrows = 50) {
    # search
    if (!is.null(location)) {
      queue = list()
      command <- createCommand(
        wid = credentials$wid,
        pid = credentials$pid,
        cid = credentials$search_CID,
        cmd = 'setTarget',
        params = list(cvalue = location, ceid = credentials$location_CID),
        equiv = paste0('ST', credentials$location_CID)
      )
      queue <- append(queue, list(command))

      command <- createCommand(
        wid = credentials$wid,
        pid = credentials$pid,
        cid = credentials$search_CID,
        cmd = 'ChangeValue',
        params = list(cvalue = location, ceid = credentials$location_CID),
        equiv = paste0('CV', credentials$location_CID)
      )

      queue <- append(queue, list(command))

      command <- createCommand(
        wid = credentials$wid,
        pid = credentials$pid,
        cid = credentials$search_CID,
        cmd = 'FilterChange',
        params = list(ceid = credentials$location_CID),
      )

      queue <- append(queue, list(command))

      command_queue <- list(queue = queue)

      data <-
        createUrlData(
          sid = credentials$sid,
          wid = credentials$wid,
          pid = credentials$pid,
          queue = command_queue,
          timestamp = timestamp()
        )
      res <-
        httr::POST(GISAID_URL, httr::add_headers(.headers = headers), body = data)
      j = httr::content(res, as = 'parsed')
    }

    # pagination
    queue = list()
    command <- createCommand(
      wid = credentials$wid,
      pid = credentials$pid,
      cid = credentials$query_cid,
      cmd = 'SetPaginating',
      params = list(start_index = start_index, rows_per_page = nrows)
    )
    queue <- append(queue, list(command))

    # get data
    command <- createCommand(
      wid = credentials$wid,
      pid = credentials$pid,
      cid = credentials$query_cid,
      cmd = 'GetData',
      params = setNames(list(), character(0)) #hack for empty {}
    )

    queue <- append(queue, list(command))

    command_queue <- list(queue = queue)

    data <-
      createUrlData(
        sid = credentials$sid,
        wid = credentials$wid,
        pid = credentials$pid,
        queue = command_queue,
        timestamp = timestamp()
      )
    res <- httr::GET(paste0(GISAID_URL, '?', data))
    j = httr::content(res, as = 'parsed')
    j <- parseResponse(res)

    if (length(j$records) > 1) {
      df <- data.frame(do.call(rbind, j$records))
      df <-
        data.frame(lapply(df, function(col) {
          col[sapply(col, is.null)] <- NA
          unlist(col)
        }))
    } else {
      df <- data.frame(j$records)
    }
    df <- setColumnNames(df)
    df <- setDataTypes(df)
    return(df)
  }
