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

create_search_queue <- function(credentials, ceid, cvalue, cmd) {
  queue = list()
  command <- createCommand(
    wid = credentials$wid,
    pid = credentials$pid,
    cid = credentials$search_cid,
    cmd = 'setTarget',
    params = list(cvalue = cvalue, ceid = ceid),
    equiv = paste0('ST', ceid)
  )
  queue <- append(queue, list(command))

  command <- createCommand(
    wid = credentials$wid,
    pid = credentials$pid,
    cid = credentials$search_cid,
    cmd = 'ChangeValue',
    params = list(cvalue = cvalue, ceid = ceid),
    equiv = paste0('CV', ceid)
  )

  queue <- append(queue, list(command))

  command <- createCommand(
    wid = credentials$wid,
    pid = credentials$pid,
    cid = credentials$search_cid,
    cmd = cmd,
    params = list(ceid = ceid),
  )

  queue <- append(queue, list(command))

  return(queue)
}

#' Query GISAID Database
#'
#' @param credentials GISAID credentials.
#' @return Dataframe.
query <-
  function(credentials = credentials,
           location = NULL,
           lineage = NULL,
           from = NULL,
           to = NULL,
           start_index = 0,
           nrows = 50,
           load_all = FALSE,
           low_coverage_excl = FALSE) {
    # search
    queue = list()
    if (!is.null(location)) {
      queue <-
        append(
          queue,
          create_search_queue(
            credentials,
            credentials$location_ceid,
            location,
            'FilterChange'
          )
        )
    }
    if (!is.null(lineage)) {
      queue <-
        append(
          queue,
          create_search_queue(
            credentials,
            credentials$linage_ceid,
            lineage,
            'LineageChange'
          )
        )
    }
    if (!is.null(from)) {
      queue <-
        append(
          queue,
          create_search_queue(credentials,
                              credentials$from_ceid,
                              from,
                              'FilterChange')
        )
    }
    if (!is.null(to)) {
      queue <-
        append(
          queue,
          create_search_queue(credentials,
                              credentials$to_ceid,
                              to,
                              'FilterChange')
        )
    }
    if (low_coverage_excl) {
      queue <-
        append(
          queue,
          create_search_queue(credentials,
                              credentials$low_coverage_excl_ceid,
                              list('lowco'),
                              'FilterChange')
        )
    }
    if (length(queue) > 0) {
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
      cmd = 'GetData'
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
    j <- parseResponse(res)
    if (load_all && j$totalRecords > nrows) {
      message(paste0("Loading all ", j$totalRecords, " entries..."))
      return(
        query(
          credentials = credentials,
          location = location,
          lineage = lineage,
          from = from,
          to = to,
          nrows = j$totalRecords,
          load_all = FALSE, # set to false to break the recursion
          low_coverage_excl = low_coverage_excl
        )
      )
    }
    if (nrows > j$totalRecords) {
      nrows <- j$totalRecords
    }
    message(paste0(
      "Returning ",
      start_index,
      "-",
      start_index + nrows,
      " of ",
      j$totalRecords,
      " entries."
    ))
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

    # reset search params
    resetQuery(credentials)

    return(df)
  }
