setColumnNames <- function(df, database) {
  if (database == 'EpiRSV'){
    names(df)[names(df) == "b"] <- "id"
    names(df)[names(df) == "d"] <- "virus_name"
    names(df)[names(df) == "e"] <- "passage_details_history"
    names(df)[names(df) == "f"] <- "accession_id"
    names(df)[names(df) == "g"] <- "collection_date"
    names(df)[names(df) == "h"] <- "submission_date"
    names(df)[names(df) == "i"] <- "information"
    names(df)[names(df) == "j"] <- "length"
    names(df)[names(df) == "k"] <- "location"
    names(df)[names(df) == "l"] <- "originating_lab"
    names(df)[names(df) == "m"] <- "submitting_lab"
  } else {
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
  }
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
#' @param location search for entries based on geographic location.
#' @param lineage search for entries based on pango lineage designations.
#' @param from search from specific collection date.
#' @param to search to specific collection date.
#' @param from_subm search from specific submission date.
#' @param to_subm search to specific submission date.
#' @param start_index page through results.
#' @param nrows number of results to return.
#' @param load_all return all results.
#' @param low_coverage_excl exclude low coverage entries from the results.
#' @param complete include only complete entries in the results.
#' @param high_coverage include only high coverage entries in the results.
#' @param collection_date_complete include only entries with complete in collection date the results.
#' @return Dataframe.
query <-
  function(credentials,
           location = NULL,
           lineage = NULL,
           from = NULL,
           from_subm = NULL,
           to = NULL,
           to_subm = NULL,
           start_index = 0,
           nrows = 50,
           load_all = FALSE,
           low_coverage_excl = FALSE,
           complete = FALSE,
           high_coverage = FALSE,
           collection_date_complete = FALSE) {
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
    if (collection_date_complete) {
      queue <-
        append(
          queue,
          create_search_queue(
            credentials,
            credentials$collection_date_complete_ceid,
            list('coldc'),
            'FilterChange'
          )
        )
    }
    if (!is.null(lineage) && credentials$database == "EpiCoV") {
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

    if (!is.null(from_subm)) {
      queue <-
        append(
          queue,
          create_search_queue(
            credentials,
            credentials$from_sub_ceid,
            from_subm,
            'FilterChange'
          )
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

    if (!is.null(to_subm)) {
      queue <-
        append(
          queue,
          create_search_queue(
            credentials,
            credentials$to_sub_ceid,
            to_subm,
            'FilterChange'
          )
        )
    }

    if (low_coverage_excl) {
      queue <-
        append(
          queue,
          create_search_queue(
            credentials,
            credentials$low_coverage_excl_ceid,
            list('lowco'),
            'FilterChange'
          )
        )
    }
    quality <- list()

    if (complete) {
      quality <- append(quality, 'complete')
    }
    if (high_coverage) {
      quality <- append(quality, 'highq')
    }

    if (length(quality) > 0) {
      queue <-
        append(
          queue,
          create_search_queue(
            credentials,
            credentials$quality_ceid,
            quality,
            'FilterChange'
          )
        )
    }

    if (length(queue) > 0) {
      command_queue <- list(queue = queue)
      data <-
        formatDataForRequest(
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
      formatDataForRequest(
        sid = credentials$sid,
        wid = credentials$wid,
        pid = credentials$pid,
        queue = command_queue,
        timestamp = timestamp()
      )
    res <- httr::GET(paste0(GISAID_URL, '?', data))
    j <- parseResponse(res)

    # Load all
    if (load_all && j$totalRecords > nrows) {
      message(paste0("Loading all ", j$totalRecords, " entries..."))
      return(
        query(
          credentials = credentials,
          location = location,
          lineage = lineage,
          from = from,
          from_subm = from_subm,
          to = to,
          to_subm = to_subm,
          nrows = j$totalRecords,
          load_all = FALSE, # set to false to break the recursion
          low_coverage_excl = low_coverage_excl,
          complete = complete,
          high_coverage = high_coverage,
          collection_date_complete = collection_date_complete,
        )
      )
    }
    if (nrows > j$totalRecords) {
      nrows <- j$totalRecords
    }
    message(
      paste0(
        "Returning ",
        start_index,
        "-",
        start_index + nrows,
        " of ",
        j$totalRecords,
        " entries."
      )
    )
    log.debug(j$records)
    if (length(j$records) >= 1) {
      df <- data.frame(do.call(rbind, j$records))
      df <-
        data.frame(lapply(df, function(col) {
          col[sapply(col, is.null)] <- NA
          unlist(col)
        }))
    } else {
      df <- data.frame(j$records)
    }
    df <- setColumnNames(df, credentials$database)
    df <- setDataTypes(df)

    # reset search params
    resetQuery(credentials)
    return(df)
  }
