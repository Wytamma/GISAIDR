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
      "lenght",
      "host",
      "location",
      "originating_lab",
      "submitting_lab"
    )
  return(df)
}


#' Query GISAID Database
#'
#' @param credentials GISAID credentials.
#' @return Dataframe.
getData <-
  function(credentials = credentials,
           start_index = 0,
           nrows = 50) {
    queue = list()


    # pagination
    command <- createCommand(
      wid = credentials$wid,
      pid = credentials$pid,
      cid = credentials$search_cid,
      cmd = 'SetPaginating',
      params = list(start_index = start_index, rows_per_page = nrows)
    )
    queue <- append(queue, list(command))

    # get data
    command <- createCommand(
      wid = credentials$wid,
      pid = credentials$pid,
      cid = credentials$search_cid,
      cmd = 'GetData',
      params = setNames(list(), character(0)) #hack for empty {}
    )

    queue <- append(queue, list(command))

    command_queue <- list(queue = queue)
    ts = as.character(as.integer(Sys.time()) * 1000)
    data <-
      createUrlData(
        sid = credentials$sid,
        wid = credentials$wid,
        pid = credentials$pid,
        queue = command_queue,
        ts = ts
      )
    res <- httr::GET(paste0(GISAID_URL, '?', data))
    j = httr::content(res, as = 'parsed')

    if ("responses" %in% names(j)) {
      if ("expired." %in% strsplit(j$responses[[1]]$data, " ")[[1]]) {
        stop("The session has expired. Please login again.")
      } else {
        stop("An error has occured.")
      }
    }
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
    return(setColumnNames(df))
  }
