epiflu_query <- function(credentials,
                         text = NULL,
                         location = NULL,
                         lineage = NULL,
                         from = NULL,
                         from_subm = NULL,
                         to = NULL,
                         to_subm = NULL,
                         total = FALSE,
                         fast = FALSE,
                         start_index = 0) {
  df <- tryCatch({
    GISAID_URL <- credentials$url
    queue = list()
    results_page_text = NULL

    # reset page
    send_request(paste0('sid=', credentials$sid), url=GISAID_URL)

    if (!is.null(text)) {
      queue <-
        append(
          queue,
          create_search_queue(
            credentials,
            credentials$text_ceid,
            text,
            'DoSimpleSearch'
          )
        )
    }
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
          create_search_queue(
            credentials,
            credentials$from_ceid,
            from,
            'OnlyCount'
          )
        )
    }

    if (!is.null(from_subm)) {
      queue <-
        append(
          queue,
          create_search_queue(
            credentials,
            credentials$from_subm_ceid,
            from_subm,
            'OnlyCount'
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
                              'OnlyCount')
        )
    }

    if (!is.null(to_subm)) {
      queue <-
        append(
          queue,
          create_search_queue(
            credentials,
            credentials$to_subm_ceid,
            to_subm,
            'OnlyCount'
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


      response <- send_request(data=data, method = 'POST', url = GISAID_URL)
      response_data <- parseResponse(response)
    }
    # send search cmd

    search_command <- createCommand(
      wid = credentials$wid,
      pid = credentials$pid,
      cid = credentials$submit_cid,
      cmd = 'search',
      params = setNames(list(), character(0))  # hack for empty {}
    )

    queue <- list(queue = list(search_command))

    data <-
      formatDataForRequest(credentials$sid, credentials$wid, credentials$pid, queue, timestamp())

    response <- send_request(data=data, method="POST", url=GISAID_URL)
    response_data <- parseResponse(response)

    # got to results page
    response_text <- httr::content(response, as = 'text')


    # --- results page

    results_page_ID <-
      extract_first_match("sys.goPage\\('(.*)')", response_text)


    results_page <-
      send_request(paste0('sid=', credentials$sid, '&pid=', results_page_ID), url=GISAID_URL)
    results_page_text = httr::content(results_page, as = 'text')

    # extract get data cid
    getdata_cid <- extract_first_match("sys.createComponent\\('(.{5,30})','IsolateResultListComponent'", results_page_text)

    # get data

    queue = list()

    # # ordering
    # if (!is.null(order_by)) {
    #   if (credentials$database == 'EpiCoV') {
    #     order_by = covid_order_by_col_map[[order_by]]
    #   } else {
    #     # epipox and epirsv are missing the host column
    #     order_by = other_order_by_col_map[[order_by]]
    #   }
    #   command <- createCommand(
    #     wid = credentials$wid,
    #     pid = credentials$pid,
    #     cid = credentials$query_cid,
    #     cmd = 'SetSorting',
    #     params = list(order_by = order_by, order_asc = order_asc)
    #   )
    #   queue <- append(queue, list(command))
    # }
    #
    # # pagination
    # command <- createCommand(
    #   wid = credentials$wid,
    #   pid = credentials$pid,
    #   cid = credentials$query_cid,
    #   cmd = 'SetPaginating',
    #   params = list(start_index = start_index, rows_per_page = nrows)
    # )
    # queue <- append(queue, list(command))

    # get data
    command <- createCommand(
      wid = credentials$wid,
      pid = results_page_ID,
      cid = getdata_cid,
      cmd = 'GetData'
    )

    queue <- append(queue, list(command))

    command_queue <- list(queue = queue)

    data <-
      formatDataForRequest(
        sid = credentials$sid,
        wid = credentials$wid,
        pid = results_page_ID,
        queue = command_queue,
        timestamp = timestamp()
      )
    res <- send_request(data, url = GISAID_URL)
    j <- parseResponse(res)

    if (total) {
      return(as.numeric(j$totalRecords))
    }

    if (fast) {
      log.debug(paste0(GISAID_URL, "?sid=", credentials$sid))
      accession_id_count <- j$totalRecords
      message(paste0('Selecting all ', accession_id_count, " accession_ids."))
      df <- get_accession_ids(credentials = credentials)

      message(
        paste0(
          "Returning ",
          start_index,
          "-",
          nrow(df),
          " of ",
          accession_id_count,
          " accession_ids."
        )
      )

      if (accession_id_count > nrow(df)) {
        message(paste0(
          "Could only get ",
          nrow(df),
          " accession_ids. Narrow your search."
        ))
      }
      return(df)

    }

    # if (load_all && j$totalRecords > nrows) {
    #   message(paste0("Loading all ", j$totalRecords, " entries..."))
    #   return(
    #     query(
    #       credentials = credentials,
    #       text = text,
    #       location = location,
    #       lineage = lineage,
    #       variant = variant,
    #       from = from,
    #       from_subm = from_subm,
    #       to = to,
    #       to_subm = to_subm,
    #       aa_substitution = aa_substitution,
    #       nucl_mutation = nucl_mutation,
    #       nrows = j$totalRecords,
    #       # set load_all to false to break the recursion
    #       load_all = FALSE,
    #       low_coverage_excl = low_coverage_excl,
    #       complete = complete,
    #       high_coverage = high_coverage,
    #       collection_date_complete = collection_date_complete,
    #     )
    #   )
    # }
    message(
      paste0(
        "Returning ",
        start_index,
        "-",
        start_index + j$recordsReturned,
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

      cleaned_data <- apply(df, MARGIN = c(1,2), FUN = remove_html_tags)
      # Convert the result back to a data frame
      df <- as.data.frame(cleaned_data, stringsAsFactors = FALSE)
    } else {
      df <- data.frame(j$records)
    }
    df <- setColumnNames(df, credentials$database)
    df <- setDataTypes(df)
  },

  finally = {
    # send go back cmd
    # IsolateDownloadButtonComponent
    if (!is.null(results_page_text)) {
      goback_cid <- extract_first_match("sys.createComponent\\('(.{5,30})','IsolateDownloadButtonComponent'", results_page_text)

      # get data
      command <- createCommand(
        wid = credentials$wid,
        pid = results_page_ID,
        cid = goback_cid,
        cmd = 'GoBack'
      )

      queue <- append(queue, list(command))

      command_queue <- list(queue = queue)

      data <-
        formatDataForRequest(
          sid = credentials$sid,
          wid = credentials$wid,
          pid = results_page_ID,
          queue = command_queue,
          timestamp = timestamp()
        )
      res <- send_request(data, url = GISAID_URL)
    }
    # reset search params
    resetQuery(credentials, url = GISAID_URL)
  })
  return(df)
}
