#' Query GISAID Database
#'
#' @param credentials GISAID credentials.
#' @param location search for entries based on geographic location.
#' @param lineage search for entries based on pango lineage designations.
#' @param variant search for entries based on variant designation
#' @param from search from specific collection date.
#' @param to search to specific collection date.
#' @param from_subm search from specific submission date.
#' @param to_subm search to specific submission date.
#' @param virus_name search for a virus_name.
#' @param start_index page through results.
#' @param nrows number of results to return.
#' @param load_all return all results.
#' @param low_coverage_excl exclude low coverage entries from the results.
#' @param complete include only complete entries in the results.
#' @param high_coverage include only high coverage entries in the results.
#' @param collection_date_complete include only entries with complete in collection date the results.
#' @param total returns the total number of sequences matching the query.
#' @param fast returns all of the accession_ids that match the query.
#' @return Dataframe.
internal_query <-
  function(credentials,
           location = NULL,
           lineage = NULL,
           variant = NULL,
           from = NULL,
           from_subm = NULL,
           to = NULL,
           to_subm = NULL,
           virus_name = NULL,
           start_index = 0,
           nrows = 50,
           load_all = FALSE,
           low_coverage_excl = FALSE,
           complete = FALSE,
           high_coverage = FALSE,
           collection_date_complete = FALSE,
           total = FALSE,
           fast= FALSE) {
    df <- tryCatch({
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
      if (!is.null(variant) && credentials$database == "EpiCoV") {
        queue <-
          append(
            queue,
            create_search_queue(
              credentials,
              credentials$variant_ceid,
              variant,
              'VariantsChange'
            )
          )
      }
      if (!is.null(virus_name)) {
        queue <-
          append(
            queue,
            create_search_queue(
              credentials,
              credentials$virus_name_ceid,
              virus_name,
              'FilterChange'
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

      if (credentials$database == 'EpiCoV') {
        if (complete) {
          queue <-
            append(
              queue,
              create_search_queue(
                credentials,
                credentials$complete_ceid,
                list('complete'),
                'FilterChange'
              )
            )
        }

        if (high_coverage) {
          queue <-
            append(
              queue,
              create_search_queue(
                credentials,
                credentials$highq_ceid,
                list('highq'),
                'FilterChange'
              )
            )
        }
      } else {
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
          message(paste0("Could only get ", nrow(df), " accession_ids. Narrow your search."))
        }
        return(df)

      }

      if (load_all && j$totalRecords > nrows) {
        message(paste0("Loading all ", j$totalRecords, " entries..."))
        return(
          query(
            credentials = credentials,
            location = location,
            lineage = lineage,
            variant = variant,
            from = from,
            from_subm = from_subm,
            to = to,
            to_subm = to_subm,
            nrows = j$totalRecords,
            load_all = FALSE,
            # set load_all to false to break the recursion
            low_coverage_excl = low_coverage_excl,
            complete = complete,
            high_coverage = high_coverage,
            collection_date_complete = collection_date_complete,
          )
        )
      }
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
      } else {
        df <- data.frame(j$records)
      }
      df <- setColumnNames(df, credentials$database)
      df <- setDataTypes(df)
    },
    finally = {
      # reset search params
      resetQuery(credentials)
    })
    return(df)
  }
