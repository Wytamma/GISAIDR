#' Query GISAID Database
#'
#' @param credentials GISAID credentials.
#' @param text full text search.
#' @param location search for entries based on geographic location.
#' @param lineage search for entries based on pango lineage designations.
#' @param variant search for entries based on variant designation
#' @param from search from specific collection date.
#' @param to search to specific collection date.
#' @param from_subm search from specific submission date.
#' @param to_subm search to specific submission date.
#' @param virus_name search for a virus_name.
#' @param order_by order results by a column.
#' @param order_asc order_by results in ascending order.
#' @param start_index page through results.
#' @param nrows number of results to return.
#' @param load_all return all results.
#' @param low_coverage_excl exclude low coverage entries from the results.
#' @param complete include only complete entries in the results.
#' @param high_coverage include only high coverage entries in the results.
#' @param collection_date_complete include only entries with complete in collection date the results.
#' @param total returns the total number of sequences matching the query.
#' @param fast returns all of the accession_ids that match the query.
#' @param aa_substitution returns all sequences with the amino acid mutation(s), negative selection by '-' prefix
#' @param nucl_mutation returns all sequences with the nucleotide mutation(s), negative selection by '-' prefix
#' @return Dataframe.
internal_query <-
  function(credentials,
           text = NULL,
           location = NULL,
           lineage = NULL,
           variant = NULL,
           from = NULL,
           from_subm = NULL,
           to = NULL,
           to_subm = NULL,
           virus_name = NULL,
           order_by = NULL,
           aa_substitution = NULL,
           nucl_mutation = NULL,
           order_asc = TRUE,
           start_index = 0,
           nrows = 50,
           load_all = FALSE,
           low_coverage_excl = FALSE,
           complete = FALSE,
           high_coverage = FALSE,
           collection_date_complete = FALSE,
           total = FALSE,
           fast = FALSE,
           subtype = FALSE
           ) {


    df <- tryCatch({
      queue = list()

      # Simple Text Filter (EpiCoV only)
      if (!is.null(text) && credentials$database == "EpiCoV") {
        new_queue <- create_search_queue(credentials, credentials$text_ceid, text, 'DoSimpleSearch')
        queue     <- append(queue, new_queue)
      }

      # Location Filter (All)
      if (!is.null(location)) {
        new_queue <- create_search_queue(credentials, credentials$location_ceid, location, 'FilterChange')
        queue     <- append(queue, new_queue)
      }

      # Collection Date Complete (All)
      if (collection_date_complete) {
        new_queue <- create_search_queue(credentials, credentials$collection_date_complete_ceid, list('coldc'), 'FilterChange')
        queue     <- append(queue, new_queue)
      }

      # Lineage (EpiCoV, EpiPox)
      if (!is.null(lineage)) {
        if (credentials$database == "EpiCoV"){
          new_queue <- create_search_queue(credentials, credentials$lineage_ceid, lineage, 'LineageChange')
          queue     <- append(queue, new_queue)
        } else {
          new_queue <- create_search_queue(credentials, credentials$lineage_ceid, lineage, 'FilterChange')
          queue     <- append(queue, new_queue)
        }
      }

      # Subtype (EpiRSV)
      if (!is.null(subtype) && credentials$database == "EpiRSV") {
        new_queue <- create_search_queue(credentials, credentials$subtype_ceid, subtype, 'FilterChange')
        queue     <- append(queue, new_queue)
      }

      # Subtype (EpiCoV)
      if (!is.null(variant) && credentials$database == "EpiCoV") {
        new_queue <- create_search_queue(credentials, credentials$variant_ceid, variant, 'VariantsChange')
        queue     <- append(queue, new_queue)
      }

      # Virus Name (All)
      if (!is.null(virus_name)) {
        new_queue <- create_search_queue(credentials, credentials$virus_name_ceid, virus_name, 'FilterChange')
        queue     <- append(queue, new_queue)
      }

      # From, collection date (All)
      if (!is.null(from)) {
        new_queue <- create_search_queue(credentials, credentials$from_ceid, from, 'FilterChange')
        queue     <- append(queue, new_queue)
      }

      # From, submission date (All)
      if (!is.null(from_subm)) {
        new_queue <- create_search_queue(credentials, credentials$from_sub_ceid, from_subm, 'FilterChange')
        queue     <- append(queue, new_queue)
      }

      # To, collection date (All)
      if (!is.null(to)) {
        new_queue <- create_search_queue(credentials, credentials$to_ceid, to, 'FilterChange')
        queue     <- append(queue, new_queue)
      }

      # To, submission date (All)
      if (!is.null(to_subm)) {
        new_queue <- create_search_queue(credentials, credentials$to_sub_ceid, to_subm, 'FilterChange')
        queue     <- append(queue, new_queue)
      }

      # Amino acid changes (all)
      if (!is.null(aa_substitution)) {
        new_queue <- create_search_queue(credentials, credentials$aa_substitution_ceid, aa_substitution, 'FilterChange')
        queue     <- append(queue, new_queue)
      }

      # Nucleotide changes (EpiCoV)
      if (!is.null(nucl_mutation) && credentials$database == "EpiCoV") {
        new_queue <- create_search_queue(credentials, credentials$nucl_mutation_ceid, nucl_mutation, 'FilterChange')
        queue     <- append(queue, new_queue)
      }

      # Low Coverage Exclude (All)
      if (low_coverage_excl) {
        new_queue <- create_search_queue(credentials, credentials$low_coverage_excl_ceid, list('lowco'), 'FilterChange')
        queue     <- append(queue, new_queue)
      }

      # Quality (mixed)
      if (credentials$database == 'EpiCoV') {
        if (complete) {
          new_queue <- create_search_queue(credentials, credentials$completed_ceid, list('complete'), 'FilterChange')
          queue     <- append(queue, new_queue)
        }
        if (high_coverage) {
          new_queue <- create_search_queue(credentials, credentials$highq_ceid, list('highq'), 'FilterChange')
          queue     <- append(queue, new_queue)
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
          new_queue <- create_search_queue(credentials, credentials$quality_ceid, quality, 'FilterChange')
          queue     <- append(queue, new_queue)
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
        res <- httr::POST(GISAID_URL, httr::add_headers(.headers = headers), body = data)
      }

      queue = list()

      # ordering
      if (!is.null(order_by)) {
        if (credentials$database == 'EpiCoV') {
          order_by = covid_order_by_col_map[[order_by]]
        } else {
          # epipox and epirsv are missing the host column
          order_by = other_order_by_col_map[[order_by]]
        }
        command <- createCommand(
          wid = credentials$wid,
          pid = credentials$pid,
          cid = credentials$query_cid,
          cmd = 'SetSorting',
          params = list(order_by = order_by, order_asc = order_asc)
        )
        queue <- append(queue, list(command))
      }

      # pagination
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
      response <- httr::GET(paste0(GISAID_URL, '?', data))
      response_data <- parseResponse(response)

      if (total) {
        return(as.numeric(response_data$totalRecords))
      }

      if (fast) {
        log.debug(paste0(GISAID_URL, "?sid=", credentials$sid))
        accession_id_count <- response_data$totalRecords
        log.info(paste('Selecting all', accession_id_count, "accession_ids."))
        df <- get_accession_ids(credentials = credentials)
        log.info(paste("Returning", start_index, "-", nrow(df), "of", accession_id_count, "accession_ids."))

        if (accession_id_count > nrow(df)) {
          log.warn(paste("Could only get", nrow(df), "accession_ids. Narrow your search."))
        }
        return(df)
      }

      if (load_all && response_data$totalRecords > nrows) {
        log.info(paste("Loading all", response_data$totalRecords, "entries."))
        return(
          query(
            credentials = credentials,
            text = text,
            location = location,
            lineage = lineage,
            variant = variant,
            from = from,
            from_subm = from_subm,
            to = to,
            to_subm = to_subm,
            aa_substitution = aa_substitution,
            nucl_mutation = nucl_mutation,
            nrows = response_data$totalRecords,
            # set load_all to false to break the recursion
            load_all = FALSE,
            low_coverage_excl = low_coverage_excl,
            complete = complete,
            high_coverage = high_coverage,
            collection_date_complete = collection_date_complete,
            subtype = subtype
          )
        )
      }
      log.info(paste("Returning", start_index, "-", start_index + response_data$recordsReturned, "of ", response_data$totalRecords, "entries."))
      log.debug(response_data$records)
      if (length(response_data$records) >= 1) {
        df <- data.frame(do.call(rbind, response_data$records))
        df <-
          data.frame(lapply(df, function(col) {
            col[sapply(col, is.null)] <- NA
            unlist(col)
          }))
      } else {
        df <- data.frame(response_data$records)
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
