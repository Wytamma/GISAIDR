
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
#' @return data.frame
query <-
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
           fast = FALSE) {

    if (nrows > 50 && !total && !load_all && !fast) {
      message(paste0("Loading entries in batches..."))
      batches <- create_batches(start_index = start_index, nrows = nrows)
      results <- data.frame()
      for (i in 1:nrow(batches)) {
        results <- rbind(results, internal_query(
          credentials = credentials,
          location = location,
          lineage = lineage,
          variant = variant,
          from = from,
          from_subm = from_subm,
          to = to,
          to_subm = to_subm,
          virus_name = virus_name,
          start_index = batches[i,1],
          nrows = batches[i,2],
          low_coverage_excl = low_coverage_excl,
          complete = complete,
          high_coverage = high_coverage,
          collection_date_complete = collection_date_complete
        ))
      }
      return(results)
    } else if (total | load_all | fast) {
      return(
        internal_query(
          credentials = credentials,
          location = location,
          lineage = lineage,
          variant = variant,
          from = from,
          from_subm = from_subm,
          to = to,
          to_subm = to_subm,
          virus_name = virus_name,
          load_all = load_all,
          low_coverage_excl = low_coverage_excl,
          complete = complete,
          high_coverage = high_coverage,
          collection_date_complete = collection_date_complete,
          total = total,
          fast = fast
        )
      )
    } else  {
      return(
        internal_query(
          credentials = credentials,
          location = location,
          lineage = lineage,
          variant = variant,
          from = from,
          from_subm = from_subm,
          to = to,
          to_subm = to_subm,
          virus_name = virus_name,
          start_index = start_index,
          nrows = nrows,
          low_coverage_excl = low_coverage_excl,
          complete = complete,
          high_coverage = high_coverage,
          collection_date_complete = collection_date_complete
        )
      )
    }


  }
