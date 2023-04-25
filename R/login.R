

#' Login to GISAID
#'
#' @param username GISAID username.
#' @param password GISAID password.
#' @return credentials used to query GISAID.
#' @examples
#' username = Sys.getenv("GISAIDR_USERNAME")
#' password = Sys.getenv("GISAIDR_PASSWORD")
#' login(username, password)
login <- function(username, password, database="EpiCoV") {
  if (!database %in% c("EpiCoV", "EpiRSV", "EpiPox")) {
    stop(sprintf("Database must be EpiCoV, EpiRSV or EpiPox (database=%s)", database))
  }
  # get a session ID
  response <- send_request()
  home_page_text = httr::content(response, as = 'text')
  session_id <-
    extract_first_match("name=\"sid\" value='([^']*)", home_page_text)
  response <- send_request(paste0('sid=', session_id))

  # Load the home page
  # TODO: ensure it's the covid-19 page
  login_page_text = httr::content(response, as = 'text')

  # extract the other IDs for log in stage 1
  WID <- extract_first_match('"WID"] = "([^"]*)', login_page_text)
  login_page_ID <-
    extract_first_match('PID"] = "([^"]*)', login_page_text)
  login_component_ID <-
    extract_first_match("sys.getC\\('(.*)').call\\('doLogin'", login_page_text)

  # frontend page
  # create doLogin command
  doLogin_command <- createCommand(
    wid = WID,
    pid = login_page_ID,
    cid = login_component_ID,
    cmd = 'doLogin',
    params = list(login = username, hash = openssl::md5(password))
  )

  # add the doLogin command to queue
  # commands can be built up into a pipeline in the queue
  queue <- list(queue = list(doLogin_command))
  data <-
    formatDataForRequest(session_id, WID, login_page_ID, queue, timestamp())

  response <- send_request(method = 'POST', data=data)
  response_data <- parseResponse(response)

  if (length(grep("^sys.goPage", response_data$responses[[1]]$data)) == 0) {
    # handle event that the default page is not frontend
    default_page <- send_request(paste0('sid=', session_id))
    default_page_text = httr::content(default_page, as = 'text')

    EpiCov_CID <- extract_first_match("sys.call\\('(.{5,20})','Go'", default_page_text)

    goto_EpiCov_page_command <- createCommand(
      wid = WID,
      pid = login_page_ID,
      cid = EpiCov_CID,
      cmd = 'Go',
      params = list(page = 'corona2020')
    )

    queue <- list(queue = list(goto_EpiCov_page_command))

    data <-
      formatDataForRequest(session_id, WID, login_page_ID, queue, timestamp())

    response <- send_request(data)
    response_data <- parseResponse(response)
  }

  frontend_page_ID <- extract_first_match("\\('(.*)')",response_data$responses[[1]]$data)

  frontend_page <- send_request(paste0('sid=', session_id, '&pid=', frontend_page_ID))
  frontend_page_text = httr::content(frontend_page, as = 'text')
  if (grepl('sys.openOverlay', frontend_page_text, fixed = TRUE)) {
    # extract overlay pid
    overlay_window_ID <- paste0('wid_', extract_first_match("openOverlay\\('wid_(.{5,20})','pid_.{5,20}'", frontend_page_text))
    overlay_page_ID <- paste0('pid_', extract_first_match("openOverlay\\('wid_.{5,20}','pid_(.{5,20})'", frontend_page_text))

    # load overlay
    overlay_page <- send_request(paste0('sid=', session_id, '&pid=', overlay_page_ID))
    overlay_page_text <- httr::content(overlay_page, as = 'text')

    # extract close cid
    close_overlay_cid <- extract_first_match("createComponent\\('(.{5,20})','CloseButtonComponent", overlay_page_text)

    # send close cmd
    close_overlay_command <- createCommand(
      wid = overlay_window_ID,
      pid = overlay_page_ID,
      cid = close_overlay_cid,
      cmd = 'Back'
    )
    queue <- list(queue = list(close_overlay_command))
    data <-
      formatDataForRequest(session_id, overlay_window_ID, overlay_page_ID, queue, timestamp())
    response <- send_request(data)
    response_data <- parseResponse(response)
  }

  if (database=="EpiRSV") {
    EpiRSV_CID <- extract_first_match("sys.call\\('(.{5,20})','Go'", frontend_page_text)

    goto_EpiRSV_page_command <- createCommand(
      wid = WID,
      pid = frontend_page_ID,
      cid = EpiRSV_CID,
      cmd = 'Go',
      params = list(page = 'rsv')
    )

    queue <- list(queue = list(goto_EpiRSV_page_command))

    data <-
      formatDataForRequest(session_id, WID, login_page_ID, queue, timestamp())

    response <- send_request(data)
    response_data <- parseResponse(response)
    RSV_page_ID <-
      extract_first_match("\\('(.*)')",response_data$responses[[1]]$data)
    RSV_page <- send_request(paste0('sid=', session_id, '&pid=', RSV_page_ID))
    RSV_page_text = httr::content(RSV_page, as = 'text')

    RSV_actionbar_component_ID <-
      extract_first_match("sys-actionbar-action.*\" onclick=\"sys.getC\\('([^']*)",
                          RSV_page_text)

    response_data <- go_to_page(session_id, WID, RSV_page_ID, RSV_actionbar_component_ID, 'page_rsv.RSVBrowsePage')
    customSearch_page_ID <-
      extract_first_match("\\('(.*)')",response_data$responses[[1]]$data)
  } else if (database=="EpiPox") {
    EpiPox_CID <- extract_first_match("sys.call\\('(.{5,20})','Go'", frontend_page_text)

    goto_EpiPox_page_command <- createCommand(
      wid = WID,
      pid = frontend_page_ID,
      cid = EpiPox_CID,
      cmd = 'Go',
      params = list(page = 'mpox')
    )

    queue <- list(queue = list(goto_EpiPox_page_command))

    data <-
      formatDataForRequest(session_id, WID, login_page_ID, queue, timestamp())

    response <- send_request(data)
    response_data <- parseResponse(response)
    # POX_page_ID <-
    #   extract_first_match("\\('(.*)')",response_data$responses[[1]]$data)
    # POX_page <- send_request(paste0('sid=', session_id, '&pid=', POX_page_ID))
    # POX_page_text = httr::content(POX_page, as = 'text')
    #
    # POX_actionbar_component_ID <-
    #   extract_first_match("sys-actionbar-action.*\" onclick=\"sys.getC\\('([^']*)",
    #                       POX_page_text)
    # response_data <- go_to_page(session_id, WID, POX_page_ID, POX_actionbar_component_ID, 'page_mpox.MPoxBrowsePage')
    customSearch_page_ID <-
      extract_first_match("\\('(.*)')",response_data$responses[[1]]$data)
  } else {
    # check for overlay

    COVID_actionbar_component_ID <-
      extract_first_match("sys-actionbar-action.*\" onclick=\"sys.getC\\('([^']*)",
                          frontend_page_text)
    response_data <- go_to_page(session_id, WID, frontend_page_ID, COVID_actionbar_component_ID, 'page_corona2020.Corona2020BrowsePage')
    customSearch_page_ID <-
      extract_first_match("\\('(.*)')",response_data$responses[[1]]$data)
  }
  customSearch_page_response <- send_request(paste0('sid=', session_id, '&pid=', customSearch_page_ID))
  customSearch_page_text = httr::content(customSearch_page_response, as = 'text')

  query_cid <- extract_first_match("div class=\"sys-datatable\" id=\"(.*)_table", customSearch_page_text)

  #selectAll_ceid <- extract_first_match("onclick=\"sys.getC\\(\"(.{5,20})\"\\).selectAll", customSearch_page_text)

    # Search
  if (database == 'EpiRSV'){
    SearchComponent <- 'RSVSearchComponent'
  } else if (database == 'EpiPox') {
    SearchComponent <- 'MPoxSearchComponent'
  } else {
    SearchComponent <- 'Corona2020SearchComponent'
  }
  search_cid <- extract_first_match(sprintf("sys.createComponent\\('(.{5,20})','%s'", SearchComponent), customSearch_page_text)

  # Location
  location_ceid <- extract_search_ceid('covv_location', customSearch_page_text)

  # Lineage
  if (database == 'EpiCoV'){
    linage_ceid <- extract_search_ceid('pangolin_lineage', customSearch_page_text)
  } else {
    linage_ceid <- NULL
  }

  # AA substitution/mutation- ", " separated values
  aa_substitution_ceid <- extract_search_ceid('mutation', customSearch_page_text)
  # nucleotide substitution/nuc mutation, ", " separated values
  nucl_mutation_ceid <- extract_search_ceid('nuc_mutation', customSearch_page_text)
  # Virus Name
  virus_name_ceid <- extract_search_ceid('covv_virus_name', customSearch_page_text)

  # From
  from_ceid <- extract_search_ceid('covv_collection_date_from', customSearch_page_text)

  # from submission
  from_sub_ceid <- extract_search_ceid('covv_subm_date_from', customSearch_page_text)

  # To
  to_ceid <- extract_search_ceid('covv_collection_date_to', customSearch_page_text)

  # To submission
  to_sub_ceid <- extract_search_ceid('covv_subm_date_to', customSearch_page_text)

  # low_coverage_excl
  if (database != 'EpiPox'){
    low_coverage_excl_ceid <- extract_search_ceid('low_quality', customSearch_page_text)
  } else {
    low_coverage_excl_ceid <- NULL
  }
  if (database == 'EpiCoV'){
    # full text search
    text_ceid <-
      extract_search_ceid("fts", customSearch_page_text)
    # Highq
    highq_ceid <-
      extract_search_ceid("highq", customSearch_page_text)
    # Complete
    complete_ceid <-
      extract_search_ceid("complete", customSearch_page_text)
    # collection date complete
    collection_date_complete_ceid <-
      extract_search_ceid('coldc', customSearch_page_text)
    # variants
    variant_ceid <-
      extract_search_ceid('variants', customSearch_page_text)
    # quality not used by EpiCov
    quality_ceid <- NULL
  } else {
    text_ceid <- NULL
    variant_ceid <- NULL
    complete_ceid <- NULL
    highq_ceid <- NULL
    # Complete and Highq
    quality_ceid <-
      extract_search_ceid("quality'", customSearch_page_text) # avoid match with quality2
    # collection date complete
    collection_date_complete_ceid <-
      extract_search_ceid('quality2', customSearch_page_text)
    # AA substitution
    aa_substitution_ceid <-
      extract_search_ceid('mutation', customSearch_page_text)
    # nucleotide mutation
    nucl_mutation_ceid <-
      extract_search_ceid('nuc_mutation', customSearch_page_text)
  }

  # send selection command
  selection_pid_wid <- get_selection_panel(session_id, WID, customSearch_page_ID, query_cid)

  #load panel
  # REFACTOR
  selection_page <-
    send_request(paste0('sid=', session_id, '&pid=', selection_pid_wid$pid))

  selection_page_text = httr::content(selection_page, as = 'text')

  selection_panel_cid <- extract_first_match("onselect=\"sys.getC\\('(.{5,20})')", selection_page_text)
  selection_ceid <- extract_first_match("getFI\\('(.{5,20})').onSelect", selection_page_text)

  send_back_cmd(session_id, selection_pid_wid$wid, selection_pid_wid$pid, selection_panel_cid)

  # back
  credentials <-
    list(
      database = database,
      pid = customSearch_page_ID,
      sid = session_id,
      wid = WID,
      query_cid = query_cid,
      selection_panel_cid = selection_panel_cid,
      selection_ceid = selection_ceid,
      download_panel_cid = query_cid,
      text_ceid = text_ceid,
      location_ceid = location_ceid,
      search_cid = search_cid,
      aa_substitution_ceid = aa_substitution_ceid,
      nucl_mutation_ceid = nucl_mutation_ceid,
      linage_ceid = linage_ceid,
      virus_name_ceid = virus_name_ceid,
      from_ceid = from_ceid,
      from_sub_ceid = from_sub_ceid,
      to_ceid = to_ceid,
      to_sub_ceid = to_sub_ceid,
      low_coverage_excl_ceid = low_coverage_excl_ceid,
      highq_ceid = highq_ceid,
      complete_ceid = complete_ceid,
      collection_date_complete_ceid = collection_date_complete_ceid,
      quality_ceid = quality_ceid,
      variant_ceid = variant_ceid
    )

  return(credentials)
}
