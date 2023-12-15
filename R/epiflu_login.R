get_epiflu_credentials <- function(session_id,
                                   wid,
                                   login_page_ID,
                                   frontend_page_ID,
                                   frontend_page_text) {


  EpiFlu_CID <-
    extract_first_match("sys.call\\('(.{5,20})','Go'", frontend_page_text)

  epiflu_page_session_id <- extract_first_match("'_sid':'(.{30,100})','back'", frontend_page_text)
  epiflu_back <- extract_first_match("'back':'(.{140,150})',", frontend_page_text)

  # --- register the session id on platform.epicov.org ---

  goto_EpiFlu_page_command <- createCommand(
    wid = wid,
    pid = frontend_page_ID,
    cid = EpiFlu_CID,
    cmd = 'GoCloud',
    params = list(
      cms_page = 'epi3',
      `_sid` = epiflu_page_session_id, # register the session id on platform.epicov.org
      back = epiflu_back,
      mode = 'page',
      `_url` = 'https://platform.epicov.org/epi3/cfrontend'
    )
  )


  queue <- list(queue = list(goto_EpiFlu_page_command))

  data <-
    formatDataForRequest(session_id, wid, frontend_page_ID, queue, timestamp())

  response <- send_request(data=data, method = 'POST', url="https://www.epicov.org/epi3/frontend")
  response_data <- parseResponse(response) # ensure sid rego

  # --- go to platform.epicov.org ---
  FLU_page <- send_request(
    data = paste0(
      "cms_page=epi3",
      "&sid=",
      epiflu_page_session_id,
      "&mode=page"
    ),
    url = "https://platform.epicov.org/epi3/cfrontend",
    method = 'POST'
  )

  FLU_page_text = httr::content(FLU_page, as = 'text')
  FLU_page_ID <- paste0("pid_", extract_first_match("pid_(.{5,15})\"", FLU_page_text))

  FLU_actionbar_component_ID <-
    extract_first_match("sys-actionbar-action.*\" onclick=\"sys.getC\\('([^']*)",
                        FLU_page_text)
  goto_browse_page <- function() {
    browse_ceid <-
      extract_first_match(".onclick\\('(.{5,20})', 'Browse',", FLU_page_text)

    goto_flu_browse_page_command <- createCommand(
      wid = wid,
      pid = FLU_page_ID,
      cid = FLU_actionbar_component_ID,
      cmd = 'Cmd',
      params = list(ceid = browse_ceid)
    )

    queue <- list(queue = list(goto_flu_browse_page_command))

    data <-
      formatDataForRequest(epiflu_page_session_id, wid, FLU_page_ID, queue, timestamp())

    response <- send_request(data, url="https://platform.epicov.org/epi3/frontend")
    response_data <- parseResponse(response)
    return(response_data)
  }

  response_data <- goto_browse_page()

  customSearch_page_ID <-
    extract_first_match("\\('(.*)')", response_data$responses[[1]]$data)

  search_page <-
    send_request(paste0('sid=', epiflu_page_session_id, '&pid=', customSearch_page_ID), url="https://platform.epicov.org/epi3/frontend")
  search_page_text = httr::content(search_page, as = 'text')

  search_cid <- extract_first_match("sys.createComponent\\('(.{5,30})','IsolateBrowseFormComponent'", search_page_text)
  submit_cid <- extract_first_match("sys.createComponent\\('(.{5,30})','IsolateSearchButtonsComponent'", search_page_text)

  text_ceid <- extract_search_ceid('search_pattern', search_page_text)
  type_ceid <- extract_search_ceid('isl_type', search_page_text)
  subtype_h_ceid <- extract_search_ceid('isl_subtype_h', search_page_text)
  subtype_n_ceid <- extract_search_ceid('isl_subtype_n', search_page_text)
  lineage_ceid <- extract_search_ceid('isl_lineage', search_page_text)
  host_ceid <- extract_search_ceid('isl_host', search_page_text)
  location_ceid <- extract_search_ceid('isl_location', search_page_text)
  clade_ceid <- extract_search_ceid('clades', search_page_text)
  collect_date_from_ceid <- extract_search_ceid('isl_collect_date_from', search_page_text)
  collect_date_to_ceid <- extract_search_ceid('isl_collect_date_to', search_page_text)
  submission_date_from_ceid <- extract_search_ceid('isl_submission_date_from', search_page_text)
  submission_date_to_ceid <- extract_search_ceid('isl_submission_date_to', search_page_text)

  credentials <-
    list(
      database = "EpiFlu",
      url = "https://platform.epicov.org/epi3/frontend",
      pid = customSearch_page_ID,
      sid = epiflu_page_session_id,
      wid = wid,
      submit_cid = submit_cid,
      search_cid = search_cid,
      text_ceid = text_ceid,
      type_ceid = type_ceid,
      subtype_h_ceid = subtype_h_ceid,
      subtype_n_ceid = subtype_n_ceid,
      lineage_ceid = lineage_ceid,
      host_ceid = host_ceid,
      location_ceid = location_ceid,
      clade_ceid = clade_ceid,
      from_ceid = collect_date_from_ceid,
      to_ceid = collect_date_to_ceid,
      from_subm_ceid = submission_date_from_ceid,
      to_subm_ceid = submission_date_to_ceid
    )

  return(credentials)
}
