#' Login to GISAID
#'
#' @param username GISAID username.
#' @param password GISAID password.
#' @return credentials used to query GISAID.
#' @examples
#' username = Sys.getenv("GISAIDR_USERNAME")
#' password = Sys.getenv("GISAIDR_PASSWORD")
#' login(username, password)
login <- function(username, password) {

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

  frontend_page_ID <- extract_first_match("\\('(.*)')",response_data$responses[[1]]$data)

  frontend_page <- send_request(paste0('sid=', session_id, '&pid=', frontend_page_ID))
  frontend_page_text = httr::content(frontend_page, as = 'text')

  actionbar_component_ID <-
    extract_first_match("sys-actionbar-action.*\" onclick=\"sys.getC\\('([^']*)",
                        frontend_page_text)
  # corona2020 page
  goto_corona2020_page_command <- createCommand(
    wid = WID,
    pid = frontend_page_ID,
    cid = actionbar_component_ID,
    cmd = 'Go',
    params = list(link = 'page_corona2020.PartnerDownloadsPage')
  )

  queue <- list(queue = list(goto_corona2020_page_command))

  data <-
    formatDataForRequest(session_id, WID, frontend_page_ID, queue, timestamp())

  response <- send_request(data)
  response_data <- parseResponse(response)

  corona2020_page_ID <-
    strsplit(response_data$responses[[1]]$data, "'")[[1]][4]

  corona2020_page <-
    send_request(paste0('sid=', session_id, '&pid=', corona2020_page_ID))

  corona2020_page_text = httr::content(corona2020_page, as = 'text')

  customSearch_button_component_ID <-
    extract_first_match("onclick=\"sys.call\\('(.{5,20})','GoAugur'", corona2020_page_text)

  # go to custom search
  ev <- createCommand(
    wid = WID,
    pid = corona2020_page_ID,
    cid = customSearch_button_component_ID,
    cmd = 'GoAugur',
    params = setNames(list(), character(0)) #hack for empty {}
  )
  queue <- list(queue = list(ev))
  data <-
    formatDataForRequest(session_id, WID, corona2020_page_ID, queue, timestamp())

  response <- send_request(method="POST", data = data)
  response_data <- parseResponse(response)

  customSearch_page_ID <-
    strsplit(response_data$responses[[3]]$data, "'")[[1]][4]

  # get query CID
  customSearch_page_response <-
    send_request(paste0('sid=', session_id, '&pid=', customSearch_page_ID))
  customSearch_page_text = httr::content(customSearch_page_response, as = 'text')

  query_cid <- extract_first_match("div class=\"sys-datatable\" id=\"(.*)_table", customSearch_page_text)

  # Search
  search_cid <- extract_first_match("sys.createComponent\\('(.{5,20})','Corona2020ToolSearchComponent'", customSearch_page_text)

  # Location
  location_ceid <- extract_search_ceid('covv_location', customSearch_page_text)

  # Lineage
  linage_ceid <- extract_search_ceid('pangolin_lineage', customSearch_page_text)

  # From
  from_ceid <- extract_search_ceid('covv_collection_date_from', customSearch_page_text)

  # from submission
  from_sub_ceid <- extract_search_ceid('covv_subm_date_from', customSearch_page_text)

  # To
  to_ceid <- extract_search_ceid('covv_collection_date_to', customSearch_page_text)

  # To submission
  to_sub_ceid <- extract_search_ceid('covv_subm_date_to', customSearch_page_text)

  # low_coverage_excl
  low_coverage_excl_ceid <- extract_search_ceid('low_quality', customSearch_page_text)

  # Complete and Highq
  quality_ceid <-
    extract_search_ceid("quality'", customSearch_page_text) # avoid match with quality2

  # collection date complete
  collection_date_complete_ceid <-
    extract_search_ceid('quality2', customSearch_page_text)

  # send selection command
  selection_command <- createCommand(
    wid = WID,
    pid = customSearch_page_ID,
    cid = query_cid,
    cmd = 'Selection',
    params = setNames(list(), character(0)) #hack for empty {}
  )
  queue <- list(queue = list(selection_command))

  data <-
    formatDataForRequest(session_id, WID, customSearch_page_ID, queue, timestamp())

  response <-
    send_request(method='POST', data=data)
  response_data <- parseResponse(response)

  # extract PID
  selection_pid <-
    strsplit(response_data$responses[[1]]$data, "'")[[1]][4]

  #load panel
  selection_page <-
    send_request(paste0('sid=', session_id, '&pid=', selection_pid))

  selection_page_text = httr::content(selection_page, as = 'text')

  panel_cid <- extract_first_match("onselect=\"sys.getC\\('(.{5,20})')", selection_page_text)
  selection_ceid <- extract_first_match("getFI\\('(.{5,20})').onSelect", selection_page_text)

  credentials <-
    list(
      pid = customSearch_page_ID,
      sid = session_id,
      wid = WID,
      query_cid = query_cid,
      panel_cid = panel_cid,
      selection_pid = selection_pid,
      selection_ceid = selection_ceid,
      download_cid = query_cid,
      location_ceid = location_ceid,
      search_cid = search_cid,
      linage_ceid = linage_ceid,
      from_ceid = from_ceid,
      from_sub_ceid = from_sub_ceid,
      to_ceid = to_ceid,
      to_sub_ceid = to_sub_ceid,
      low_coverage_excl_ceid = low_coverage_excl_ceid,
      quality_ceid = quality_ceid,
      collection_date_complete_ceid = collection_date_complete_ceid
    )
  if (!all(unlist(sapply(credentials, function(x)
    isTRUE(nchar(x) != 0))))) {
    stop("Login failed")
  }
  return(credentials)
}
