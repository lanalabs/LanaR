#' mining request build
#'
#' @param log_id
#' @param tf_type
#' @param tf_min
#' @param tf_max
#'
#' @return
#' @export
#'
#' @examples
mining_request <- function(log_id, tf_type, tf_min, tf_max){
miningRequestData <- list(
  includeHeader = TRUE,
  includeLogId = TRUE,
  logId = log_id,
  edgeThreshold = 1,
  traceFilterSequence = trace_filter(tf_type, tf_min, tf_max),
  runConformance = FALSE
)
return(miningRequestData)
}

#' trace filter sequence build
#'
#' @param type
#' @param min
#' @param max
#'
#' @return
#' @export
#'
#' @examples

trace_filter <- function(type, min, max){
  if (is.null(type) | is.null(min) | is.null(max)) {
    traceFilterSequence = list()
  } else {
    traceFilterSequence = list(list(
      max = max,
      min = min,
      type = type))
  }
  return(traceFilterSequence)
}


#' request data build
#'
#' @param date_type
#' @param grouping_date
#' @param values_from
#' @param maxAmountAttributes
#' @param miningRequestData
#'
#' @return
#' @export
#'
#' @examples
request_data <- function(date_type, grouping_date, values_from, maxAmountAttributes, miningRequestData){
requestData <- list(
  grouping = list(
    dateType = date_type,
    timeZone = "Europe/Berlin",
    type = grouping_date
  ),
  metric = list(
    type = "frequency"
  ),
  valuesFrom = list(
    type = values_from
  ),
  options =  list(
    maxAmountAttributes = maxAmountAttributes
  ),
  miningRequest = miningRequestData
)
return(requestData)
}

#' post aggregate api call to lana
#'
#' @param lana_url
#' @param requestData
#' @param application_key
#'
#' @return
#' @export
#'
#' @examples
aggregate_api_call <- function(lana_url, requestData, application_key){
response <- httr::POST(
  paste0("https://", lana_url, "/api/v2/aggregate-data"),
  body = list(request = jsonlite::toJSON(requestData, auto_unbox = TRUE)),
  encode = "multipart",
  httr::add_headers( c(
    Authorization = application_key
  ))
)
content <- httr::content(response)
return(content)
}

#' aggregated frequencies API call
#'
#' @param lana_url default = "cloud-backend.lanalabs.com"
#' @param application_key API Key in Lana (Export Filter Settings)
#' @param log_id id of uploaded log in lana
#' @param date_type "startDate" or "endDate"
#' @param values_from "allCases" or "allEvents"
#' @param grouping_date "byYear", "byQuarter", "byMonth", "byDayOfYear", "byDayOfWeek" or "byHourOfDay"
#' @param maxAmountAttributes default = 10
#' @param tf_type trace filter type
#' @param tf_min trace filter minimum
#' @param tf_max trace filter maximum
#'
#' @return frequency counts
#'
#' @examples
get_freq <- function(lana_url = "cloud-backend.lanalabs.com", application_key, log_id, date_type, values_from,
                     grouping_date, maxAmountAttributes = 10, tf_type = NULL, tf_min = NULL, tf_max = NULL){

  miningRequestData <- mining_request(log_id, tf_type, tf_min, tf_max)
  requestData <- request_data(date_type, grouping_date, values_from, maxAmountAttributes, miningRequestData)
  content <- aggregate_api_call(lana_url, requestData, application_key)
  return(content$chartValues)
}

