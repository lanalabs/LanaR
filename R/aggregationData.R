#' Make authorisation headers for lana api
#' 
#' @param application_key the application key with or without starting 'API-Key'
#' 
#' @export
make_authorisation_header <- function(application_key) {
  
  header_fields <- if ( grepl('API-Key', application_key) )  {
    c(
      Authorization = application_key
    )
  } else {
    c(
      Authorization = paste('API-Key', application_key)
    )
  }
  return(header_fields)
}

#' Programmatically remove filters from a trace filter sequence by type
#' 
#' @param trace_filter the trace filter as character or R list
#' @param remove_filters filter types to remove
#'
#' @export
handle_trace_filter_argument <- function(trace_filter, remove_filters = list()) {
  
  res <- if (typeof( trace_filter) == 'character' ) 
    jsonlite::fromJSON(trace_filter, simplifyVector = FALSE) else
      trace_filter
  
  return(
    rlist::list.filter(res, !(type %in% remove_filters) )
  )
}

build_time_type <- function(time_aggregation) {
  if (time_aggregation == "byMonth") {
    type <- "byMonth"
  } else if (time_aggregation == "dayOfWeek") {
    type <- "byDayOfWeek"
  } else if (time_aggregation == "byHour") {
    type <- "byHourOfDay"
  } else {
    type <- time_aggregation
  }
  return(type)
}

build_request_grouping <- function(x_dimension) {
  if (grepl("^byAttribute=", x_dimension)) {
    grouping <- list(
      attribute = gsub("^byAttribute=","",x_dimension),
      type = "byAttribute"
    )
  } else if (grepl("^byTime=", x_dimension)) {
    grouping <- list(
      type = build_time_type(gsub("^byTime=","",x_dimension)),
      dateType = "startDate",
      timeZone = "Europe/Berlin"
    )
  }
  return(grouping)
}

build_request_metric <- function(y_dimension) {
  if (grepl("^byAttribute=", y_dimension)) {
    metric <- list(
      attribute = gsub("^byAttribute=","",y_dimension),
      type = "attribute",
      aggregationFunction = "sum"
    )
  } else if (y_dimension == "frequency") {
    metric <- list(
      type = "frequency"
    )
  } else if (y_dimension == "avgDuration") {
    metric <- list(
      type = "duration",
      aggregationFunction = "mean"
    )
  } else if (y_dimension == "medianDuration") {
    metric <- list(
      type = "duration",
      aggregationFunction = "median"
    )
  } else if (y_dimension == "totalDuration") {
    metric <- list(
      type = "duration",
      aggregationFunction = "total"
    )
  }
  return(metric)
}

#' @title Aggregate
#' Aggregate data once uploaded to Lana
#' Aggregations can be calculated by time (month, day of week, hour) or by attribute regarding the frequency, average duration, median duration and total duration. Also the aggregated data can be grouped by attributes.
#' @description Gets the aggregation of the requested data with the specified parameters . \cr See https://api.lana-labs.com/#/routes/getAggregatedData
#' @param lanaUrl URL of the instance that LANA is running on
#' @param lanaToken Lana API token read from LANA
#' @param logId Log ID being read from LANA
#' @param xDimension Define the x dimension for the aggregation
#' @param yDimension Define the y dimension for the aggregation
#' @param zDimension Define the z dimension for the aggregation (optional, default = "null")
#' @param aggrLevel Define the aggregation level (optional, default = "traces")
#' @param followers Define followers (optional, default = "null")
#' @param type (optional, default = "null")
#' @param cache (optional, default = "{}")
#' @param maxValueAmount Define the amount of values you wanto tdisplay before the rest are aggregated into "other" (optional, default = 5)
#' @param activityExclusionFilter Hide activities in aggregation (optional, default = "[]")
#' @param traceFilterSequence Integrate any kind of filter from lana into your aggregation (optional, default = "[]")
#' @param limit (optional, default = 10)
#' @param page (optional, default = 1)
#' @return Aggregated data
#' @examples
#' aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byMonth", yDimension = "frequency")
#' aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=dayOfWeek", yDimension = "avgDuration")
#' aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byHour", yDimension = "medianDuration")
#' aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byMonth", yDimension = "totalDuration")
#' aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byMonth", yDimension = "frequency", zDimension = "byAttribute=Est. Cost")

aggregate <- function(lanaUrl, lanaToken, logId, xDimension, yDimension, zDimension = "null", 
                       aggrLevel = "traces", followers = "null",
                       type = "aggregation", cache = "{}", maxValueAmount = 5, 
                       activityExclusionFilter = "[]", traceFilterSequence="[]", 
                       limit = 10, page = 1) {
  header_fields <- make_authorisation_header(lanaToken)
  
  mining_request_data <- list(
    activityExclusionFilter = handle_trace_filter_argument(activityExclusionFilter),
    includeHeader = TRUE,
    includeLogId = TRUE,
    logId = logId,
    edgeThreshold = 1,
    traceFilterSequence = handle_trace_filter_argument(traceFilterSequence),
    runConformance = FALSE,
    options =  list(
      maxAmountAttributes = maxValueAmount
    ),
    sort = "start",
    limit = limit,
    page = page
  )
  
  request_data <- list(
    type = type,
    metric = build_request_metric(yDimension),
    grouping = build_request_grouping(xDimension),
    if (zDimension != "null") (secondaryGrouping = build_request_grouping(zDimension)),
    valuesFrom = list(
      type = "allCases"
    ),
    miningRequest = mining_request_data
  )
  
  r <- httr::POST(
    paste0("https://", lanaUrl, "/api/v2/aggregate-data"),
    body = list(request = jsonlite::toJSON(request_data, auto_unbox = TRUE)),
    encode = "multipart",
    httr::add_headers(header_fields)
  )
  
  content <- jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8"))
  
  chart_values <- content$chartValues
  
  if(zDimension != "null"){
    chart_values %<>% 
      unnest(values, names_repair = "unique")
  }
  
  names(chart_values)[names(chart_values) == "xAxis"] <-  gsub(".*=", "", xDimension)
  names(chart_values)[names(chart_values) == "yAxis"] <- gsub(".*=", "", yDimension)
  names(chart_values)[names(chart_values) == "zAxis"] <- gsub(".*=", "", zDimension)
  
  chart_values <- chart_values[, !names(chart_values) %in% c("$type","$type...1","$type...2")]
  
  return(chart_values)
}
