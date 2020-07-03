#' Make authorisation headers for lana api
#' 
#' @param applicationKey the application key with or without starting 'API-Key'
#' 
#' @export
makeAuthorisationHeader <- function(applicationKey) {
  
  headerFields <- if (grepl('API-Key', applicationKey))  {
    c(
      Authorization = applicationKey
    )
  } else {
    c(
      Authorization = paste('API-Key', applicationKey)
    )
  }
  return(headerFields)
}

#' Programmatically remove filters from a trace filter sequence by type
#' 
#' @param traceFilter the trace filter as character or R list
#' @param removeFilters filter types to remove
#'
#' @export
handleTraceFilterArgument <- function(traceFilter, removeFilters = list()) {
  
  res <- if (typeof( traceFilter) == 'character' ) 
    jsonlite::fromJSON(traceFilter, simplifyVector = FALSE) else
      traceFilter
  
  return(
    rlist::list.filter(res, !(type %in% removeFilters) )
  )
}

#' Make list used for the grouping requests for time dimensions, translates old api time aggregations
#' 
#' @param timeAggregation the time aggregation type
#'
#' @export
buildTimeType <- function(timeAggregation) {
  
  type <- if (timeAggregation == "byMonth") {
    "byMonth"
  } else if (timeAggregation == "dayOfWeek") {
    "byDayOfWeek"
  } else if (timeAggregation == "byHour") {
    "byHourOfDay"
  } else {
    timeAggregation
  }
  return(type)
}

#' Create grouping for request, separate handling for attributes and time dimensions
#' 
#' @param xDimension x dimension that is used for grouping for the aggregation 
#'
#' @export
buildRequestGrouping <- function(xDimension) {
  
  grouping <- if (grepl("^byAttribute=", xDimension)) {
    list(
      attribute = gsub("^byAttribute=", "", xDimension),
      type = "byAttribute"
    )
  } else if (grepl("^byTime=", xDimension)) {
    list(
      type = buildTimeType(gsub("^byTime=", "", xDimension)),
      dateType = "startDate",
      timeZone = "Europe/Berlin"
    )
  } else {
    list(
      attribute = xDimension,
      type = "byAttribute"
    )
  }
  return(grouping)
}

#' Create metric list for request, old api metrics are translated to new type and aggregationFunction
#' 
#' @param yDimension y dimension that is used for the metric for the aggregation 
#'
#' @export
buildRequestMetric <- function(yDimension) {
  
  metric <- if (grepl("^byAttribute=", yDimension)) {
    list(
      attribute = gsub("^byAttribute=", "", yDimension),
      type = "attribute",
      aggregationFunction = "sum"
    )
  } else if (yDimension == "frequency") {
    list(
      type = "frequency"
    )
  } else if (yDimension == "avgDuration") {
    list(
      type = "duration",
      aggregationFunction = "mean"
    )
  } else if (yDimension == "medianDuration") {
    list(
      type = "duration",
      aggregationFunction = "median"
    )
  } else if (yDimension == "totalDuration") {
    list(
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
  headerFields <- makeAuthorisationHeader(lanaToken)
  
  miningRequestData <- list(
    activityExclusionFilter = handleTraceFilterArgument(activityExclusionFilter),
    includeHeader = TRUE,
    includeLogId = TRUE,
    logId = logId,
    edgeThreshold = 1,
    traceFilterSequence = handleTraceFilterArgument(traceFilterSequence),
    runConformance = FALSE,
    sort = "",
    limit = limit,
    page = page
  )
  
  requestData <- list(
    metric = buildRequestMetric(yDimension),
    valuesFrom = list(
      type = "allCases"
    ),
    options =  list(
      maxAmountAttributes = maxValueAmount,
      sortingOrder = "descending"
    ),
    miningRequest = miningRequestData,
    type = type
  )

  if (xDimension != "noAggregation") {
    requestData[["grouping"]] <- buildRequestGrouping(xDimension)
  }  
    
  if (zDimension != "null") {
    requestData[["secondaryGrouping"]] <- buildRequestGrouping(zDimension)
  }
  
  r <- httr::POST(
    paste0("https://", lanaUrl, "/api/v2/aggregate-data"),
    body = list(request = jsonlite::toJSON(requestData, auto_unbox = TRUE)),
    encode = "multipart",
    httr::add_headers(headerFields)
  )
  
  checkHttpErrors(r)
  
  content <- jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8"))
  
  chartValues <- content$chartValues %>%
    select(-`$type`)
  
  if(zDimension != "null"){
    chartValues %<>% 
      unnest(values, names_repair = "unique")
  }
  
  names(chartValues)[names(chartValues) == "xAxis"] <- gsub(".*=", "", xDimension)
  names(chartValues)[names(chartValues) == "yAxis"] <- gsub(".*=", "", yDimension)
  names(chartValues)[names(chartValues) == "zAxis"] <- gsub(".*=", "", zDimension)
  
  chartValues <- chartValues[, !names(chartValues) %in% c("$type","$type...1","$type...2")]
  
  return(chartValues)
  
}
