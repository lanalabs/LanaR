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
  
  res <- if (typeof(traceFilter) == 'character' ) 
    jsonlite::fromJSON(traceFilter, simplifyVector = FALSE) else
      traceFilter
  
  return(
    rlist::list.filter(res, !(type %in% removeFilters) )
  )
}

#' Translates legacy aggregation levels 
#' 
#' @param aggrLevel the aggregation level
#'
#' @export
buildValuesFrom <- function(aggrLevel) {
  
  valuesFrom <- if (aggrLevel == "traces") {
    "allCases"
  } else if (aggrLevel == "events") {
    "allEvents"
  } else {
    aggrLevel
  }
  return(valuesFrom)
}

#' Translates legacy time aggregations
#' 
#' @param timeAggregation the time aggregation type
#'
#' @export
buildTimeType <- function(timeAggregation) {
  
  type <- if (timeAggregation == "dayOfWeek") {
    "byDayOfWeek"
  } else if (timeAggregation == "dayOfYear") {
    "byDayOfYear"
  } else if (timeAggregation == "byHour") {
    "byHourOfDay"
  } else {
    timeAggregation
  }
  return(type)
}

#' Create grouping for request, separate handling for attributes and time dimensions, if no prefix "byAttribute" or "byTime" is given, xDimension is interpreted as attribute
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

#' Create metric list for request, legacy api metrics are translated to new type and aggregationFunction
#' 
#' @param yDimension y dimension that is used for the metric for the aggregation
#' @param aggregationFunction aggregation function ("min" / "max" / "mean" / "median" / "sum") 
#'
#' @export
buildRequestMetric <- function(yDimension, aggregationFunction) {
  
  metric <- if (grepl("^byAttribute=", yDimension)) {
    list(
      attribute = gsub("^byAttribute=", "", yDimension),
      type = "attribute",
      aggregationFunction = aggregationFunction
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
      aggregationFunction = "sum"
    )
  } else if (yDimension == "duration") {
    list(
      type = "duration",
      aggregationFunction = aggregationFunction
    )
  } else {
    list(
      attribute = yDimension,
      type = "attribute",
      aggregationFunction = aggregationFunction
    )    
  }
  return(metric)
}

#' @title Aggregate
#' Aggregate data once uploaded to Lana
#' Aggregations can be calculated by time (month, day of week, hour) or by attribute regarding the frequency, average duration, median duration and total duration. Also the aggregated data can be grouped by attributes.
#' @description Gets the aggregation of the requested data with the specified parameters . \cr See https://cloud-backend.lanalabs.com/swagger#/Aggregation/post_api_v2_aggregate_data
#' @param lanaUrl URL of the instance that LANA is running on
#' @param lanaToken Lana API token read from LANA
#' @param logId Log ID being read from LANA
#' @param xDimension Define the grouping for the aggregation (optional, default = "noAggregation", attributeName / "frequency / "duration")
#' @param yDimension Define the metric for the aggregation (attributeName / "byTime=byMonth" / byTime=byDayOfWeek" / "byTime=byHourOfDay" / "byTime=byQuarter" / byTime=byYear" / byTime=byDayOfYear")
#' @param zDimension Define the secondary grouping for the aggregation (optional, default = "null", attributeName / "frequency / "duration")
#' @param aggrLevel Define the aggregation level (optional, default = "traces", "traces" == "allCases" / "events" == "allEvents")
#' @param followers Define followers (legacy leftover, not used)
#' @param type (optional, default = "null")
#' @param aggregationFunction Define the aggregation that is used for numeric and time metrics ("min" / "max" / "mean" / "median" / "sum")
#' @param cache (optional, default = "{}")
#' @param maxValueAmount Define the amount of values that are displayed before the rest are aggregated into "other" (optional, default = 5)
#' @param activityExclusionFilter Hide activities in aggregation (optional, default = "[]")
#' @param traceFilterSequence Integrate any kind of filter from lana into your aggregation (optional, default = "[]")
#' @param limit (optional, default = 10)
#' @param page (optional, default = 1)
#' @param valueSorting Sorting option of the aggregated values (optional, default = "caseCount", "caseCount" / "numericValue" / "alphabetic")
#' @param sortingOrder Sorting order of the aggregated values (optional, default = "descending", "ascending" / "descending")
#' @return A data frame with the aggregated data

aggregate <- function(lanaUrl, lanaToken, logId, xDimension = "noAggregation", yDimension, zDimension = "null", 
                       aggrLevel = "traces", followers = "null",
                       type = "aggregation", aggregationFunction = "sum", cache = "{}", maxValueAmount = 5, 
                       activityExclusionFilter = "[]", traceFilterSequence="[]", 
                       limit = 10, page = 1, valueSorting = "caseCount",
                      sortingOrder = "descending") {
  headerFields <- makeAuthorisationHeader(lanaToken)
  
  miningRequestData <- list(
    activityExclusionFilter = handleTraceFilterArgument(activityExclusionFilter),
    includeHeader = TRUE,
    includeLogId = TRUE,
    logId = logId,
    edgeThreshold = 1,
    traceFilterSequence = handleTraceFilterArgument(traceFilterSequence),
    runConformance = FALSE,
    limit = limit,
    page = page
  )
  
  requestData <- list(
    metric = buildRequestMetric(yDimension, aggregationFunction),
    valuesFrom = list(
      type = buildValuesFrom(aggrLevel)
    ),
    options =  list(
      maxAmountAttributes = maxValueAmount,
      valueSorting = valueSorting,
      sortingOrder = sortingOrder
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
  
  chartValues <- content$chartValues
  
  if(zDimension != "null"){
    chartValues <- chartValues %>%
      tidyr::unnest(values, names_repair = "unique")
  }
  
  names(chartValues)[names(chartValues) == "xAxis"] <- gsub(".*=", "", xDimension)
  names(chartValues)[names(chartValues) == "yAxis"] <- gsub(".*=", "", yDimension)
  names(chartValues)[names(chartValues) == "zAxis"] <- gsub(".*=", "", zDimension)
  
  chartValues <- chartValues[, !names(chartValues) %in% c("$type","$type...1","$type...2")]
  
  return(chartValues)
  
}