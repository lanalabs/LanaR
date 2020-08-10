library(stringr)

optionsBuilder <- function(maxAmountAttributes, valueSorting, sortingOrder){
  options = list(
    maxAmountAttributes = maxAmountAttributes,
    valueSorting =  valueSorting,
    sortingOrder =  sortingOrder
  )
  return(options)
}

metricNumBuilder <- function(attribute, agg){
  metric = list(
    aggregationFunction = agg,
    attribute = attribute,
    type = "attribute"
  )
  return(metric)
}

metricFreqBuilder <- function(){
  metric = list(
    type = "frequency"
  )
  return(metric)
}


#' grouping parameter for time grouping
#'
#' @param dateType
#' @param groupingType
#'
#' @return
#' @export
#'
#' @examples
groupingTime <- function(dateType, groupingType){
  grouping = list(
    dateType = dateType,
    timeZone = "Europe/Berlin",
    type = groupingType
  )
  return(grouping)
}

#' grouping parameter for attribute grouping
#'
#' @param attribute
#' @param groupingType
#'
#' @return
#' @export
#'
#' @examples
groupingAttribute <- function(attribute, groupingType){
  grouping = list(
    attribute = attribute,
    type = groupingType
  )
  return(grouping)
}

#' grouping builder
#'
#' @param groupingType
#' @param dateType
#' @param attribute
#'
#' @return
#' @export
#'
#' @examples
groupingBuilder <- function(groupingType, dateType, attribute){
  listTime <- c("byYear", "byQuarter", "byMonth", "byDayOfYear", "byDayOfWeek", "byHourOfDay")
  if (groupingType == "byAttribute") {
    grouping = groupingAttribute(attribute, grouping)} else if (groupingType %in% listTime) {
      grouping = groupingTime(dateType, groupingType)
    }
  return(grouping)
}

#' mining request builder
#'
#' @param logId
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
miningRequestBuilder <- function(logId, traceFilter){
  miningRequestData <- list(
    includeHeader = TRUE,
    includeLogId = TRUE,
    logId = logId,
    edgeThreshold = 1,
    traceFilterSequence = traceFilter,
    runConformance = FALSE
  )
  return(miningRequestData)
}

#' post aggregate api call to lana
#'
#' @param lanaUrl
#' @param requestData
#' @param applicationKey
#'
#' @return
#' @export
#'
#' @examples
aggregateApiCall <- function(lanaUrl, requestData, applicationKey){
  response <- httr::POST(
    paste0("https://", lanaUrl, "/api/v2/aggregate-data"),
    body = list(request = jsonlite::toJSON(requestData, auto_unbox = TRUE)),
    encode = "multipart",
    httr::add_headers( c(
      Authorization = applicationKey
    ))
  )
  c <- httr::content(response)
  groupingLabel <- c[["groupingAxisLabel"]]
  groupingLabel <- str_replace_all(groupingLabel, " ", "_")
  metrikLabel <- c[["metricAxisLabel"]]
  metrikLabel <- str_replace_all(metrikLabel, " ", "_")
  content <- c$chartValues
  content <- do.call(rbind.data.frame, content)
  content$X.type <- NULL
  names(content)[names(content) == "xAxis"] <- groupingLabel
  names(content)[names(content) == "yAxis"] <- metrikLabel
  return(content)
}
