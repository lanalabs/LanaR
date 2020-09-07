#' options builder
#'
#' @param maxAmountAttributes
#' @param valueSorting
#' @param sortingOrder
#'
#' @return
#' @export
#'
#' @examples
optionsBuilder <- function(maxAmountAttributes, valueSorting, sortingOrder){
  options = list(
    maxAmountAttributes = maxAmountAttributes,
    valueSorting =  valueSorting,
    sortingOrder =  sortingOrder
  )
  return(options)
}

#' metric builder for numeric attributes
#'
#' @param attribute
#' @param agg
#'
#' @return
#' @export
#'
#' @examples
metricNumBuilder <- function(attribute, agg){
  metric = list(
    aggregationFunction = agg,
    attribute = attribute,
    type = "attribute"
  )
  return(metric)
}

#' metric builder for frequencies
#'
#' @return
#' @export
#'
#' @examples
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
#' @param traceFilterSequence
#'
#' @return
#' @export
#'
#' @examples
miningRequestBuilder <- function(logId, traceFilterSequence){
  miningRequestData <- list(
    includeHeader = TRUE,
    includeLogId = TRUE,
    logId = logId,
    edgeThreshold = 1,
    traceFilterSequence = traceFilterSequence,
    runConformance = FALSE
  )
  return(miningRequestData)
}

#' post aggregate api call to lana
#'
#' @param lanaUrl
#' @param requestData
#' @param lanaToken
#'
#' @return
#' @export
#'
#' @examples
aggregateApiCall <- function(lanaUrl, requestData, lanaToken){
  response <- httr::POST(
    paste0("https://", lanaUrl, "/api/v2/aggregate-data"),
    body = list(request = jsonlite::toJSON(requestData, auto_unbox = TRUE)),
    encode = "multipart",
    httr::add_headers( c(
      Authorization = lanaToken
    ))
  )
  c <- httr::content(response)
  groupingLabel <- c[["groupingAxisLabel"]]
  groupingLabel <- stringr::str_replace_all(groupingLabel, " ", "_")
  metricLabel <- c[["metricAxisLabel"]]
  metricLabel <- stringr::str_replace_all(metricLabel, " ", "_")
  content <- c$chartValues
  content <- do.call(rbind.data.frame, content)
  content$X.type <- NULL
  names(content)[names(content) == "xAxis"] <- groupingLabel
  names(content)[names(content) == "yAxis"] <- metricLabel
  return(content)
}

