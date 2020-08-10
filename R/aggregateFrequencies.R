#installed.packages("stringr")
library(stringr)

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

#' requestDataBuilder
#'
#' @param groupingType
#' @param dateType
#' @param attribute
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param miningRequestData
#'
#' @return
#' @export
#'
#' @examples
requestDataBuilder <- function(groupingType, dateType, attribute, valuesFrom, maxAmountAttributes, valueSorting, sortingOrder, miningRequestData){
  requestData <- list(
    metric = list(
      type = "frequency"
    ),
    valuesFrom = list(
      type = valuesFrom
    ),
    options =  list(
      maxAmountAttributes = maxAmountAttributes,
      valueSorting =  valueSorting,
      sortingOrder =  sortingOrder
    ),
    miningRequest = miningRequestData
  )
  if (groupingType != "null") {
    requestData[["grouping"]] <- groupingBuilder(groupingType, dateType, attribute)
  }
  return(requestData)
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

#' aggregated frequencies API call
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param attribute
#' @param valuesFrom
#' @param groupingType
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
freqBuilder <- function(lanaUrl, applicationKey, logId, dateType, attribute, valuesFrom,
                        groupingType, maxAmountAttributes, valueSorting, sortingOrder, traceFilter){

  miningRequestData <- miningRequestBuilder(logId, traceFilter)
  requestData <- requestDataBuilder(groupingType, dateType, attribute, valuesFrom, maxAmountAttributes, valueSorting, sortingOrder, miningRequestData)
  content <- aggregateApiCall(lanaUrl, requestData, applicationKey)
  return(content)
}

#' aggregated frequencies without grouping
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
getFreq <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, dateType = "null",
                    valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                    sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "null"
  freq <- freqBuilder(lanaUrl, applicationKey, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                      maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
  return(freq)

}

#' aggregated frequencies per year
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
getFreqYear <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, dateType = "startDate",
                        valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                        sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "byYear"
  freqYear <- freqBuilder(lanaUrl, applicationKey, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                          maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
  return(freqYear)

}

#' aggregated frequencies per quarter
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
getFreqQuarter <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, dateType = "startDate",
                           valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                           sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "byQuarter"
  freqQuarter <- freqBuilder(lanaUrl, applicationKey, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                             maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
  return(freqQuarter)

}

#' aggregated frequencies per month
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
getFreqMonth <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, dateType = "startDate",
                         valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                         sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "byMonth"
  freqMonth <- freqBuilder(lanaUrl, applicationKey, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                           maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
  return(freqMonth)

}

#' aggregated frequencies per day of year
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
getFreqDayofYear <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, dateType = "startDate",
                             valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                             sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "byDayOfYear"
  freqDayofYear <- freqBuilder(lanaUrl, applicationKey, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                               maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
  return(freqDayofYear)

}

#' aggregated frequencies per day of week
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
getFreqDayofWeek <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, dateType = "startDate",
                             valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                             sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "byDayOfWeek"
  freqDayofWeek <- freqBuilder(lanaUrl, applicationKey, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                               maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
  return(freqDayofWeek)

}

#' aggregated frequencies per hour of day
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
getFreqHourofDay <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, dateType = "startDate",
                             valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                             sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "byHourOfDay"
  freqHourofDay <- freqBuilder(lanaUrl, applicationKey, logId, dateType, attribute = NULL, valuesFrom, groupingType,
                               maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
  return(freqHourofDay)

}

#' aggregated frequencies per attribute
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param attribute
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#'
#' @return
#' @export
#'
#' @examples
getFreqAttribute <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, attribute,
                             valuesFrom = "allCases", maxAmountAttributes = 10, valueSorting = "CaseCount",
                             sortingOrder = "Descending", traceFilter = list()){
  groupingType <- "byAttribute"
  freqAttribute <- freqBuilder(lanaUrl, applicationKey, logId, dateType = NULL, attribute, valuesFrom, groupingType,
                               maxAmountAttributes, valueSorting, sortingOrder, traceFilter)

  return(freqAttribute)
}

