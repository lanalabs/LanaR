source('R/aggregateUtils.R')
#source(here::here('/Users/goegges/Development/lanar'))

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
requestDataBuilder <- function(groupingType, dateType, attribute, agg, valuesFrom, maxAmountAttributes, valueSorting, sortingOrder, miningRequestData){
  requestData <- list(
    metric = metricNumBuilder(attribute, agg),
    valuesFrom = list(
      type = valuesFrom
    ),
    options =  optionsBuilder(maxAmountAttributes, valueSorting, sortingOrder),
    miningRequest = miningRequestData
  )
  if (groupingType != "null") {
    requestData[["grouping"]] <- groupingBuilder(groupingType, dateType, attribute)
  }
  return(requestData)
}

contentBuilder <- function(lanaUrl, applicationKey, logId, dateType, attribute, agg, valuesFrom,
                           groupingType, maxAmountAttributes, valueSorting, sortingOrder, traceFilter){

  miningRequestData <- miningRequestBuilder(logId = logId, traceFilter = traceFilter)
  requestData <- requestDataBuilder(groupingType, dateType, attribute, agg, valuesFrom, maxAmountAttributes,
                                    valueSorting, sortingOrder, miningRequestData)
  content <- aggregateApiCall(lanaUrl, requestData, applicationKey)
  return(content)
}

#' aggregated numeric attribute per year
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#' @param attribute
#' @param agg
#' @param valueSorting
#' @param sortingOrder
#'
#' @return
#' @export
#'
#' @examples
getNumericYear <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, attribute, agg,
                                dateType = "startDate", valuesFrom = "allCases", maxAmountAttributes = 10,
                                valueSorting = "CaseCount", sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "byYear"
  freqNumericYear <- contentBuilder(lanaUrl, applicationKey, logId, dateType, attribute, agg, valuesFrom, groupingType,
                                  maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
  return(freqNumericYear)

}

#' aggregated numeric attribute per quarter
#'
#' @param lanaUrl
#' @param applicationKey
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilter
#' @param attribute
#' @param agg mean, median, sum, min, max
#' @param valueSorting
#' @param sortingOrder
#'
#' @return
#' @export
#'
#' @examples
getNumericQuarter <- function(lanaUrl = "cloud-backend.lanalabs.com", applicationKey, logId, attribute, agg,
                           dateType = "startDate", valuesFrom = "allCases", maxAmountAttributes = 10,
                           valueSorting = "CaseCount", sortingOrder = "Descending", traceFilter = list()){

  groupingType <- "byQuarter"
  freqNumericQuarter <- contentBuilder(lanaUrl, applicationKey, logId, dateType, attribute, agg, valuesFrom, groupingType,
                                    maxAmountAttributes, valueSorting, sortingOrder, traceFilter)
  return(freqNumericQuarter)

}

