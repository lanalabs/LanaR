#' numRequestDataBuilder
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
numRequestDataBuilder <- function(groupingType, dateType, attribute, agg, valuesFrom, maxAmountAttributes, valueSorting, sortingOrder, miningRequestData){
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

numContentBuilder <- function(lanaUrl, lanaToken, logId, dateType, attribute, agg, valuesFrom,
                           groupingType, maxAmountAttributes, valueSorting, sortingOrder, traceFilterSequence){

  miningRequestData <- miningRequestBuilder(logId = logId, traceFilterSequence = traceFilterSequence)
  requestData <- numRequestDataBuilder(groupingType, dateType, attribute, agg, valuesFrom, maxAmountAttributes,
                                    valueSorting, sortingOrder, miningRequestData)
  content <- aggregateApiCall(lanaUrl, requestData, lanaToken)
  return(content)
}

#' aggregated numeric attribute per year
#'
#' @param lanaUrl
#' @param lanaToken
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilterSequence
#' @param attribute
#' @param agg
#' @param valueSorting
#' @param sortingOrder
#'
#' @return
#' @export
#'
#' @examples
getNumericYear <- function(lanaUrl = "cloud-backend.lanalabs.com", lanaToken, logId, attribute, agg,
                                dateType = "startDate", valuesFrom = "allCases", maxAmountAttributes = 10,
                                valueSorting = "CaseCount", sortingOrder = "Descending", traceFilterSequence = list()){

  groupingType <- "byYear"
  freqNumericYear <- numContentBuilder(lanaUrl, lanaToken, logId, dateType, attribute, agg, valuesFrom, groupingType,
                                  maxAmountAttributes, valueSorting, sortingOrder, traceFilterSequence)
  return(freqNumericYear)

}

#' aggregated numeric attribute per quarter
#'
#' @param lanaUrl
#' @param lanaToken
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilterSequence
#' @param attribute
#' @param agg mean, median, sum, min, max
#' @param valueSorting
#' @param sortingOrder
#'
#' @return
#' @export
#'
#' @examples
getNumericQuarter <- function(lanaUrl = "cloud-backend.lanalabs.com", lanaToken, logId, attribute, agg,
                           dateType = "startDate", valuesFrom = "allCases", maxAmountAttributes = 10,
                           valueSorting = "CaseCount", sortingOrder = "Descending", traceFilterSequence = list()){

  groupingType <- "byQuarter"
  freqNumericQuarter <- numContentBuilder(lanaUrl, lanaToken, logId, dateType, attribute, agg, valuesFrom, groupingType,
                                    maxAmountAttributes, valueSorting, sortingOrder, traceFilterSequence)
  return(freqNumericQuarter)

}

#' aggregated numeric attribute per quarter
#'
#' @param lanaUrl
#' @param lanaToken
#' @param logId
#' @param dateType
#' @param valuesFrom
#' @param maxAmountAttributes
#' @param traceFilterSequence
#' @param attribute
#' @param agg mean, median, sum, min, max
#' @param valueSorting
#' @param sortingOrder
#'
#' @return
#' @export
#'
#' @examples
getNumericQuarter <- function(lanaUrl = "cloud-backend.lanalabs.com", lanaToken, logId, attribute, agg,
                              dateType = "startDate", valuesFrom = "allCases", maxAmountAttributes = 10,
                              valueSorting = "CaseCount", sortingOrder = "Descending", traceFilterSequence = list()){

  groupingType <- "byQuarter"
  freqNumericQuarter <- numContentBuilder(lanaUrl, lanaToken, logId, dateType, attribute, agg, valuesFrom, groupingType,
                                          maxAmountAttributes, valueSorting, sortingOrder, traceFilterSequence)
  return(freqNumericQuarter)

}
