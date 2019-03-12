# build aggregation settings for the API call

buildVariantFilterSettings <- function(logId, traceFilterSequence, runConformance){
    rqBody <- paste0('
          {
            "activityExclusionFilter": [],
            "includeHeader": true,
            "includeLogId": true,
            "logId": ', logId,' ,
            "edgeThreshold": 1,
            "traceFilterSequence": ', traceFilterSequence,' ,
            "runConformance": ', runConformance, ',
            "graphControl": {
            "sizeControl": "Frequency",
            "colorControl": "AverageDuration"
            },
            "sort": "cases",
            "limit": 20,
            "page": 1
          }
      ')
}

#' @title Get discovered model data
#' @description Get the discovered model data, which includes logId, modelId, logStatistics, variants and discoveredModels. \cr See https://api.lana-labs.com/#/routes/getDiscoveredModelWithFilter
#' @return discovered model
#' @param logName Full name of the uploaded csv file in Lana
#' @param traceFilterSequence Integrate any kind of filter from lana into your aggregation (optional, default = "[]")
#' @param runConformance Decide whether you ant to include conformance data with a booelan value (optional, default = "true")
#' @name discoveredModel

discoveredModel <- function(lanaUrl, lanaToken, logId, traceFilterSequence="[]", runConformance="true"){

  # Check the variables being tramsitted from LANA and receiving the user ID.

  rqBody <- lanar::buildVariantFilterSettings(selectedLogId, traceFilterSequence, "true")


  discoveredModelRequestData <- httr::GET(paste0(lanaUrl, "/api/discoveredModelWithFilter?request=", URLencode(rqBody, reserved = T)),
                                          httr::add_headers(Authorization = lanaToken))


  if (!discoveredModelRequestData$status_code==400){
    discoveredModelData <- jsonlite::fromJSON(httr::content(discoveredModelRequestData, as = "text", encoding = "UTF-8"))
    } else {
      return(NULL)
  }

  return(discoveredModelData)
}

#' @title Get activity performance statistics
#' @description Get the activity performance statistics, which include activity durations and counts.
#' @return activity performance statistics as data frame
#' @param discoveredModelData
#' @name activityPerformance

activityPerformance <- function(lanaUrl, lanaToken, logId){

  discoveredModelData <- discoveredModel(lanaUrl, lanaToken, logId)

  if(length(discoveredModelData) == 0) {
    stop(paste0("The model is empty"))
  }

  actStats <- ldply(discoveredModelData$activityPerformanceStatistics, data.frame)

  return(actStats)
}

