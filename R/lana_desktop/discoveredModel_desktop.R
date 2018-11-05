buildVariantDesktopFilterSettings <- function(logId, variantMin, variantMax){
    rqBody <- paste0('
          {
            "activityExclusionFilter": [],
            "includeHeader": true,
            "includeLogId": true,
            "logId": ', logId,' ,
            "edgeThreshold": 1,
            "traceFilterSequence": [
            {
            "type": "variantSliderFilter",
            "min": ', variantMin,' ,
            "max": ', variantMax,'
            }
            ],
            "runConformance": false,
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
#' @param rqBody - request body as JSON
#' @name filter
filter_desktop <- function(logName, variantMin = 1, variantMax = 10000){

  logId <- lanar::chooseLog(logName)

  rqBody <- lanar::buildVariantFilterSettings(logId, variantMin, variantMax)


  discoveredModelRequestData <- httr::GET(paste0(lanaApiUrl, "/api/discoveredModelWithFilter?request=", URLencode(rqBody, reserved = T)))

  checkHttpErrors(discoveredModelRequestData)

  discoveredModelData <- jsonlite::fromJSON(httr::content(discoveredModelRequestData, as = "text", encoding = "UTF-8"))
  return(discoveredModelData)
}

#' @title Get activity performance statistics
#' @description Get the activity performance statistics, which include activity durations and counts.
#' @return activity performance statistics as data frame
#' @param discoveredModelData
#' @name activityPerformance
activityPerformance <- function(logName){

  discoveredModelData <- filter(logName)

  if(length(discoveredModelData) == 0) {
    stop(paste0("The model is empty"))
  }

  actStats <- ldply(discoveredModelData$activityPerformanceStatistics, data.frame)

  return(actStats)
}


