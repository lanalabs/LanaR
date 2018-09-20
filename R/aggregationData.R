#' @title Build aggregation settings
#' @description Builds the settings JSON needed for \code{\link{getAggregationRequestData}}. \cr See https://api.lana-labs.com/#/routes/getAggregatedData
#' @return Aggregation settings as JSON string
#' @param xDimension, yDimension, zDimension, aggrLevel, miningRequest
#' @name buildAggregationSettings
#' #' @examples
#' buildAggregationSettings("byTime=dayOfWeek", "frequency",  )
buildAggregationSettings <- function(xDimension, yDimension, logId, zDimension="null", aggrLevel="traces", followers="null", type="aggregation", cache="{}") {
  rqBody <- paste0('
         {
         "xDimension": "', xDimension, '",
         "yDimension": "', yDimension, '",
         "zDimension": ', zDimension, ',
         "aggregationType": "', aggrLevel, '",
         "type": "', type, '",
         "followers": ', followers, ',
         "cache": "', cache, '",
         "miningRequest": {
          "includeHeader": true,
          "includeLogId": true,
           "logId": ', logId, ',
           "runConformance": false,
           "sort": "start",
           "limit": 10,
           "page": 1
          }
         }')
}

#' @title Aggregate data by different dimensions
#' @description a method that gets the aggregation of the requested data. \cr See https://api.lana-labs.com/#/routes/getAggregatedData
#' @return total of the aggregated data
#' @param rqBody
#' @name aggregate
aggregate <- function(xDimension, yDimension, logName){
  checkAuthentication()
  lanaApiUrl <- Sys.getenv("LANA_URL")
  lanaAuthorization <- Sys.getenv("LANA_TOKEN")
  lanaUserId <- Sys.getenv("LANA_USERID")

  logId <- chooseLog(logName)

  rqBody <- buildAggregationSettings(xDimension, yDimension, logId)

  # Make request to get aggregated data from LANA
  aggregationRequestData <- httr::GET(paste0(lanaApiUrl, "/api/aggregatedData?request=", URLencode(rqBody, reserved = T)),
                                      httr::add_headers(Authorization = lanaAuthorization)
  )

  checkHttpErrors(aggregationRequestData)

  # Read response into data frame
  if(!isEmptyLog(aggregationRequestData)) {
    actAggrData <- jsonlite::fromJSON(httr::content(aggregationRequestData, as = "text", encoding = "UTF-8"))

    chartValues <- actAggrData$chartValues

    if(".id" %in% colnames(chartValues)) {
      chartValues <- plyr::rename(chartValues, c(".id"="action"))
    }

    return(chartValues)
  } else {
    return(NULL)
  }

}
