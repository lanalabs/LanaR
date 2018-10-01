# build aggregation settings for the API call
buildAggregationSettings <- function(xDimension, yDimension, logId, zDimension="null", aggrLevel="traces", followers="null", type="aggregation", cache="{}", limit = 10, page = 1) {

  if (zDimension != "null" ){
    zDimension = paste0('"', zDimension, '"')
  }

  if (followers != "null" ){
    followers = paste0('"', zDimension, '"')
  }

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
           "limit": ', limit, ',
           "page": ', page, '
          }
         }')
}

#' @title Aggregate
#' Aggregate data once uploaded to Lana
#' Aggregations can be calculated by time (month, day of week, hour) or by attribute regarding the frequency, average duration, median duration and total duration. Also the aggregated data can be grouped by attributes.
#' @description Gets the aggregation of the requested data with the specified parameters . \cr See https://api.lana-labs.com/#/routes/getAggregatedData
#' @param logName Full name of the uploaded csv file in Lana
#' @param xDimension Define the x dimension for the aggregation
#' @param yDimension Define the y dimension for the aggregation
#' @param zDimension Define the z dimension for the aggregation (optional, default = "null")
#' @param aggrLevel Define the aggregation level (optional, default = "traces")
#' @param followers Define followers (optional, default = "null")
#' @param type (optional, default = "null")
#' @param cache (optional, default = "{}")
#' @param limit (optional, default = 10)
#' @param page (optional, default = 1)
#' @return Aggregated data
#' @examples
#' aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byMonth", yDimension = "frequency")
#' aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=dayOfWeek", yDimension = "avgDuration")
#' aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byHour", yDimension = "medianDuration")
#' aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byMonth", yDimension = "totalDuration")
#' aggregate("Incident_withImpactAttributes.csv", xDimension = "byAttribute=Activity", yDimension = "frequency", zDimension = "byAttribute=Stoerung vorhanden")

aggregate <- function(logName, xDimension, yDimension, zDimension="null", aggrLevel, followers, type, cache, limit, page){
  checkAuthentication()
  lanaApiUrl <- Sys.getenv("LANA_URL")
  lanaAuthorization <- Sys.getenv("LANA_TOKEN")
  lanaUserId <- Sys.getenv("LANA_USERID")

  logId <- lanar::chooseLog(logName)

  rqBody <- lanar::buildAggregationSettings(xDimension, yDimension, logId, zDimension)

  # Make request to get aggregated data from LANA
  aggregationRequestData <- httr::GET(paste0(lanaApiUrl, "/api/aggregatedData?request=", URLencode(rqBody, reserved = T)),
                                      httr::add_headers(Authorization = lanaAuthorization)
  )

  lanar::checkHttpErrors(aggregationRequestData)

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
