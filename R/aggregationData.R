#' @title Build aggregation settings
#' @description Builds the settings JSON needed for \code{\link{getAggregationRequestData}}. \cr See https://api.lana-labs.com/#/routes/getAggregatedData
#' @return Aggregation settings as JSON string
#' @param xDimension, yDimension, zDimension, aggrLevel, miningRequest
#' @name buildAggregationSettings
buildAggregationSettings <- function(xDimension, yDimension, zDimension, aggrLevel, miningRequest, followers="null", maxValueAmount="20", type="aggregation", cache="{}") {
  paste0('
         {
         "yDimension": "', yDimension, '",
         "xDimension": "', xDimension, '",
         "zDimension": "', zDimension, '",
         "followers": "', followers, '",
         "aggregationType": "', aggrLevel, '",
         "maxValueAmount": "', maxValueAmount, '",
         "type": "', aggregation, '",
         "cache": "', cache, '",
         "miningRequest": ', miningRequest,'
         }')
}

#' @title Aggregate data by different dimensions
#' @description a method that gets the aggregation of the requested data. \cr See https://api.lana-labs.com/#/routes/getAggregatedData
#' @return total of the aggregated data
#' @param discoveredModelData
#' @name aggregate
aggregate <- function(rqBody){
  checkAuthentication()
  lanaApiUrl <- Sys.getenv("LANA_URL")
  lanaAuthorization <- Sys.getenv("LANA_TOKEN")
  lanaUserId <- Sys.getenv("LANA_USERID")

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
