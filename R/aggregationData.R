#' @title \code Aggregate data by different dimensions
#' @description a method that gets the aggregation of the requested data. \cr See https://api.lana-labs.com/#/routes/getAggregatedData
#' @return total of the aggregated data
#' @param discoveredModelData
#' @name getAggregationRequestData

getAggregationRequestData <- function(rqBody){
  checkAuthentication()
  lanaApiUrl <- Sys.getenv("LANA_URL")
  lanaAuthorization <- Sys.getenv("LANA_TOKEN")
  lanaUserId <- Sys.getenv("LANA_USERID")

  # Make request LANA
  aggregationRequestData <- GET(paste0(lanaApiUrl, "/api/aggregatedData?request=", URLencode(rqBody)),
                                add_headers(Authorization = lanaAuthorization)
  )

  # Read response into data frame
  actAggrData <- fromJSON(content(aggregationRequestData, as = "text"))
  chartValues <- plyr::rename(actAggrData$chartValues, c(".id"="action"))
  return(chartValues)
}
