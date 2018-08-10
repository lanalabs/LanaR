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
