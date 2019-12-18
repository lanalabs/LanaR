# build aggregation settings for the API call

buildAggregationSettings <- function(valuesFrom, extractedValue, aggregationType, xDimension, yDimension, logId, zDimension, aggrLevel, followers, type, cache, maxValueAmount, activityExclusionFilter, traceFilterSequence, limit, page) {

  if (zDimension != "null" ){
    zDimension = paste0('"', zDimension, '"')
  }

  if (followers != "null" ){
    followers = paste0('"', zDimension, '"')
  }

  if (valuesFrom %in% c("allCases", "allEvents", "allFollowers")){
    valuesFrom = paste0('{"type": "', valuesFrom, '"}')
  } else if (grepl(",", valuesFrom)){
    events = unlist(strsplit(valuesFrom, "[,]"))
    valuesFrom = paste0('{"type": "follower", 
      "preActivity": "', events[1], '",
      "succActivity": "', events[2], '"}')
  } else {
    valuesFrom = paste0('{"type": "event",
      "activity": "', valuesFrom, '"}')
  }

  if (extractedValue %in% c("frequency", "duration", "startDate", "endDate")){
    extractedValue = paste0('{"type": "', extractedValue, '"}')
  } else {
    extractedValue = paste0('{"type": "attributeValue", 
      "attribute": "', extractedValue, '"}') 
  }

  miningBody <- paste0('
         {
         "xDimension": "', xDimension, '",
         "yDimension": "', yDimension, '",
         "zDimension": ', zDimension, ',
         "aggregationType": "', aggrLevel, '",
         "type": "', type, '",
         "followers": ', followers, ',
         "cache": "', cache, '",
         "maxValueAmount": ', maxValueAmount, ',
         "miningRequest": {
          "activityExclusionFilter":', activityExclusionFilter, ',
          "includeHeader": true,
          "includeLogId": true,
           "logId": "', logId, '",
           "traceFilterSequence":', traceFilterSequence, ',
           "runConformance": false,
           "sort": "start",
           "limit": ', limit, ',
           "page": ', page, '
          }
         }')

  rqBody <- paste0('{
    "valuesFrom": ', valuesFrom, ',
    "extractedValue": ', extractedValue, ',
    "aggregationType": ', aggregationType, ',
    "miningRequest": ', miningBody,'
    }')
}

#' @title Aggregate
#' Aggregate data once uploaded to Lana
#' Aggregations can be calculated by time (month, day of week, hour) or by attribute regarding the frequency, average duration, median duration and total duration. Also the aggregated data can be grouped by attributes.
#' @description Gets the aggregation of the requested data with the specified parameters . \cr See https://api.lana-labs.com/#/routes/getAggregatedData
#' @param valuesFrom ("allCases" / "allEvents" / "allFollowers" / "event" / "followers")
#' @param extractedValue ("frequency" / "duration" / "startDate" / "endDate" / "custom")
#' @param aggregationType ("min" / "max" / "sum" / "mean" / "median")
#' @param lanaUrl URL of the instance that LANA is running on
#' @param lanaToken Lana API token read from LANA
#' @param logId Log ID being read from LANA
#' @param xDimension Define the x dimension for the aggregation
#' @param yDimension Define the y dimension for the aggregation
#' @param zDimension Define the z dimension for the aggregation (optional, default = "null")
#' @param aggrLevel Define the aggregation level (optional, default = "traces")
#' @param followers Define followers (optional, default = "null")
#' @param type (optional, default = "null")
#' @param cache (optional, default = "{}")
#' @param maxValueAmount Define the amount of values you wanto tdisplay before the rest are aggregated into "other" (optional, default = 5)
#' @param activityExclusionFilter Hide activities in aggregation (optional, default = "[]")
#' @param traceFilterSequence Integrate any kind of filter from lana into your aggregation (optional, default = "[]")
#' @param limit (optional, default = 10)
#' @param page (optional, default = 1)
#' @return Aggregated data
#' @examples
#' aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byMonth", yDimension = "frequency")
#' aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=dayOfWeek", yDimension = "avgDuration")
#' aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byHour", yDimension = "medianDuration")
#' aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byMonth", yDimension = "totalDuration")
#' aggregate("Incident_withImpactAttributes.csv", xDimension = "byTime=byMonth", yDimension = "frequency", zDimension = "byAttribute=Est. Cost")

aggregate <- function(lanaUrl, lanaToken, logId, xDimension, yDimension, valuesFrom, extractedValue, aggregationType="null", zDimension="null", aggrLevel="traces", followers="null",
                      type="aggregation", cache="{}", maxValueAmount=5, activityExclusionFilter="[]", traceFilterSequence="[]",
                      limit = 10, page = 1){

  # Make request to get aggregated data from LANA

  rqBody <- buildAggregationSettings(valuesFrom, extractedValue, aggregationType, xDimension, yDimension, logId, zDimension, aggrLevel, followers, type, cache,
                                     maxValueAmount, activityExclusionFilter, traceFilterSequence, limit, page)

  aggregationRequestData <- httr::GET(paste0(lanaUrl, "/api/v2/aggregatedData?request=", URLencode(rqBody, reserved = T)),
                                      httr::add_headers(Authorization = lanaToken)
                                      )

  checkHttpErrors(aggregationRequestData)

  # Read response into data frame
  actAggrData <- jsonlite::fromJSON(httr::content(aggregationRequestData, as = "text", encoding = "UTF-8"))
  chartValues <- actAggrData$chartValues

  if(zDimension != "null"){
    chartValues <- chartValues %>% select(-`$type`) %>% unnest(values)
    }

  names(chartValues)[names(chartValues) == "xAxis"] <-  gsub(".*=", "", xDimension)

  names(chartValues)[names(chartValues) == "yAxis"] <- gsub(".*=", "", yDimension)

  names(chartValues)[names(chartValues) == "zAxis"] <- gsub(".*=", "", zDimension)

  chartValues$`$type` <- NULL
  chartValues$`$type1` <- NULL

  return(chartValues)
}
