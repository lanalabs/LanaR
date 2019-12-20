# build aggregation settings for the API call

buildAggregationSettings <- function(valuesFrom, extractedValue, aggregationType, outerBinning, innerBinning, outerDateType, innerDateType, timeZone, maxAmountAttributes, logId, activityExclusionFilter, traceFilterSequence, limit, page) {

  # process the valueFrom attribute
  # if input within "allCases", "allEvents", "allFollowers", put it directly as type
  # if input contains a comma, separate into pre and succ
  # if input does not contain commas, treat it as an activity
  if (valuesFrom %in% c("allCases", "allEvents", "allFollowers")){
    valuesFrom = paste0('
      {
      "type": "', valuesFrom, '"
      }')
  } else if (grepl(",", valuesFrom)){
    events = unlist(strsplit(valuesFrom, "[,]"))
    valuesFrom = paste0('
      {
      "type": "follower",
      "preActivity": "', events[1], '",
      "succActivity": "', events[2], '"
      }')
  } else {
    valuesFrom = paste0('
      {
      "type": "event",
      "activity": "', valuesFrom, '"
      }')
  }

  # process the extractedValue attribute
  # if input within "frequency", "duration", "startDate", "endDate", put it directly as type
  # else treat it as the attribute name
  if (extractedValue %in% c("frequency", "duration", "startDate", "endDate", "exists")){
    extractedValue = paste0('
      {
      "type": "', extractedValue, '"
      }')
  } else {
    extractedValue = paste0('
      {
      "type": "attributeValue",
      "attribute": "', extractedValue, '"
      }')
  }

  # process the outerbinning and innerbinning according to API documentation
  if (outerBinning != "null") {
    if (outerBinning %in% c("byActivity", "byDuration")){
      outerBinning = paste0('
        {
        "type": "', outerBinning, '"
        }')
    } else if (outerBinning %in% c("byYear", "byMonth", "byQuarter", "byDayOfWeek", "byDayOfYear", "byHourOfDay")){
      outerBinning = paste0('
      {
      "type": "', outerBinning, '",
      "dateType": "', outerDateType, '",
      "timeZone": "', timeZone, '"
      }')
    } else {
      outerBinning = paste0('
      {
      "type": "byAttribute",
      "attribute": "', outerBinning, '"
      }')
    }
  }

  if (innerBinning != "null") {
    if (innerBinning %in% c("byActivity", "byDuration")){
      innerBinning = paste0('
        {
        "type": "', innerBinning, '"
        }')
    } else if (innerBinning %in% c("byYear", "byMonth", "byQuarter", "byDayOfWeek", "byDayOfYear", "byHourOfDay")){
      innerBinning = paste0('
      {
      "type": "', innerBinning, '",
      "dateType": "', innerDateType, '",
      "timeZone": "', timeZone, '"
      }')
    } else {
      innerBinning = paste0('
      {
      "type": "byAttribute",
      "attribute": "', innerBinning, '"
      }')
    }
  }

  maxAmountAttributes = paste0('{"maxAmountAttributes": ', maxAmountAttributes, '}')

  miningBody <- paste0('
         {
          "activityExclusionFilter":', activityExclusionFilter, ',
          "includeHeader": true,
          "includeLogId": true,
          "logId": "', logId, '",
          "traceFilterSequence":', traceFilterSequence, ',
          "runConformance": false,
          "sort": "start",
          "limit": ', limit, ',
          "page": ', page, '
         }')

  if (outerBinning == "null"){
    rqBody <- paste0('
      {
      "valuesFrom": ', valuesFrom, ',
      "extractedValue": ', extractedValue, ',
      "aggregationType": "', aggregationType, '",
      "options": ', maxAmountAttributes, ',
      "miningRequest": ', miningBody,'
      }')
  } else {
    rqBody <- paste0('
      {
      "valuesFrom": ', valuesFrom, ',
      "extractedValue": ', extractedValue, ',
      "aggregationType": "', aggregationType, '",
      "outerBinning": ', outerBinning, ',
      "innerBinning": ', innerBinning, ',
      "options": ', maxAmountAttributes, ',
      "miningRequest": ', miningBody,'
      }')
  }

}

#' @title Aggregate
#' Aggregate data once uploaded to Lana
#' Aggregations can be calculated by time (month, day of week, hour) or by attribute regarding the frequency, average duration, median duration and total duration. Also the aggregated data can be grouped by attributes.
#' @description Gets the aggregation of the requested data with the specified parameters . \cr See https://api.lana-labs.com/#/routes/getAggregatedData
#' @param valuesFrom ("allCases" / "allEvents" / "allFollowers" / "event" / "followers") - if input is event, valuesFrom should be the activity name; if input is followers, valuesFrom should contain both pre and succ separated by comma.
#' @param extractedValue ("frequency" / "duration" / "startDate" / "endDate" / "attributeValue") - if input is a attribute value, extractedValue should be that attribute name
#' @param aggregationType (optional, "min" / "max" / "sum" / "mean" / "median")
#' @param outerBinning (optional, "byActivity" / "byDuration" / "byYear" / "byMonth" / "byQuarter" / "byDayOfWeek" / "byDayOfYear" / "byHourOfDay")
#' @param outerDateType (optional, the date type for outerbinning)
#' @param innerBinning (optional, "byActivity" / "byDuration" / "byYear" / "byMonth" / "byQuarter" / "byDayOfWeek" / "byDayOfYear" / "byHourOfDay")
#' @param innerDateType (optional, the date type for innerbinning)
#' @param timeZone (optional, time zone id for outer and inner binning)
#' @param maxAmountAttributes (optional, default = 4)
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

aggregate <- function(lanaUrl, lanaToken, logId, valuesFrom, extractedValue, aggregationType="null", outerBinning="null", innerBinning="null",
                      outerDateType="null", innerDateType="null", timeZone="null", maxAmountAttributes=4, activityExclusionFilter="[]", traceFilterSequence="[]",
                      limit = 10, page = 1) {

  # Make request to get aggregated data from LANA


  rqBody <- buildAggregationSettings(valuesFrom, extractedValue, aggregationType, outerBinning, innerBinning, outerDateType, innerDateType, timeZone,
                                     maxAmountAttributes, logId, activityExclusionFilter, traceFilterSequence, limit, page)

  aggregationRequestData <- httr::GET(paste0(lanaUrl, "/api/v2/aggregatedData?request=", URLencode(rqBody, reserved = T)),
                                      httr::add_headers(Authorization = lanaToken)
  )

  checkHttpErrors(aggregationRequestData)

  # Read response into data frame
  actAggrData <- jsonlite::fromJSON(httr::content(aggregationRequestData, as = "text", encoding = "UTF-8"))
  chartValues <- actAggrData$chartValues

  if(innerBinning != "null"){
    chartValues <- chartValues %>% select(-`$type`) %>% unnest(values)
  }

  names(chartValues)[names(chartValues) == "xAxis"] <-  outerBinning

  if (extractedValue == "exists")
    names(chartValues)[names(chartValues) == "yAxis"] <- aggregationType
  else
    names(chartValues)[names(chartValues) == "yAxis"] <- extractedValue

  names(chartValues)[names(chartValues) == "zAxis"] <- innerBinning

  chartValues$`$type` <- NULL
  chartValues$`$type1` <- NULL

  return(chartValues)
}
