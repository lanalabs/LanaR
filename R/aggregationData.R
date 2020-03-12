# build aggregation settings for the API call

buildAggregationSettings <- function(valuesFrom, extractedValue, aggregationFunction, grouping, secondaryGrouping, outerDateType, innerDateType, maxAmountAttributes, logId, activityExclusionFilter, traceFilterSequence, limit, page) {

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
  if (extractedValue %in% c("frequency", "duration", "startDate", "endDate")){
    extractedValue = paste0('
      {
      "type": "', extractedValue, '",
      "aggregationFunction": "', aggregationFunction, '"
      }')
  } else {
    extractedValue = paste0('
      {
      "type": "attribute",
      "attribute": "', extractedValue, '",
      "aggregationFunction": "', aggregationFunction, '"
      }')
  }

  # process the grouping and secondaryGrouping according to API documentation
  if (grouping != "null") {
    if (grouping %in% c("byActivity", "byDuration")){
      grouping = paste0('
        {
        "type": "', grouping, '"
        }')
    } else if (grouping %in% c("byYear", "byMonth", "byQuarter", "byDayOfWeek", "byDayOfYear", "byHourOfDay")){
      grouping = paste0('
      {
      "type": "', grouping, '",
      "dateType": "', outerDateType, '"
      }')
    } else {
      grouping = paste0('
      {
      "type": "byAttribute",
      "attribute": "', grouping, '"
      }')
    }
  }

  if (secondaryGrouping != "null") {
    if (secondaryGrouping %in% c("byActivity", "byDuration")){
      secondaryGrouping = paste0('
        {
        "type": "', secondaryGrouping, '"
        }')
    } else if (secondaryGrouping %in% c("byYear", "byMonth", "byQuarter", "byDayOfWeek", "byDayOfYear", "byHourOfDay")){
      secondaryGrouping = paste0('
      {
      "type": "', secondaryGrouping, '",
      "dateType": "', innerDateType, '"
      }')
    } else {
      secondaryGrouping = paste0('
      {
      "type": "byAttribute",
      "attribute": "', secondaryGrouping, '"
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

  if (grouping == "null"){
    rqBody <- paste0('
      {
      "valuesFrom": ', valuesFrom, ',
      "metric": ', extractedValue, ',
      "options": ', maxAmountAttributes, ',
      "miningRequest": ', miningBody,'
      }')
  } else {
    rqBody <- paste0('
      {
      "valuesFrom": ', valuesFrom, ',
      "metric": ', extractedValue, ',
      "grouping": ', grouping, ',
      "secondaryGrouping": ', secondaryGrouping, ',
      "options": ', maxAmountAttributes, ',
      "miningRequest": ', miningBody,'
      }')
  }

}

#' @title Aggregate
#' Aggregate data once uploaded to Lana
#' Aggregations can be calculated by time (month, day of week, hour) or by attribute regarding the frequency, average duration, median duration and total duration. Also the aggregated data can be grouped by attributes.
#' @description Gets the aggregation of the requested data with the specified parameters . \cr See https://gitlab.lana-labs.com/lana-labs/lana-backend-scala/blob/master/janus/assets/static/swagger.yml#model-AggregationRequest
#' @param valuesFrom ("allCases" / "allEvents" / "allFollowers" / "event" / "followers") - if input is event, valuesFrom should be the activity name; if input is followers, valuesFrom should contain both pre and succ separated by comma.
#' @param extractedValue ("frequency" / "duration" / "startDate" / "endDate" / "attributeValue") - if input is a attribute value, extractedValue should be that attribute name
#' @param aggregationFunction (optional, "min" / "max" / "sum" / "mean" / "median")
#' @param grouping (optional, attributeName / "byActivity" / "byDuration" / "byYear" / "byMonth" / "byQuarter" / "byDayOfWeek" / "byDayOfYear" / "byHourOfDay")
#' @param outerDateType (optional, the date type for grouping)
#' @param secondaryGrouping (optional, attributeName / "byActivity" / "byDuration" / "byYear" / "byMonth" / "byQuarter" / "byDayOfWeek" / "byDayOfYear" / "byHourOfDay")
#' @param innerDateType (optional, the date type for secondaryGrouping)
#' @param maxAmountAttributes (optional, default = 4)
#' @param lanaUrl URL of the instance that LANA is running on
#' @param lanaToken Lana API token read from LANA
#' @param logId Log ID being read from LANA
#' @param cache (optional, default = "{}")
#' @param maxValueAmount Define the amount of values you wanto tdisplay before the rest are aggregated into "other" (optional, default = 5)
#' @param activityExclusionFilter Hide activities in aggregation (optional, default = "[]")
#' @param traceFilterSequence Integrate any kind of filter from lana into your aggregation (optional, default = "[]")
#' @param limit (optional, default = 10)
#' @param page (optional, default = 1)
#' @return Aggregated data
aggregate <- function(lanaUrl, lanaToken, logId, valuesFrom, extractedValue, aggregationFunction="null", grouping="null", secondaryGrouping="null",
                      outerDateType="null", innerDateType="null", maxAmountAttributes=4, activityExclusionFilter="[]", traceFilterSequence="[]",
                      limit = 10, page = 1) {

  # Make request to get aggregated data from LANA


  rqBody <- buildAggregationSettings(valuesFrom, extractedValue, aggregationFunction, grouping, secondaryGrouping, outerDateType, innerDateType,
                                     maxAmountAttributes, logId, activityExclusionFilter, traceFilterSequence, limit, page)

  aggregationRequestData <- httr::POST(paste0(lanaUrl, "/api/v2/aggregate-data"),
                                      body = list(request = rqBody),
                                      encode = "multipart",
                                      httr::add_headers(Authorization = lanaToken)
  )

  checkHttpErrors(aggregationRequestData)

  # Read response into data frame
  actAggrData <- jsonlite::fromJSON(httr::content(aggregationRequestData, as = "text", encoding = "UTF-8"))
  chartValues <- actAggrData$chartValues

  if(secondaryGrouping != "null"){
    chartValues <- chartValues %>% select(-`$type`) %>% unnest(values)
  }

  names(chartValues)[names(chartValues) == "xAxis"] <-  grouping

  if (extractedValue == "exists")
    names(chartValues)[names(chartValues) == "yAxis"] <- aggregationFunction
  else
    names(chartValues)[names(chartValues) == "yAxis"] <- extractedValue

  names(chartValues)[names(chartValues) == "zAxis"] <- secondaryGrouping

  chartValues$`$type` <- NULL
  chartValues$`$type1` <- NULL

  return(chartValues)
}
