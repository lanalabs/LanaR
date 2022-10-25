context("Test context for aggregation function")
source("/Users/lanaguest/PycharmProjects/LanaR/LanaR/tests/testthat/config.R")

#the expected data frames were generated with the old aggregate-function calling the /api/aggregatedData endpoint or via the LANA frontend

test_that("Change in discoveredModel results when timestamp format changed", {
  model_exp <- discoveredModel(logId = selectedLogId,
                               lanaToken = lanaToken,
                               lanaUrl = lanaUrl,
                               traceFilterSequence = list(),
                               computeNumericAttributeRanges = TRUE
                              )

  model_is <- discoveredModel(logId = selectedLogId,
                              lanaToken = lanaToken,
                              lanaUrl = lanaUrl,
                              traceFilterSequence = list(),
                           computeNumericAttributeRanges = FALSE) # change in timestamp format

  testthat::expect_identical(model_exp, model_is)

})

test_that("aggregations with numeric attribute metrics return the expected dataframe", {
  df_aggregation <- aggregate(yDimension = "byAttribute=Cost",
                              traceFilterSequence = "[]",
                              lanaToken = lanaToken,
                              lanaUrl = lanaUrl,
                              logId = selectedLogId,
                              aggregationFunction = "sum",
                              aggrLevel = "traces")

  caseCount <- c(2000)
  noAggregation <- c("No grouping")
  Cost <- c(8536000)

  df_expected <- data.frame(caseCount, noAggregation, Cost, stringsAsFactors = FALSE) %>%
    dplyr::mutate(caseCount = as.integer(caseCount), Cost = as.integer(Cost))

  testthat::expect_identical(df_expected, df_aggregation)

})

test_that("aggregations with specified valueSorting and sortingOrder return the expected dataframe", {
  df_aggregation <- aggregate(xDimension = "Classification",
                              yDimension = "avgDuration",
                              traceFilterSequence = "[]",
                              lanaToken = lanaToken,
                              lanaUrl = lanaUrl,
                              logId = selectedLogId,
                              aggrLevel = "allCases",
                              valueSorting = "alphabetic",
                              sortingOrder = "ascending")

  caseCount <- c(109, 295, 57, 610, 929)
  Classification <- c("Backup", "Citrix", "Intranet", "Mail", "SAP")
  avgDuration <- c(23770458.715596333, 63067525.423728816, 22307368.421052627, 27052327.868852418, 49583186.221743822)

  df_expected <- data.frame(caseCount, Classification, avgDuration, stringsAsFactors = FALSE) %>%
    dplyr::mutate(caseCount = as.integer(caseCount))

  testthat::expect_identical(df_expected, df_aggregation)

})

test_that("aggregations with a traceFilterSequence and maxValueAmount return the expected dataframe", {
  df_aggregation <- aggregate(xDimension = "byTime=byHour",
                              yDimension = "byAttribute=Cost",
                              traceFilterSequence = '[{"pre":"Incident classification","succ":"Functional escalation","direct":false,"useDuration":false,"type":"followerFilter","inverted":false}]',
                              maxValueAmount = 9,
                              lanaToken = lanaToken,
                              lanaUrl = lanaUrl,
                              logId = selectedLogId,
                              aggrLevel = "events")

  caseCount <- c(1080, 978, 762, 368, 281, 142, 140, 139, 133, 1677)
  byHour <- c("13", "12", "14", "11", "15", "23", "16", "6", "4", "Other")
  Cost <- c(146000, 120000, 162000, 176000, 109000, 94000, 64000, 168000, 149000, 2223000)

  df_expected <- data.frame(caseCount, byHour, Cost, stringsAsFactors = FALSE) %>%
    dplyr::mutate(caseCount = as.integer(caseCount), Cost = as.integer(Cost))

  testthat::expect_identical(df_expected, df_aggregation)

})
