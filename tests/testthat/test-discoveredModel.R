context("Test context for aggregation function")
source("/Users/lanaguest/PycharmProjects/LanaR/LanaR/tests/testthat/config.R")
library(jsonlite)
library(tidyr)
library(purrr)
library(dplyr)

filePath <- '/Users/lanaguest/PycharmProjects/LanaR/LanaR/test_responses/'

#the expected model data is obtained from postman API calls

test_that("Direct follower results", {
  directFollowers_is <- discoveredModel(logId = selectedLogId,
                               lanaToken = lanaToken,
                               lanaUrl = lanaUrl,
                               traceFilterSequence = '[]'
                              )$directFollowerStatistics$directFollowers %>% select_if(~sum(!is.na(.)) > 0)

  directFollowers_exp <-  read_json(path = paste0(filePath,'response_1.json'))$directFollowerStatistics$directFollowers %>%
                                  map_df(flatten) %>% unnest() %>% select_if(~sum(!is.na(.)) > 0)

  testthat::expect_identical(directFollowers_exp, directFollowers_is)

})

test_that("Activity performance results", {
  activityPerformanceStatistics_is <- discoveredModel(logId = selectedLogId,
                               lanaToken = lanaToken,
                               lanaUrl = lanaUrl,
                               traceFilterSequence = '[]'
                              )$activityPerformanceStatistics

  activityPerformanceStatistics_exp <-  read_json(path = paste0(filePath,'response_1.json'))$activityPerformanceStatistics

  testthat::expect_identical(activityPerformanceStatistics_exp, activityPerformanceStatistics_is)

})

test_that("Discovered model results", {
  discoveredModel_is <- discoveredModel(logId = selectedLogId,
                               lanaToken = lanaToken,
                               lanaUrl = lanaUrl,
                               traceFilterSequence = '[]', renderDiscoveredModel = TRUE
                              )$discoveredModel

  discoveredModel_exp <-  read_json(path = paste0(filePath,'response_model.json'))$discoveredModel

  testthat::expect_identical(discoveredModel_exp, discoveredModel_is)

})

test_that("log statistics results", {
  logstat_is <- logStat(logId = selectedLogId,
                               lanaToken = lanaToken,
                               lanaUrl = lanaUrl,
                               traceFilterSequence = '[]'
                              )

  discoveredModelData <-  read_json(path = paste0(filePath,'response_model.json'))
  statList <- within(discoveredModelData$logStatistics, rm("numericAttributeRanges", "attributeCounts",
                                                           "conformanceStatistics"))
  logstat_exp <- as.data.frame(do.call(rbind, lapply(statList, `length<-`,
                                                  max(sapply(statList,length)))))
  logstat_exp <- setNames(cbind(rownames(logstat_exp), logstat_exp, row.names = NULL), c("Stats", "Values"))
  rownames(logstat_exp)<- logstat_exp$Stats
  logstat_exp<- transpose(logstat_exp)
  colnames(logstat_exp) <- logstat_exp[1,]
  logstat_exp <- logstat_exp[-1,]

  testthat::expect_identical(logstat_is, logstat_exp)

})