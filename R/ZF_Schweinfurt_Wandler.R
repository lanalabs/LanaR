---
  title: "ZF Schweinfurt Wandler"
author: "Lana Labs GmbH"
date: "8/22/2018"
output:
  flexdashboard::flex_dashboard:
  orientation: rows
vertical_layout: scroll
runtime: shiny
---


#################################
#### Setup required packages ####
#################################
list.of.packages <- c("rstudioapi","httr", "jsonlite", "stringr", "ggplot2", "plyr", "tidyr", "knitr", "flexdashboard", "highcharter", "reshape2", "dplyr", "devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for(package in list.of.packages) {
  eval(parse(text=paste("require(",package,")")))
}

devtools::install_github("lanalabs/lanar")

# Authenticate against LANA
lanar::authenticate(Sys.getenv("LANA_URL"), Sys.getenv("LANA_TOKEN"))

myUserLogs <- lanar::getLogs()

logId <- "35"

##############################
#### Case Count per month ####
##############################
rqBodyCaseCount <- paste0('{
  "xDimension": "byTime=byMonth",
  "yDimension": "frequency",
  "zDimension": null,
  "followers": null,
  "aggregationType": "traces",
  "maxValueAmount": 3,
  "type": "aggregation",
  "miningRequest": {
    "activityExclusionFilter": [],
    "includeHeader": true,
    "includeLogId": true,
    "logId": ', logId, ',
    "edgeThreshold": 1,
    "traceFilterSequence": [
      {
        "max": 23,
        "min": 1,
        "type": "variantSliderFilter"
      }
      ],
    "runConformance": false,
    "sort": "start",
    "limit": 10,
    "page": 1
  }
}')

caseCountAggrData <- lanar::getAggregationRequestData(rqBodyCaseCount)
caseCountAggrData <- actAggrData[ -c(3)]
names(caseCountAggrData) <- c("Date","Frequency")
hchart(caseCountAggrData, "column", hcaes(x = "Date", y = "Frequency"))

##########################################
#### Average Duration Cases per month ####
##########################################

rqBodyAvgDuration <- paste0('{
  "xDimension": "byTime=byMonth",
  "yDimension": "avgDuration",
  "zDimension": null,
  "followers": null,
  "aggregationType": "traces",
  "maxValueAmount": 10,
  "yDimensionMetric": "",
  "type": "aggregation",
  "cache": {},
  "miningRequest": {
    "activityExclusionFilter": [],
    "includeHeader": true,
    "includeLogId": true,
    "logId": ', logId, ',
    "edgeThreshold": 1,
    "traceFilterSequence": [
      {
        "max": 23,
        "min": 1,
        "type": "variantSliderFilter"
      }
    ],
    "runConformance": false,
    "sort": "",
    "limit": 20,
    "page": 1
  }
}')

avgDurationAggrData <- lanar::getAggregationRequestData(rqBodyAvgDuration)
avgDurationAggrData <- avgDurationAggrData[ -c(3)]
names(avgDurationAggrData) <- c("Date","Average")
hchart(avgDurationAggrData, "column", hcaes(x = "Date", y = "Average"))

#########################################
#### Median Duration Cases per month ####
#########################################

rqBodyMedianDuration <- paste0('{
  "xDimension": "byTime=byMonth",
  "yDimension": "medianDuration",
  "zDimension": null,
  "followers": null,
  "aggregationType": "traces",
  "maxValueAmount": 10,
  "yDimensionMetric": "",
  "type": "aggregation",
  "cache": {},
  "miningRequest": {
    "activityExclusionFilter": [],
    "includeHeader": true,
    "includeLogId": true,
    "logId": ', logId, ',
    "edgeThreshold": 1,
    "traceFilterSequence": [
      {
        "max": 23,
        "min": 1,
        "type": "variantSliderFilter"
      }
    ],
    "runConformance": false,
    "sort": "",
    "limit": 20,
    "page": 1
  }
}')

medianDurationAggrData <- lanar::getAggregationRequestData(rqBodyMedianDuration)
medianDurationAggrData <- medianDurationAggrData[ -c(3)]
names(medianDurationAggrData) <- c("Date","Median")
hchart(medianDurationAggrData, "column", hcaes(x = "Date", y = "Median"))

##############################################################
#### Development of total duration per activity per month ####
##############################################################

rqBodyTotalDuration <- paste0('{
  "xDimension": "byTime=byMonth",
  "yDimension": "totalDuration",
  "zDimension": null,
  "followers": null,
  "aggregationType": "traces",
  "maxValueAmount": 10,
  "yDimensionMetric": "",
  "type": "aggregation",
  "cache": {},
  "miningRequest": {
    "activityExclusionFilter": [],
    "includeHeader": true,
    "includeLogId": true,
    "logId": ', logId, ',
    "edgeThreshold": 1,
    "traceFilterSequence": [
      {
        "max": 23,
        "min": 1,
        "type": "variantSliderFilter"
      }
    ],
    "runConformance": false,
    "sort": "",
    "limit": 20,
    "page": 1
  }
}')

totalDurationAggrData <- lanar::getAggregationRequestData(rqBodyMedianDuration)
totalDurationAggrData <- totalDurationAggrData[ -c(3)]
names(totalDurationAggrData) <- c("Date","Total")
hchart(totalDurationAggrData, "column", hcaes(x = "Date", y = "Total"))


