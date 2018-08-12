buildMiningRequest <- function(activityExclusionFilter, traceFilterSequence) {
  paste0('{
         "activityExclusionFilter": ', activityExclusionFilter ,',
         "includeHeader": true,
         "includeLogId": true,
         "logId": ', logMuen, ',
         "edgeThreshold": 1,
         "traceFilterSequence": ', traceFilterSequence, ',
         "runConformance": false,
         "sort": "",
         "limit": 20,
         "page": 1
  }')
}
