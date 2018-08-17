buildMiningRequest <- function(logId, hideActivityFilter, traceFilterSequence) {
  paste0('{
         "hideActivityFilter": ', hideActivityFilter ,',
         "includeHeader": true,
         "includeLogId": true,
         "logId": ', logId, ',
         "edgeThreshold": 1,
         "traceFilterSequence": ', traceFilterSequence, ',
         "runConformance": false,
         "sort": "",
         "limit": 20,
         "page": 1
  }')
}
