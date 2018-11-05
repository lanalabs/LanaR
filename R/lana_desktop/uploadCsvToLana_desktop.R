#' @title Upload CSV log without case attributes
#' @description Uploads a CSV log file without case attributes to LANA
#' @param logName, logSemantics
#' @name uploadWithoutCaseAttr
uploadWithoutCaseAttr_desktop <- function(logName, eventSemanticsJson) {

  responseLogCreation <- httr::POST(paste0(lanaApiUrl, "/api", "logs/csv"),
                              body = list(eventSemantics=eventSemanticsJson, file=httr::upload_file(logName, "text/csv")),
                              encode = "multipart",
                              httr::verbose(data_out = F)
  )

  checkHttpErrors(responseLogCreation)
}
