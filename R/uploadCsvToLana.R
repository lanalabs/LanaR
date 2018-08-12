#' @title \code Upload CSV log without case attributes
#' @description Uploads a CSV log file without case attributes to LANA
#' @param logName, logSemantics
#' @name uploadWithoutCaseAttr
uploadWithoutCaseAttr <- function(logName, eventSemanticsJson) {

  checkAuthentication()
  lanaApiUrl <- Sys.getenv("LANA_URL")
  lanaAuthorization <- Sys.getenv("LANA_TOKEN")
  lanaUserId <- Sys.getenv("LANA_USERID")

  responseLogCreation <- httr::POST(paste0(lanaApiUrl, "/api", "logs/csv"),
                              body = list(eventSemantics=eventSemanticsJson, file=httr::upload_file(logName, "text/csv")),
                              encode = "multipart",
                              httr::add_headers(Authorization = lanaAuthorization),
                              httr::verbose(data_out = F)
  )

  checkHttpErrors(responseLogCreation)
}
