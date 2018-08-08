#' @title \code Upload Csv Log without Case Attributes
#' @description uploads a csv log file without case attributes to Lana
#' @param logName, logSemantics
#' @name uploadWithoutCase

uploadWithoutCase <- function(logName, eventSemanticsJson){

  checkAuthentication()
  lanaApiUrl <- Sys.getenv("LANA_URL")
  #doesn't work with
  #lanaApiUrl <- "http://localhost:9000/api/"

  lanaAuthorization <- Sys.getenv("LANA_TOKEN")
  lanaUserId <- Sys.getenv("LANA_USERID")


  responseLogCreation <- httr::POST(paste0(lanaApiUrl, "/api", "logs/csv"),
                              body = list(eventSemantics=eventSemanticsJson, file=httr::upload_file(logName, "text/csv")),
                              encode = "multipart",
                              httr::add_headers(Authorization = lanaAuthorization),
                              httr::verbose(data_out = F)
)

  stopifnot(responseLogCreation$status_code == 200)
}
