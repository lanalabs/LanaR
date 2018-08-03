#' \code a method that gives the recent log files
#' @return prints out a list of recent log files
#' @name getLogs


getLogs <- function() {

  checkAuthentication()
  lanaApiUrl <- Sys.getenv("LANA_URL")
  lanaAuthorization <- Sys.getenv("LANA_TOKEN")
  lanaUserId <- Sys.getenv("LANA_USERID")

  jsonlite::fromJSON(
    httr::content(httr::GET(paste0(lanaApiUrl, "/api/users/", lanaUserId, "/logs"), httr::add_headers(Authorization = lanaAuthorization)),  encoding = "UTF-8", type = "text")
  )
}
