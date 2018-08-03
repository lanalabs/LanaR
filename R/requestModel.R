getRequestedModel{function()

  checkAuthentication()
  lanaApiUrl <- Sys.getenv("LANA_URL")
  lanaAuthorization <- Sys.getenv("LANA_TOKEN")
  lanaUserId <- Sys.getenv("LANA_USERID")

  myUserLogs <- fromJSON(content(GET(paste0(lanaApiUrl, myUserLogs), add_headers(Authorization = lanaAuthorization)), type = "text"))
  logId <- myUserLogs[[1]][1] # get last uploaded log

  jsonlite::fromJSON(
    httr::content(httr::GET(paste0(lanaApiUrl, "/api/users/", lanaUserId, "/discoveredModel"), httr::add_headers(Authorization = lanaAuthorization)),  encoding = "UTF-8", type = "text")
  )

  discoveredModelRequestData <- GET(paste0(lanaApiUrl, "discoveredModelWithFilter?request=", URLencode(rqBody)),
                                    add_headers(Authorization = lanaAuthorization)

  discoveredModelData <- fromJSON(content(discoveredModelRequestData, as = "text"))

}
