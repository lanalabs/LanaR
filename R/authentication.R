#' @name authenticate
#' a method, that authenticates the user
#'
#' DEPRECATED

authenticate <- function(lanaUrl, lanaToken) {

  # Obtaining the user ID by reading the token from LANA

  userInfo <- httr::GET(paste0(lanaUrl, "/api/users/by-token"), httr::add_headers(Authorization = lanaToken))

  if(userInfo$status_code != 200) {
    stop(paste0("Wrong user credentials. Error code = ", userInfo$status_code, " - ", httr::content(userInfo)$message))
  }

  userId <- jsonlite::fromJSON(httr::content(userInfo, encoding = "UTF-8", type = "text"))$id

  return(userId)

}

checkAuthentication <- function(lanaUrl, lanaToken) {

  if(is.null(lanaUrl) || lanaUrl == "") {
    stop("Authentication error: You haven't defined a URL for LANA.")
  }

  if(is.null(lanaToken) || lanaToken == "") {
    stop("Authentication error: You haven't defined a TOKEN for LANA.")
  }
}
