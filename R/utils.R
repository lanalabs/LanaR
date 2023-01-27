#' Make authorisation headers for Appian Process Mining api
#' @param applicationKey the application key with or without starting 'API-Key '
#' @export

makeAuthorisationHeader <- function(applicationKey) {
  headerFields <-
    c(
      Authorization = applicationKey
    )
  
  return(headerFields)
}

#' Programmatically remove filters from a trace filter sequence by type
#' @param traceFilter the trace filter as character or R list
#' @export

handleTraceFilterArgument <- function(traceFilter) {

  res <- if (typeof(traceFilter) == 'character' )
    jsonlite::fromJSON(traceFilter, simplifyVector = FALSE) else if
  (typeof(traceFilter) == 'list' & any(sapply(traceFilter, is.list)) == FALSE & length(unlist(traceFilter) != 0))
        list(traceFilter) else traceFilter

  return(res)
}

joinTFS <- function(tfsfix, tfsdyn, filterString){
  tfsFromHere <- fromJSON(paste0(tfsfix))
  tfsFromLanaDF <- fromJSON(paste0(tfsdyn))


  if (NROW(tfsFromLanaDF)>0){
    if ("attributeFilter" %in% tfsFromLanaDF$type){
      tfsFromLanaDF <- tfsFromLanaDF[!tfsFromLanaDF$type == filterString,]
    }
  }

  if (NROW(tfsFromLanaDF)>0){
    tfs <- rbind.fill(tfsFromHere, tfsFromLanaDF)
  } else {
    tfs <- tfsFromHere
  }

  return(toJSON(tfs))

}