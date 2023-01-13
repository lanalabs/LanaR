#' Make authorisation headers for Appian Process Mining api
#' @param applicationKey the application key with or without starting 'API-Key'
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
#' @param removeFilters filter types to remove
#' @export
handleTraceFilterArgument <- function(traceFilter, removeFilters = list()) {
  
  #TODO: Warnings are shown when a traceFilterSequence list is passed
  res <- if (typeof(traceFilter) == 'character' )
    jsonlite::fromJSON(traceFilter, simplifyVector = FALSE) else if 
  (typeof(traceFilter) == 'list' & length(unlist(traceFilter) != 0))
        list(traceFilter) else traceFilter
  
  return(
    rlist::list.filter(res, !(type %in% removeFilters) )
  )
}