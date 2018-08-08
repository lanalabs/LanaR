download <- function(csvUrl){

  #csvUrl <- 'https://lana-labs.com/examples/Incident_withImpactAttributes.csv'

url <- csvUrl
filename <- substr(csvUrl, 31, 100)
workingDirectory <- getwd()

destination <- paste0(workingDirectory,filename,".csv")

download.file(csvUrl, destfile = destination)
}
