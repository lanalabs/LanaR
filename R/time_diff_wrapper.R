library(jsonlite)
library(httr)

url <- "localhost:9000/api/working-schedules/"
token <- "Bearer 8j*63lE&Rg2aOLcdfMWNLcAXjoxxxxxx"

s_id <- "17c628aa-a0b4-40ab-a5c9-4efdd0c15ac4"


#Test data:
start <- c("2019-04-29T18:00:04.762", "2019-04-30T18:00:04.762")
end <- c("2019-04-30T18:00:04.762", "2019-04-30T19:00:04.762")
column.names <- c("start", "end")

df <- data.frame("start" = start, "end" = end)


retrieve_differences <- function(schedule_id, df){
  #df to list
 x <- c(differences = unname(split(df, nrow(df))))

 response <- POST(paste0(url, s_id, "/time-differences/"),
                        body = x,
                        encode='json',
                        add_headers(Authorization = token),
                        verbose())

 diff_data <- content(response)

 return(diff_data)
}

diff_list <- retrieve_differences(schedule_id = s_id, df = df)

print(diff_list)
