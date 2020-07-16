# How to test

* tests require access to a lana api
* we require the user has access to the log `Incident_management.csv`
* credentials have to be provided in the file `/testthat/config.R`
    ```R
      lanaUrl <- “<URL of LANA instance>”
      selectedLogId <- “<LogId of Incident_management.csv>“
      lanaToken <- “<apiKey>”
    ```
