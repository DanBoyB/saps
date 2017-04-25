library(tidyverse)
library(rjstat)
library(purrr)

url <- "http://www.cso.ie/StatbankServices/StatbankServices.svc/jsonservice/responseinstance/"
table <- paste("EY0", sprintf("%02d", 1), sep = "")

table %>% 
    map_df(function (x) {
        l <- fromJSONstat(readLines(paste(url, x, sep = "")))
        
        df <- l[[1]] %>% 
            as_data_frame()
        
       df %>% 
            write_csv(path = paste("output/", names(l), ".csv", sep = ""))
       
    })





