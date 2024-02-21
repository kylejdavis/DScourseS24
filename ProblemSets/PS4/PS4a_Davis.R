
library(tidyverse)
library(jsonlite)

system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')

json_url <- "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"
download.file(json_url, destfile = "dates.json", mode = "wb")


mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])
class(mydf)
class(mydf$date)
head(mydf)
