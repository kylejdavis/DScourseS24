library(rvest)
library(tidyverse)

h <- read_html("https://en.wikipedia.org/wiki/List_of_Major_League_Baseball_career_stolen_bases_leaders")

reps <- h %>%
  html_node("#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(11) > tbody") %>%
  html_table()

url <- "https://en.wikipedia.org/wiki/List_of_Major_League_Baseball_career_stolen_bases_leaders"

css_selector <- "#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(11) > tbody"
webpage <- read_html(url)

table <- webpage %>%
  html_nodes(css_selector) %>%
  html_table()

df <- table[[1]]

print(df)
view(df)


##

install.packages(c("jsonlite", "httr"))

library(jsonlite)
library(httr)
library(tidyverse)

generate_table <- function(symbol) {
  API_KEY <- "ALPHA_VANTAGE_API_KEY"
  url <- paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=", symbol, "&apikey=", API_KEY, "&datatype=json")
  response <- GET(url)
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  return(data$`Time Series (Daily)`)
}

table <- generate_table("GOOGL")
print(table)

##PLOTTING DATA
# Flatten the data
df <- table %>%
  enframe(name = "date", value = "values") %>%
  unnest_wider(values) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(date = as.Date(date))


#

# Check for NA or Inf values in the 'date' and '4. close' columns
na_date <- sum(is.na(df$date))
na_close <- sum(is.na(df$`4. close`))
inf_date <- sum(is.infinite(df$date))
inf_close <- sum(is.infinite(df$`4. close`))

print(paste("NA values in 'date':", na_date))
print(paste("NA values in '4. close':", na_close))
print(paste("Infinite values in 'date':", inf_date))
print(paste("Infinite values in '4. close':", inf_close))

# If there are NA or Inf values, you might want to remove or replace them before plotting
df <- df[!is.na(df$date) & !is.na(df$`4. close`), ]
df <- df[!is.infinite(df$date) & !is.infinite(df$`4. close`), ]

# Now you can plot the data
ggplot(data = df, aes(x = date, y = `4. close`)) +
  geom_line() +
  labs(x = "Date", y = "Close Price", title = "Closing Prices Over Time")



