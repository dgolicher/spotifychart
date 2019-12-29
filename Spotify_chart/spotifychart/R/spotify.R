#' Scrapes spotify chart data
#'
#' @param start Defaults to "2019/01/03"
#' @param end  defaults to "2019/12/19"
#' @param url defaults to "https://spotifycharts.com/regional/gb/weekly/latest/"
#'
#' @return
#' @export
#'
#' @examples ## d<-spotify_chart()
#' 
#' 
#' 
spotify_chart<-function(start= "2018/10/26", 
                        end = Sys.Date(), 
                        url ="https://spotifycharts.com/regional/gb/weekly/"){
  # Code from https://rpubs.com/argdata/web_scraping
  require(rvest)#Easily Harvest (Scrape) Web Pages. html_nodes
  require(tidyverse)#Designed to make it easy to install and load multiple 'tidyverse' packages in a single step.
  require(magrittr)#A Forward-Pipe Operator for R
  require(scales)
  require(knitr)
  require(lubridate)#Lubridate provides tools that make it easier to parse and manipulate dates.
  require(ggrepel)
  
#start= "2018/10/29"
#end = "2019/12/19"
#url ="https://spotifycharts.com/regional/gb/weekly/"
library(lubridate)
start<-as.Date(start)
end<- as.Date(end)
start<-start + (6 - wday(start))
end <- end +(5-wday(end))

week_start <- seq(start, (end-7), by = "week")
week_end <- seq((start + 7) , end, by = "week")
 
  timevalues <- paste(week_start,week_end, sep="--")
  concat.url<- function(x){
  full_url <- paste0(url, x)
  full_url
}
#Run the function
finalurl <- concat.url(timevalues)
finalurl

SpotifyScrape <- function(x){
  page <- x
  rank <- page %>%
    read_html() %>% #Reads an HTML page
    html_nodes('.chart-table-position') %>% #RVEST.PKG: extract pieces out of HTML docs. using XPath & css selectors.
    html_text() %>% #RVEST.PKG:Extract attributes, text and tag name from html
    as.data.frame()
  track <- page %>% 
    read_html() %>% 
    html_nodes('strong') %>% 
    html_text() %>% 
    as.data.frame()
  artist <- page %>% 
    read_html() %>% 
    html_nodes('.chart-table-track span') %>% 
    html_text() %>% 
    as.data.frame()
  streams <- page %>% 
    read_html() %>% 
    html_nodes('td.chart-table-streams') %>% 
    html_text() %>% 
    as.data.frame()
  dates <- page %>% 
    read_html() %>% 
    html_nodes('.responsive-select~ .responsive-select+ .responsive-select .responsive-select-value') %>%
    html_text() %>% 
    as.data.frame()
  
  #combine, name, and make it a tibble
  chart <- cbind(rank, track, artist, streams, dates) #Combine R Objects by Columns
  names(chart) <- c("Rank", "Track", "Artist", "Streams", "Date") #Functions to get or set the names of an object
  chart <- as.tibble(chart)#TIBBLE.PKG:turns an existing object into a so-called tibble
  
 
  return(chart) #Final tibble 5 columns & (200 rows * 365 days) = 73,000
}
spotify <- map_df(finalurl, SpotifyScrape) 
spotify %<>% 
  mutate( Artist = gsub("by ", "", Artist), #gsub perform replacement of the first and all matches respectively
          Streams = gsub(",", "", Streams), 
          Streams = as.numeric(Streams), 
          Date = as.Date(spotify$Date, "%m/%d/%Y"),
          WeekDay = wday(Date, label = TRUE),#LUBRIDATE.PKG:Get days component of a date-time
          Month = month(Date, label = TRUE)
  ) -> spotify



spotify
}


get_token<-function() {
library(spotifyr)
library(httr)
client_id <- 'a749814f307b45fc9144e5ab6a0436e1'
client_secret <- '7beb57b32692463589c0491d9e35f60d'
access_token <- POST('https://accounts.spotify.com/api/token',
                     accept_json(), authenticate(client_id, client_secret),
                     body = list(grant_type='client_credentials'),
                     encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token
access_token
}


