##
## Scrape NHS Trust names and codes from NHS Digital's ODS Portal: https://odsportal.hscic.gov.uk/Organisation/Search
##
library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(stringr)

url_trust_codes <- "https://odsportal.hscic.gov.uk/Organisation/Search?Type=TR&Name=Trust&FromPOST=True"

# set user agent
ua = httr::user_agent("https://redcross.org.uk")

# get webpage data
req = httr::GET(url_trust_codes, ua)

# load html
out = content(req, "text")
doc = read_html(out)

# get Trust codes, names and open/closed status
trust_codes <- doc %>% 
  html_nodes(xpath="//*[contains(@class, 'search-result-row')]//a/div[@class = 'hidden-xs col-sm-3 col-md-2']") %>% 
  html_text() %>% 
  str_trim()

trust_names <- doc %>% 
  html_nodes(xpath="//*[contains(@class, 'search-result-row')]//a/div[@class = 'col-xs-6 col-sm-5 col-md-4']") %>% 
  html_text() %>% 
  str_trim()

trust_status <- doc %>% 
  html_nodes(xpath="//*[contains(@class, 'search-result-row')]//a/div[@class = 'col-xs-3 col-sm-1']") %>% 
  html_text() %>% 
  str_trim()

trusts <- tibble(Code = trust_codes, Name = trust_names, Status = trust_status)

write_csv(trusts, "England Trusts names and codes.csv")
