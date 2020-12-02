##
## Scrape names and locations of NHS England hospitals from NHS Choices website
##
library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(stringr)

##
## URLs
##
# url_trusts_list = "https://www.nhs.uk/servicedirectories/pages/nhstrustlisting.aspx"
url_trusts_list_acute = "https://www.nhs.uk/Services/Pages/AcuteTrustList.aspx?trustType=Acute"
url_trusts_list_other = "https://www.nhs.uk/Services/Pages/AcuteTrustList.aspx?trustType=HealthAndCare"

url_trusts = "https://www.nhs.uk/Services/Trusts/HospitalsAndClinics/DefaultView.aspx"

url_hospital = "https://www.nhs.uk/Services/hospitals/Overview/DefaultView.aspx"

# set user agent
ua = httr::user_agent("https://redcross.org.uk")

# a couple of helper functions just to make the code neater
extract_html = function(x) doc %>% 
  html_nodes(xpath = x)

extract_text = function(x) extract_html(x) %>% 
  html_text()


################################################################################################
## Download list of Trust IDs
##
scrape_trusts_list <- function(url) {
  # get webpage data
  req = httr::GET(url, ua)
  
  # load html
  out = content(req, "text")
  doc = read_html(out)
  
  # get list of links to Trusts' webpages
  trusts_list = doc %>%
    html_nodes(xpath="//*[contains(@class, 'o-listing')]//a/@href") %>%  # get href of any <a> tags within anything that has the class "trust-list"
    html_text()
  
  trusts_list = str_extract(trusts_list, "id=[0-9]+")  # extract the IDs for each Trust
  trusts_list = na.omit(trusts_list)                   # remove NAs (these are the 'return to top' links)
  trusts_list
}

trusts_list_acute <- scrape_trusts_list(url_trusts_list_acute)
trusts_list_other <- scrape_trusts_list(url_trusts_list_other)

trusts_list <- c(trusts_list_acute, trusts_list_other)

rm(trusts_list_acute, trusts_list_other)


################################################################################################
## Scrape hospital addresses and URLs for each Trust, as well as Trust reviews
##
# loop over each Trust, extracting hospital data
all_hospitals = tibble()
all_trusts = tibble()
i = 1

for (trust in trusts_list) {

  # get Trust webpage data
  req = httr::GET( modify_url( url_trusts, query = trust ), ua )
  
  # load html
  out = content(req, "text")
  doc = read_html(out)
  
  trust_name = extract_text(".//h1[@id = 'org-title']")
  
  trust_address1 = extract_text(".//span[@property = 'streetAddress']")
  trust_address2 = extract_text(".//span[@property = 'addressLocality']")
  trust_address3 = extract_text(".//span[@property = 'addressRegion']")
  trust_pc = extract_text(".//span[@property = 'postalCode']")
  trust_web = extract_text(".//a[@property = 'url']")
  
  trusts = tibble(Name = trust_name, Address1 = trust_address1, Address2 = trust_address2, Address3 = trust_address3, Postcode = trust_pc, URL = trust_web)
  
  ##
  ## get hospital details from this Trust
  ##
  hosp_names = extract_text(".//*[contains(@class, 'hospital-list')]//*//h3//a")
  hosp_urls = extract_text(".//*[contains(@class, 'hospital-list')]//*//h3//a/@href")
  hosp_address = as.character(extract_html(".//*[contains(@class, 'hospital-list')]//*//dd[@class = 'addrss']"))  # keep html for address
  
  hosp_address = hosp_address[1:length(hosp_names)]
  
  hospitals = tibble(Name = hosp_names, URL = hosp_urls, Address = hosp_address, Trust = trust_name)
  
  # tidy up addresses:
  hospitals = hospitals %>% 
    mutate(Address = str_replace_all(Address, "<br>", ", ")) %>%  # replace <br> with ", "
    mutate(Address = str_replace_all(Address, "<[^>]*>", "")) %>%     # remove other html tags
    mutate(Address = str_replace_all(Address, "\\n", ""))
  
  # extract postcodes
  # regular expression to match postcodes (allowing lowercase and unlimited spaces)
  # source: https://stackoverflow.com/a/7259020
  # see also: page 6 of https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/488478/Bulk_Data_Transfer_-_additional_validation_valid_from_12_November_2015.pdf
  postcode_regex = "(([gG][iI][rR] {0,}0[aA]{2})|((([a-pr-uwyzA-PR-UWYZ][a-hk-yA-HK-Y]?[0-9][0-9]?)|(([a-pr-uwyzA-PR-UWYZ][0-9][a-hjkstuwA-HJKSTUW])|([a-pr-uwyzA-PR-UWYZ][a-hk-yA-HK-Y][0-9][abehmnprv-yABEHMNPRV-Y]))) {0,}[0-9][abd-hjlnp-uw-zABD-HJLNP-UW-Z]{2}))"
  
  hospitals = hospitals %>% 
    mutate(Postcode = str_extract(Address, postcode_regex))
  
  # save to main dataframe
  all_trusts = bind_rows(all_trusts, trusts)
  all_hospitals = bind_rows(all_hospitals, hospitals)
  
  print(paste0("Finished ", i, " of ", length(trusts_list)))
  i = i + 1
  
  Sys.sleep(0.75)  # scrape responsibly with a pause between requests

}  # end for

# save
write_csv(all_hospitals, "England hospitals.csv")
write_csv(all_trusts, "England Trusts.csv")

# geocode the hospitals - ignore this step for now
# source("process hospitals.r")


################################################################################################
## Scrape hospital stats
##
# hospitals_list = str_extract(all_hospitals$URL, "id=[0-9]+")  # extract the IDs for each Trust

# grab hospital ID - get the last bunch of characters until the = sign
all_hospitals = all_hospitals %>% 
  mutate(ID = str_extract(URL, "[0-9]+"))

hospitals_list = all_hospitals$ID

# DEBUG: use this if you need to start the loop again from a specific hospital:
# hospitals_list = hospitals_list[886:length(hospitals_list)]

all_hospital_stats = data_frame()
i = 1

for (hospital in hospitals_list) {
  
  qry = paste0("id=", hospital)

  # get Trust webpage data
  req = httr::GET( modify_url( url_hospital, query = qry ), ua )
  
  # load html
  out = content(req, "text")
  doc = read_html(out)
  
  ##
  ## get hospital details
  ##
  # wrap in try-catch because some hospital profiles are hidden and cause errors (e.g. https://www.nhs.uk/Services/hospitals/Overview/DefaultView.aspx?id=114825)
  tryCatch({
    hosp_name = extract_text(".//h1[@id = 'org-title']")
    
    hosp_rating = as.numeric(extract_text(".//*[@property = 'v:rating']"))  # overall rating (stars out of five)
    
    if (is_empty(hosp_rating)) hosp_rating = NA  # set rating to NA if the hospital hasn't been rated yet
    
    hosp_ind_name = extract_text(".//*[@class = 'service-feedback clear']//div[@class = 'col one-sm']")       # get quality of service indicator names
    hosp_ind_val  = extract_text(".//*[@class = 'service-feedback clear']//div[@class = 'col one-sm last']")  # get quality of service indicator values
    
    # convert indicators into a dataframe and do some string manipulation/tidying
    hosp_inds = data_frame(Indicator = hosp_ind_name, Value = hosp_ind_val) %>% 
      mutate(Indicator = str_remove_all(Indicator, "[\\r\\n]*")) %>%   # trim to get rid of newlines and spaces
      mutate(Indicator = trimws(Indicator)) %>%   # remove whitespace
      mutate(Value = ifelse( str_detect(Value, "CQC"),
                             trimws( str_remove_all(Value, "[\\r\\n]*") ),  # extract CQC rating
                             str_extract(Value, "[0-9]+[%]*|CQC"))) %>%     # extract numbers (and percent signs) from indicator values
      mutate(Value = str_remove(Value, "Visit CQC profile"))  # get rid of extraneous CQC text
    
    this_hospital = data_frame(Hospital = hosp_name, ID = hospital, Rating = hosp_rating)
    
    # add quality of service indicators for this hospital
    this_hospital = this_hospital %>% 
      bind_cols(
        hosp_inds %>% 
          spread(Indicator, Value)  # spread so there's only one entry for this hospital
      )
    
    # save to main dataframe
    all_hospital_stats = bind_rows(all_hospital_stats, this_hospital)
    
    print(paste0("Finished ", i, " of ", length(hospitals_list)))
  
  },
  error = function(e) {
    print(paste0("Error in ", i, " (hospital ", hospital, ")"))
  })
  
  i = i + 1
  
  Sys.sleep(0.75)  # scrape responsibly with a pause between requests

}  # end for

# merge stats into all_hospitals
all_hospitals = all_hospitals %>% 
  left_join(all_hospital_stats, by = "ID")

# clean up data
all_hospitals = all_hospitals %>%
  mutate(URL = paste0("https://www.nhs.uk", URL)) %>%  # make full URL
  distinct()  # remove duplicates

# save
write_csv(all_hospitals, "England hospitals stats.csv")
