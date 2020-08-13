##
## Process hospitals
## - geocode based on postcode
## - look up IL/CR area
## - look up CCG
##
library(tidyverse)
library(rgdal)

eng_hospitals = read_csv("England hospitals.csv")

if (!exists("postcodes")) postcodes = load_postcodes()

# the ONS data truncates 7-character postcodes to remove spaces (e.g. CM99 1AB --> CM991AB); get rid of all spaces in both datasets to allow merging
postcodes$Postcode2 = gsub(" ", "", postcodes$Postcode)
eng_hospitals$Postcode2 = gsub(" ", "", eng_hospitals$Postcode)

# merge
eng_hospitals = eng_hospitals %>% 
  left_join(postcodes %>% select(Postcode2, Longitude, Latitude), by="Postcode2")

eng_hospitals$Postcode2 = NULL  # don't need the truncated column anymore
eng_hospitals$Postcode.y = NULL
eng_hospitals = rename(eng_hospitals, Postcode = Postcode.x)

##
## look up IL areas associated with these Trusts
##
# load BRC area boundaries
brc_areas = readOGR(dsn = file.path(data.dir, "Boundaries", "BRC_Areas_2017", "IL CR"), 
                    layer = "IL_CR_boundaries_20161006_v2", verbose = F)

# make spatial points object for Trusts...
eng_hospitals.sp = SpatialPointsDataFrame(subset(eng_hospitals, select = c(Longitude, Latitude)), eng_hospitals,
                                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# get IL areas that each Trust belongs to
eng_hospitals.areas = eng_hospitals.sp %over% brc_areas

# merge IL areas into Trusts dataframe
eng_hospitals = bind_cols(eng_hospitals,
                          eng_hospitals.areas %>% select(`IL Area` = name))

# for some reason, three of the England Trusts are in Wales - remove them
# eng_hospitals = eng_hospitals %>% 
#   filter(`IL Area` != "Wales")

# save
write_csv(eng_hospitals, "../../../Data/NHS/England/England hospitals.csv")

