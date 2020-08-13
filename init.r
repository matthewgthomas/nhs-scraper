data.dir   = "../../../Data/"
# data.dir = "P:/Operations/Innovation & Insight/Insight/Data science/Data"

load_postcodes = function() {
  read_csv(file.path(data.dir, "Postcodes", "National_Statistics_Postcode_Lookup - BRC.csv"),
           col_types = cols(
             Postcode = col_character(),
             Longitude = col_double(),
             Latitude = col_double(),
             Country = col_character(),
             CountryName = col_character(),
             `Output Area` = col_character(),
             LSOA = col_character(),
             `Local Authority Code` = col_character(),
             `Primary Care Trust` = col_character(),
             `Rural or Urban?` = col_character(),
             `Rural Urban classification` = col_character(),
             IMD = col_integer(),
             IMD.Decile = col_integer(),
             IMD.Decile.Name = col_character(),
             `Rurality index` = col_double()
           ))
}