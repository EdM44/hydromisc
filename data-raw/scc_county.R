library(tigris)
library(sf)

ca_counties <- tigris::counties("CA")
scc_county <- ca_counties[ca_counties$NAME == "Santa Clara",] |> sf::st_transform(4326)

usethis::use_data(scc_county, overwrite = TRUE)
