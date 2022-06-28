#### Geocode unmatched (not previously geocoded) geokeys ####
#### Jordan Klein
#### Load package
library(mapsapi)

#### Geocode using google maps api 
#*this wont run without manually setting the api key 
Geocoded <- mp_geocode(To.geocode$location_to_geocode, key = key)

