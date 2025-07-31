library(tidycensus)
library(tidyverse)
library(sf)

# assumes `precincts` spatial df has been created in precinct-geo.R

tracts <- get_acs(
  geography = "tract",
  state = 53,
  year = 2022,
  geometry = TRUE,
  keep_geo_vars = TRUE,
  variables = 'B01003_001' # don't actually use this, but have to ask for something from tidycensus
) %>% select(STATEFP, COUNTYFP, GEOID, Population=estimate) %>% st_transform(crs=st_crs(precincts)) %>%
  mutate(area=st_area(geometry))

# intersecting county by county, then row-binding, goes about 1000x faster than intersecting the whole state at once
# this also avoids problems with geometry imprecision/errors, which cause little slices of tracts from adjacent counties to be included in border precincts (we
# know that neither precincts nor tracts can cross county boundaries)

precinctTractOverlap <- precincts %>% st_drop_geometry() %>%
  pull(FIPS) %>% unique() %>% map(function(eachFips) {
    print(paste0('Processing FIPS ', eachFips))
    precincts %>% filter(FIPS==eachFips) %>%
      mutate(precinctArea=st_area(geometry)) %>%
      st_intersection(tracts %>% mutate(FIPS=paste0(STATEFP, COUNTYFP)) %>% filter(FIPS==eachFips)) %>% st_make_valid() %>% mutate(overlapArea=st_area(geometry)) %>%
      select(FIPS, Precinct, GEOID, precinctArea, overlapArea) %>% st_drop_geometry()
  }) %>% bind_rows() %>% as_tibble()

precinctTractOverlap <- precinctTractOverlap %>% group_by(FIPS, Precinct) %>% mutate(weight=overlapArea/sum(overlapArea), weight=as.double(weight))
