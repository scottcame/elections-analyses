library(tidyverse)
library(sf)
library(tidycensus)
library(tidyjson)

makePrecincts2023 <- function() {
  
  # this spatial dataset cleanup was necessary before the SOS assembled a shapefile for the 2024 general. no longer needed, but keeping here as documentation of what was done for the primary.
  
  awRaw <- list.files('/opt/data/shapefiles/tl_areawater/', full.names = TRUE) %>% map(st_read) %>% bind_rows()
  aw <- awRaw %>% filter((is.na(FULLNAME) & AWATER > 5000000) | grepl(x=FULLNAME,
                                                                      pattern='Sea|Inlt|Cv|Columbia Riv|Snake Riv|Ocean|Strait|Umatilla|Willapa|Hbr|Bay|Sopun|Beardslee|Mallard Slough|Long Island Slough|Naselle'))
  
  precinctsRaw <- st_read('/opt/data/elections/wa_precincts_2023/sos_statewide/') %>%
    st_transform(crs=st_crs(aw)) %>%
    select(FIPS, CountyName, Precinct, PrecName) %>%
    filter(!(CountyName %in% c('Thurston', 'Whatcom', 'Lewis', 'Clark', 'Kitsap', 'Klickitat', 'Yakima', 'Pierce', 'Snohomish', 'King'))) %>% mutate(OriginalPrecinct=Precinct)
  
  thurstonPrecinctsRaw <- st_read('/opt/data/elections/wa_precincts_2023/thurston/') %>%
    st_transform(crs=st_crs(aw)) %>% select(Precinct=PrecinctNu, PrecName=Name) %>%
    mutate(FIPS=53067, CountyName='Thurston', Precinct=as.double(Precinct)) %>%
    mutate(OriginalPrecinct=Precinct)
  
  whatcomPrecinctsRaw <- st_read('/opt/data/elections/wa_precincts_2023/whatcom/') %>%
    st_transform(crs=st_crs(aw)) %>% select(Precinct) %>%
    mutate(OriginalPrecinct=Precinct, Precinct=as.integer(Precinct)) %>%
    mutate(FIPS=53073, CountyName='Whatcom', PrecName=paste0('Whatcom #', as.character(OriginalPrecinct)))
  
  lewisPrecinctsRaw <- st_read('/opt/data/elections/wa_precincts_2023/lewis/') %>%
    st_transform(crs=st_crs(aw)) %>% select(Precinct=PRECINCT_N, PrecName=PRECINCT) %>%
    mutate(FIPS=53041, CountyName='Lewis') %>%
    mutate(OriginalPrecinct=Precinct)
  
  clarkPrecinctsRaw <- st_read('/opt/data/elections/wa_precincts_2023/clark/') %>%
    st_transform(crs=st_crs(aw)) %>% select(Precinct) %>%
    mutate(OriginalPrecinct=Precinct, Precinct=as.integer(Precinct)) %>%
    mutate(FIPS=53011, CountyName='Clark', PrecName=paste0('Clark #', as.character(OriginalPrecinct)))
  
  kitsapPrecinctsRaw <- st_read('/opt/data/elections/wa_precincts_2023/kitsap/') %>%
    st_transform(crs=st_crs(aw)) %>% select(Precinct=DISTRICT, PrecName=DESCRIPTIO) %>%
    mutate(Precinct=as.integer(Precinct), OriginalPrecinct=Precinct) %>%
    mutate(FIPS=53035, CountyName='Kitsap')
  
  # https://geo.gartrellgroup.com/server/rest/services/Klickitat/Layers/MapServer/27/query?outSR=4326&f=json&where=1%3D1&outFields=*&returnGeometry=true&geometryPrecision=7
  klikitatPrecinctsRaw <- st_read('/opt/data/elections/wa_precincts_2023/klickitat/precincts.esri.json') %>%
    st_transform(crs=st_crs(aw)) %>% select(Precinct=PRECINCT) %>%
    mutate(Precinct=as.integer(Precinct), OriginalPrecinct=Precinct, PrecName=paste0('Klickitat #', Precinct)) %>%
    mutate(FIPS=53039, CountyName='Klickitat')
  
  yakimaPrecinctsRaw <- st_read('/opt/data/elections/wa_precincts_2023/yakima/') %>%
    st_transform(crs=st_crs(aw)) %>% mutate(precinct_s=as.double(precinct_s)) %>% select(Precinct=CODE, OriginalPrecinct=precinct_s) %>%
    mutate(Precinct=as.integer(Precinct), PrecName=paste0('Yakima #', Precinct)) %>%
    mutate(FIPS=53077, CountyName='Yakima')
  
  piercePrecinctsRaw <- st_read('/opt/data/elections/wa_precincts_2023/pierce/') %>%
    st_transform(crs=st_crs(aw)) %>% select(PrecName=Precinct) %>%
    mutate(Precinct=as.integer(PrecName), OriginalPrecinct=Precinct, PrecName=gsub(x=PrecName, pattern='([0-9]{2})([0-9]{3})', replacement='\\1-\\2')) %>%
    mutate(FIPS=53053, CountyName='Pierce')
  
  snohomishPrecinctsRaw <- st_read('/opt/data/elections/wa_precincts_2023/snohomish/') %>%
    st_transform(crs=st_crs(aw)) %>% select(PrecName=Name, Precinct) %>%
    mutate(OriginalPrecinct=as.integer(Precinct), Precinct=as.double(str_sub(Precinct, 5))) %>%
    mutate(FIPS=53061, CountyName='Snohomish')
  
  kingPrecinctsRaw <- st_read('/opt/data/elections/wa_precincts_2023/king/') %>%
    st_transform(crs=st_crs(aw)) %>%
    select(Precinct=votdst, PrecName=NAME) %>%
    mutate(Precinct=as.integer(Precinct), OriginalPrecinct=Precinct) %>%
    mutate(FIPS=53033, CountyName='King')
  
  precincts <- precinctsRaw %>%
    bind_rows(thurstonPrecinctsRaw, whatcomPrecinctsRaw, lewisPrecinctsRaw, clarkPrecinctsRaw, kitsapPrecinctsRaw, klikitatPrecinctsRaw, yakimaPrecinctsRaw, piercePrecinctsRaw, snohomishPrecinctsRaw, kingPrecinctsRaw) %>%
    st_make_valid() %>% st_simplify(dTolerance = 25) %>%
    st_difference(st_union(aw))
  
  clipOcean <- function(coords, sdf, crs) {
    clp <- matrix(coords, ncol = 2, byrow = TRUE)
    clp <- st_polygon(list(clp))
    clp <- st_sf(geometry=st_sfc(clp, crs = crs))
    sdf %>% st_difference(clp)
  }
  
  # extra stuff in Juan de Fuca
  precincts <- clipOcean(c(
    -124.2, 48.4,
    -123.1, 48.4,
    -123.1, 48.3,
    -123.6, 48.2,
    -124.2, 48.3,
    -124.2, 48.4
  ), precincts, st_crs(aw))
  
  # ocean
  precincts <- clipOcean(c(
    -124.13, 46.6,
    -124.35, 47.6,
    -124.5, 47.6,
    -124.5, 46.6,
    -124.13, 46.6
  ), precincts, st_crs(aw))
  
  # ocean
  precincts <- clipOcean(c(
    -124.62, 47.7,
    -124.8, 48.05,
    -124.9, 48.05,
    -124.9, 47.7,
    -124.62, 47.7
  ), precincts, st_crs(aw))
  
  # ocean
  precincts <- clipOcean(c(
    -124.76, 48.0,
    -124.76, 48.45,
    -124.95, 48.45,
    -124.95, 48.0,
    -124.76, 48.0
  ), precincts, st_crs(aw))
  
  # ocean
  precincts <- clipOcean(c(
    -124.5, 48.4,
    -124.5, 48.5,
    -124.95, 48.5,
    -124.95, 48.4,
    -124.5, 48.4
  ), precincts, st_crs(aw))
  
  # ocean
  precincts <- clipOcean(c(
    -124.12, 46.22,
    -124.12, 46.61,
    -124.2, 46.61,
    -124.2, 46.22,
    -124.12, 46.22
  ), precincts, st_crs(aw))
  
  precincts
  
}

makePrecincts2024 <- function() {
  
  awRaw <- list.files('/opt/data/shapefiles/tl_areawater/', full.names = TRUE) %>% map(st_read) %>% bind_rows()
  aw <- awRaw %>% filter((is.na(FULLNAME) & AWATER > 5000000) | grepl(x=FULLNAME,
                                                                      pattern='Sea|Inlt|Cv|Columbia Riv|Snake Riv|Ocean|Strait|Umatilla|Willapa|Hbr|Bay|Sopun|Beardslee|Mallard Slough|Long Island Slough|Naselle'))
  
  precinctsRaw <- st_read('/opt/data/elections/wa_precincts_2024/sos_statewide/') %>%
    st_transform(crs=st_crs(aw)) %>%
    select(FIPS=County, CountyName, Precinct=PrecinctNu, PrecName=PrecinctNa, LegDistrict=LEG) %>%
    mutate(Precinct=as.character(Precinct)) %>%
    # snohomish county weirdness...last 4 characters is the precinct code
    mutate(Precinct=if_else(FIPS=='53061', str_sub(Precinct, 5), Precinct)) %>%
    mutate(FIPS=as.integer(FIPS), Precinct=as.integer(Precinct))
  
  precincts <- precinctsRaw %>%
    st_make_valid() %>% st_simplify(dTolerance = 25) %>%
    st_difference(st_union(aw))
  
  clipOcean <- function(coords, sdf, crs) {
    clp <- matrix(coords, ncol = 2, byrow = TRUE)
    clp <- st_polygon(list(clp))
    clp <- st_sf(geometry=st_sfc(clp, crs = crs))
    sdf %>% st_difference(clp)
  }
  
  # extra stuff in Juan de Fuca
  precincts <- clipOcean(c(
    -124.2, 48.4,
    -123.1, 48.4,
    -123.1, 48.3,
    -123.6, 48.2,
    -124.2, 48.3,
    -124.2, 48.4
  ), precincts, st_crs(aw))
  
  # ocean
  precincts <- clipOcean(c(
    -124.13, 46.6,
    -124.35, 47.6,
    -124.5, 47.6,
    -124.5, 46.6,
    -124.13, 46.6
  ), precincts, st_crs(aw))
  
  # ocean
  precincts <- clipOcean(c(
    -124.62, 47.7,
    -124.8, 48.05,
    -124.9, 48.05,
    -124.9, 47.7,
    -124.62, 47.7
  ), precincts, st_crs(aw))
  
  # ocean
  precincts <- clipOcean(c(
    -124.76, 48.0,
    -124.76, 48.45,
    -124.95, 48.45,
    -124.95, 48.0,
    -124.76, 48.0
  ), precincts, st_crs(aw))
  
  # ocean
  precincts <- clipOcean(c(
    -124.5, 48.4,
    -124.5, 48.5,
    -124.95, 48.5,
    -124.95, 48.4,
    -124.5, 48.4
  ), precincts, st_crs(aw))
  
  # ocean
  precincts <- clipOcean(c(
    -124.12, 46.22,
    -124.12, 46.61,
    -124.2, 46.61,
    -124.2, 46.22,
    -124.12, 46.22
  ), precincts, st_crs(aw))
  
  precincts %>% mutate(precinctArea=st_area(geometry))
  
}

precincts2024 <- makePrecincts2024()

# we won't make pretty maps from the 2020 precincts, so we don't need to do all the other cleanup
# note that the 2020 shapefile from SOS does not contain legislative district, so we'll need to infer that from results

precincts2020 <- st_read('/opt/data/elections/wa_precincts_2020/') %>%
  select(FIPS=County, CountyName, Precinct=PrecCode, PrecName) %>%
  mutate(Precinct=as.character(Precinct)) %>%
  # snohomish county weirdness...last 4 characters is the precinct code
  mutate(Precinct=if_else(FIPS=='53061', str_sub(Precinct, 5), Precinct)) %>%
  mutate(FIPS=as.integer(FIPS), Precinct=as.integer(Precinct)) %>%
  # random other precincts that didn't line up with results
  mutate(
    Precinct=case_when(
      FIPS=='53067' ~ case_when(
        Precinct==433 ~ 802,
        Precinct==26 ~ 803,
        Precinct==25 ~ 804,
        Precinct==354 ~ 805,
        .default = Precinct
      ),
      FIPS=='53035' ~ case_when(
        Precinct==166 ~ 1804,
        .default = Precinct
      ),
      FIPS=='53037' ~ case_when(
        Precinct==65 ~ 1900,
        .default = Precinct
      ),
      FIPS=='53073' ~ case_when(
        Precinct==702 ~ 3708,
        .default = Precinct
      ),
      .default = Precinct
    )
  )


