library(tidyverse)
library(ggthemes)
library(tidycensus)
library(ggrepel)
library(sf)
library(scales)

# assumes you've created `precincts` spatial df (running precinct-geo.R) and `electionResultsDf` df (running CleanResults.R)

counties <- st_read('/opt/data/shapefiles/cb_county//') %>% st_transform(crs=st_crs(precincts)) %>% filter(STATEFP==53)
places <- get_acs(
  geography = "place",
  state = 53,
  variables = "B01003_001",  # Total population
  year = 2022,
  survey = "acs5",
  geometry = TRUE,
  keep_geo_vars = TRUE
) %>% select(Place=NAME.x, Population=estimate) %>%
  st_transform(crs=st_crs(precincts)) %>%
  st_centroid() %>%
  st_join(counties %>% select(County=NAME, CountyFIPS=GEOID)) %>% group_by(County) %>% slice_max(Population, n=3)

candidate <- 'Reykdal'

ospiDf <- generalResultsDf %>%
  filter(grepl(x=Race, pattern='Superintendent')) %>%
  group_by(FIPS, PrecinctCode) %>%
  mutate(Ballots=sum(Votes)) %>%
  filter(Ballots > 0) %>%
  mutate(VoteShare=Votes/Ballots)

candidateDf <- ospiDf %>%
  filter(grepl(x=Candidate, pattern=candidate))

maxVoteShare <- max(candidateDf$VoteShare)
minVoteShare <- min(candidateDf$VoteShare)
medianVoteShare <- median(candidateDf$VoteShare)
meanVoteShare <- mean(candidateDf$VoteShare)

analysisCounties <- c('Garfield')

precincts %>% filter(CountyName %in% analysisCounties) %>% left_join(candidateDf %>% select(FIPS, Precinct=PrecinctCode, VoteShare)) %>%
  ggplot() +
  geom_sf(mapping=aes(fill=VoteShare)) +
  scale_fill_gradient2(low="#d8b365", high="#5ab4ac", guide="colorbar", na.value="grey90", labels=label_percent(), midpoint=medianVoteShare, limits=c(minVoteShare, maxVoteShare)) +
  #geom_sf(data=places %>% filter(County %in% analysisCounties), size=.5) +
  #geom_text_repel(data=places %>% filter(County %in% analysisCounties), aes(label=Place, geometry=geometry), stat='sf_coordinates', size=3) +
  labs(fill=NULL) +
  guides(fill=guide_colorbar(position='bottom')) +
  labs(title=paste0(candidate, ' share of vote in 2024 General Election'),
       subtitle=paste0(paste0(sort(analysisCounties), collapse=', '), ' ', if_else(length(analysisCounties) > 1, 'Counties', 'County')),
       caption=paste0(
          paste0('Color gradient scaled so that neutral/white represents ', candidate, "'s statewide median vote share of ", percent(medianVoteShare, .01)),
          '\nGrey-shaded precincts had no results reported, or results were combined with nearby precincts by SOS Elections to protect voter privacy'
         ),
       ) +
  theme_map() +
  theme(legend.justification.bottom = 'center', legend.key.width = unit(50, "points"))
