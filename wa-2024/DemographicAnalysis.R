library(tidyverse)
library(tidycensus)

acsDf <- get_acs(
  geography = "tract",
  variables = c(
    'B01003_001', # total pop
    paste0('B01001_00', 3:6), # male under 18
    paste0('B01001_0', 27:30), # female under 18
    'B19013_001', # median hh income
    'B25003_002', # owner-occupied housing units
    'B25003_001', # total housing units,
    'B15003_001', # education denom
    paste0("B15003_0", str_pad(2:18, 2, 'left', '0')) # ed levels thru high school
  ),
  state = "WA",
  year = 2022,
  survey = "acs5",
  geometry = FALSE
) %>% select(-moe) %>% pivot_wider(names_from=variable, values_from=estimate) %>%
  rowwise() %>%
  mutate(
    u18 = sum(c_across(starts_with("B01001_0")), na.rm = TRUE),
    hs = sum(c_across(matches("B15003_0((0[2-9])|(1[0-9]))")), na.rm = TRUE),
    pctHighSchool=hs/B15003_001 * 100,
    pctUnder18 = u18 / B01003_001 * 100,
    pctOwnerOccupied = B25003_002 / B25003_001 * 100
  )  %>%
  select(GEOID, population=B01003_001, medianIncome=B19013_001, housingUnits=B25003_001, educationTotal=B15003_001, u18, hs, ownerOccupiedHousingUnits=B25003_002)

precinctAcsDf <- precinctTractOverlap %>% left_join(acsDf, by='GEOID') %>%
  group_by(FIPS, Precinct) %>%
  mutate(
    precinctArea=as.double(precinctArea)/2589988.11,
    population=population*weight,
    medianIncome=medianIncome*weight,
    housingUnits=housingUnits*weight,
    educationTotal=educationTotal*weight,
    u18=u18*weight,
    hs=hs*weight,
    ownerOccupiedHousingUnits=ownerOccupiedHousingUnits*weight
  ) %>%
  summarise(
    population=sum(population),
    housingUnits=sum(housingUnits),
    ownerOccupiedHousingUnits=sum(ownerOccupiedHousingUnits),
    hs=sum(hs),
    u18=sum(u18),
    educationTotal=sum(educationTotal),
    medianIncome=mean(medianIncome),
    precinctArea=min(precinctArea)
  ) %>%
  mutate(
    pctUnder18=u18/population,
    pctHighSchool=hs/educationTotal,
    pctOwnerOccupied=ownerOccupiedHousingUnits/housingUnits
  )

ospiDf <- electionResultsDf %>%
  filter(grepl(x=Race, pattern='Superintendent')) %>%
  mutate(PrecinctCode=as.integer(PrecinctCode)) %>%
  group_by(FIPS, PrecinctCode, Candidate) %>%
  summarize(Votes=sum(Votes)) %>%
  group_by(FIPS, PrecinctCode) %>%
  mutate(Ballots=sum(Votes)) %>%
  filter(Ballots > 0) %>%
  mutate(VoteShare=Votes/Ballots) %>%
  filter(grepl(x=Candidate, pattern='Reykdal')) %>% select(-Candidate)

governorDf <- electionResultsDf %>%
  filter(grepl(x=Race, pattern='Governor|State of Washington Governor')) %>%
  mutate(PrecinctCode=as.integer(PrecinctCode)) %>%
  group_by(FIPS, PrecinctCode, Candidate) %>%
  summarize(Votes=sum(Votes)) %>%
  group_by(FIPS, PrecinctCode) %>%
  mutate(Ballots=sum(Votes)) %>%
  filter(Ballots > 0) %>%
  mutate(VoteShare=Votes/Ballots) %>%
  filter(grepl(x=Candidate, pattern='Ferguson')) %>% select(-Candidate)

regressionDf <- ospiDf %>% select(FIPS, Precinct=PrecinctCode, Votes, VoteShare) %>%
  inner_join(governorDf %>% select(FIPS, Precinct=PrecinctCode, FergusonShare=VoteShare), by=c('FIPS','Precinct')) %>%
  inner_join(precinctAcsDf, by=c('FIPS','Precinct')) %>%
  mutate(voterDensity=Votes/precinctArea) %>% ungroup() %>% mutate(idx=row_number())
  
model <- lm(VoteShare ~ pctUnder18 + pctHighSchool + pctOwnerOccupied + voterDensity + medianIncome + FergusonShare, data = regressionDf)
summary(model)
predicted <- predict(model)

predicted <- tibble(predicted=predicted, idx=names(predicted)) %>% mutate(idx=as.integer(idx))
regressionDf <- regressionDf %>% inner_join(predicted, by='idx') %>% mutate(overperformance=VoteShare-predicted)

analysisCounties <- c('Thurston','Mason','Kitsap','Pierce')

precincts %>%
  left_join(regressionDf, by=c('FIPS', 'Precinct')) %>% filter(CountyName %in% analysisCounties) %>%
  ggplot() + geom_sf(aes(fill=overperformance)) +
  scale_fill_gradient2(low="#d8b365", high="#5ab4ac", guide="colorbar", na.value="grey90",
                       midpoint=0, limits=c(min(regressionDf$overperformance), max(regressionDf$overperformance))) +
  theme_map() +
  labs(fill=NULL) +
  guides(fill=guide_colorbar(position='bottom')) +
  labs(title=paste0('Reykdal relative performance in August 2024 Primary'),
       subtitle=paste0(paste0(sort(analysisCounties), collapse=', '), ' ', if_else(length(analysisCounties) > 1, 'Counties', 'County')),
       caption=paste0(
         paste0('Color gradient scaled so that neutral/white represents predicted performance'),
         '\nGrey-shaded precincts had no results reported, or results were combined with nearby precincts by SOS Elections to protect voter privacy'
       ),
  ) +
  theme_map() +
  theme(legend.justification.bottom = 'center', legend.key.width = unit(50, "points"))



