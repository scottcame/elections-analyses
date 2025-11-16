library(tidyverse)
library(sf)

# assumes tibble `precincts` is created from precinct-geo.R

countyCodes <- tribble(
  ~CountyCode,~FIPS,
  'AD',53001,
  'AS',53003,
  'BE',53005,
  'CH',53007,
  'CM',53009,
  'CR',53011,
  'CU',53013,
  'CZ',53015,
  'DG',53017,
  'FE',53019,
  'FR',53021,
  'GA',53023,
  'GR',53025,
  'GY',53027,
  'IS',53029,
  'JE',53031,
  'KI',53033,
  'KP',53035,
  'KS',53037,
  'KT',53039,
  'LE',53041,
  'LI',53043,
  'MA',53045,
  'OK',53047,
  'PA',53049,
  'PE',53051,
  'PI',53053,
  'SJ',53055,
  'SK',53057,
  'SM',53059,
  'SN',53061,
  'SP',53063,
  'ST',53065,
  'TH',53067,
  'WK',53069,
  'WL',53071,
  'WM',53073,
  'WT',53075,
  'YA',53077
)

cleanResults2024 <- function(precincts, statewideFile, kingFile) {
  
  nonKing <- read_csv(statewideFile, col_types = 'ccccii') %>%
    filter(PrecinctCode != -1) %>% inner_join(countyCodes, by='CountyCode') %>% mutate(FIPS=as.integer(FIPS)) %>%
    filter(FIPS != 53033) %>%
    select(-CountyCode) %>%
    mutate(StarFlag=grepl(x=PrecinctName, pattern='.+\\*\\)$'), PrecinctName=gsub(x=PrecinctName, pattern='(.+)(?:\\(\\*\\))', replacement='\\1')) %>%
    filter(!(FIPS==53077 & PrecinctCode==1)) # weird yakima county courthouse "precinct" with no votes
  
  nonKing <- nonKing %>% filter(FIPS != 53011) %>% bind_rows(
    # Clark uses the name field
    nonKing %>% filter(FIPS==53011) %>% mutate(PrecinctCode=as.double(PrecinctName))
  )
  
  king <- read_csv(kingFile) %>% mutate(FIPS=53033) %>%
    select(Race, PrecinctName=Precinct, Candidate=CounterType, Votes=SumOfCount, FIPS) %>%
    filter(!(PrecinctName %in% c('WESTWOOD','ELECTIONS OFFICE'))) %>%
    left_join(precincts %>% st_drop_geometry() %>% select(PrecinctCode=Precinct, PrecinctName=PrecName, FIPS)) %>%
    filter(!(Candidate %in% c('Registered Voters', 'Times Counted', 'Times Under Voted', 'Times Over Voted'))) %>%
    filter(!is.na(PrecinctCode))
  
  pierce <- nonKing %>% filter(FIPS==53053) %>% select(-PrecinctCode) %>%
    left_join(precincts %>% st_drop_geometry() %>% filter(FIPS==53053) %>% select(PrecinctCode=Precinct, PrecinctName=PrecName))
  
  electionResultsDf <- bind_rows(king, pierce, nonKing %>% filter(FIPS != 53053)) %>% arrange(FIPS)
  
  electionResultsDf
  
}

cleanResults2020 <- function(precincts, statewideFile, kingFile) {
  
  nonKing <- read_csv(statewideFile, col_types = 'ccccii') %>%
    filter(PrecinctCode != -1) %>% inner_join(countyCodes, by='CountyCode') %>% mutate(FIPS=as.integer(FIPS)) %>%
    filter(FIPS != 53033) %>%
    select(-CountyCode) %>%
    mutate(StarFlag=grepl(x=PrecinctName, pattern='.+\\*\\)$'), PrecinctName=gsub(x=PrecinctName, pattern='(.+)(?:\\(\\*\\))', replacement='\\1')) %>%
    filter(!(FIPS==53077 & PrecinctCode==1)) # weird yakima county courthouse "precinct" with no votes
  
  nonKing <- nonKing %>% filter(FIPS != 53011) %>% bind_rows(
    # Clark uses the name field
    nonKing %>% filter(FIPS==53011) %>% mutate(PrecinctCode=as.double(PrecinctName))
  )
  
  king <- read_csv(kingFile) %>% mutate(FIPS=53033) %>%
    select(Race, PrecinctName=Precinct, Candidate=CounterType, Votes=SumOfCount, FIPS) %>%
    filter(!(PrecinctName %in% c('WESTWOOD','ELECTIONS OFFICE'))) %>%
    left_join(precincts %>% st_drop_geometry() %>% select(PrecinctCode=Precinct, PrecinctName=PrecName, FIPS)) %>%
    filter(!(Candidate %in% c('Registered Voters', 'Times Counted', 'Times Under Voted', 'Times Over Voted'))) %>%
    filter(!is.na(PrecinctCode))

  pierce <- nonKing %>% filter(FIPS==53053) %>% select(-PrecinctCode) %>%
    left_join(precincts %>% st_drop_geometry() %>% filter(FIPS==53053) %>% select(PrecinctCode=Precinct, PrecinctName=PrecName))

  electionResultsDf <- bind_rows(king, pierce, nonKing %>% filter(FIPS != 53053)) %>% arrange(FIPS)

  electionResultsDf
  
}

generalResults2020Df <- cleanResults2020(precincts2020, '/opt/data/elections/wa_general_20201103/statewide.csv', '/opt/data/elections/wa_general_20201103/king.csv')

# infer the legislative districts for 2020 from results

legDistricts2020 <- generalResults2020Df %>%
  mutate(Race=tolower(Race)) %>%
  filter(grepl(x=Race, pattern='legis')) %>%
  mutate(LegDistrict=gsub(x=Race, pattern='[^0123456789]+([0-9]+).*', replacement='\\1')) %>%
  select(FIPS, PrecinctCode, LegDistrict) %>% distinct() %>%
  mutate(LegDistrict=as.integer(LegDistrict))

precincts2020 <- precincts2020 %>% left_join(legDistricts2020, by=c('FIPS', 'Precinct'='PrecinctCode')) %>%
  mutate(
    LegDistrict=case_when(
      FIPS=='53025' ~ case_when(
        Precinct %in% 76:79 ~ 13,
        Precinct==80 ~ 12,
        .default = LegDistrict
      ),
      FIPS=='53041' ~ case_when(
        Precinct==114 ~ 20,
        .default = LegDistrict
      ),
      FIPS=='53007' ~ case_when(
        Precinct==142 ~ 12,
        .default = LegDistrict
      ),
      .default = LegDistrict
    )
  )

primaryResults2024Df <- cleanResults2024(precincts2024, '/opt/data/elections/wa_primary_20240806/statewide.csv', '/opt/data/elections/wa_primary_20240806/king.csv')
generalResults2024Df <- cleanResults2024(precincts2024, '/opt/data/elections/wa_general_20241105/statewide.csv', '/opt/data/elections/wa_general_20241105/king.csv')

# write out local files so RMarkdown can find them

precincts2020 %>% saveRDS('/tmp/precincts2020.rds')
precincts2024 %>% saveRDS('/tmp/precincts2024.rds')
primaryResults2024Df %>% saveRDS('/tmp/primaryResults2024Df.rds')
generalResults2024Df %>% saveRDS('/tmp/generalResults2024Df.rds')
generalResults2020Df %>% saveRDS('/tmp/generalResults2020Df.rds')





