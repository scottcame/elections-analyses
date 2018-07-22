library(rgdal)
library(rgeos)
library(tidyverse)
library(broom)
library(ggthemes)

# Script that generates visualizations of results of the 2016 Australian election for House of Representatives
# This script was just experimentation...final visualizations appear in the .Rmd in this project

# Downloaded MapInfo file from https://www.aec.gov.au/Electorates/gis/gis_datadownload.htm

nationalShp <- readOGR('/opt/data/Shapefiles/australia/election-districts/national/COM_ELB.TAB', layer="COM_ELB", stringsAsFactors = FALSE)

nationalShpData <- nationalShp@data
nationalShpData$id <- rownames(nationalShpData)

nationalShpData <- nationalShpData %>%
  mutate(Elect_div=case_when(
    Elect_div=='Mcpherson' ~ 'McPherson',
    Elect_div=='Mcmillan' ~ 'McMillan',
    TRUE ~ Elect_div
  ))

nationalShp <- nationalShp %>%
  # gSimplify(.01) %>%
  SpatialPolygonsDataFrame(nationalShpData)

turnout <- results %>%
  inner_join(Parties, by=c('WinningParty'='Party')) %>%
  select(-WinningParty, -State) %>%
  rename(WinningParty=PartyName) %>%
  inner_join(nationalShpData, by=c('CED'='Elect_div')) %>%
  mutate(alp=ALP/Turnout, Turnout=Turnout/Enrolment) %>%
  select(id, Turnout, alp, WinningParty, State)

nationalSPDF <- tidy(nationalShp) %>%
  left_join(turnout, by='id') %>%
  filter(State=='QLD')

nationalSPDF %>% ggplot(aes(x=long, y=lat, group=group)) + # , fill=alp, color=WinningParty)) +
  geom_polygon() +
  coord_map() + theme_map()
