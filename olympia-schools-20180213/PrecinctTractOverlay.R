library(rgdal)
library(rgeos)
library(sp)
library(tidyverse)
library(broom)

# Script to experiment with extent of overlap between voting precincts and census tracts in Thurston County

thurstonPrecincts <- readOGR('/opt/data/Shapefiles/wa-voting-precincts/', 'Statewide_Prec_2017', stringsAsFactors = FALSE) %>%
  subset(COUNTY=='Thurston')

thurstonTracts <- readOGR('/opt/data/Shapefiles/cb_2016_53_tract_500k/', 'cb_2016_53_tract_500k', stringsAsFactors = FALSE) %>%
  subset(COUNTYFP == '067')

thurstonPrecincts <- spTransform(thurstonPrecincts, CRS(proj4string(thurstonTracts)))

thurstonTractsData <- thurstonTracts@data
thurstonTractsData$id <- rownames(thurstonTractsData)

ggplot() +
  geom_polygon(data=tidy(thurstonTracts) %>% inner_join(thurstonTractsData, by='id'), mapping=aes(x=long, y=lat, group=group, fill=ALAND), color='red') +
  geom_path(data=tidy(thurstonPrecincts), mapping=aes(x=long, y=lat, group=group)) +
  coord_map() +
  theme_void() +
  theme(legend.position = 'none')