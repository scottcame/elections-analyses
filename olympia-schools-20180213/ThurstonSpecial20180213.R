library(tidyverse)
library(rgdal)
library(rgeos)
library(broom)
library(scales)

# Generate a visualization of precinct-level results for the Olympia School District capital projects levy in Feb 2018
# Note that this requires downloading voting precinct shapefiles from the Washington Secretary of State and Census boundary files from the US Census site

thurstonPrecincts <- readOGR('/opt/data/Shapefiles/wa-voting-precincts/', 'Statewide_Prec_2017', stringsAsFactors = FALSE) %>%
  subset(COUNTY=='Thurston')

thurstonPrecinctsData <- thurstonPrecincts@data
thurstonPrecinctsData$id <- rownames(thurstonPrecinctsData)
thurstonPrecinctsData <- thurstonPrecinctsData %>% as_tibble()

osdLevyResults <- read_csv('http://results.vote.wa.gov/results/20180213/export/20180213_Thurston_116397-Precincts.csv', skip = 2, col_names = c('Precinct', 'Yes', 'No'))

osdLevyResults <- osdLevyResults %>%
  mutate(Yes=case_when(Yes=='*' ~ NA_character_, TRUE ~ Yes),
         No=case_when(No=='*' ~ NA_character_, TRUE ~ No)) %>%
  mutate_at(vars(-Precinct), as.integer) %>%
  mutate(Precinct=toupper(Precinct)) %>%
  mutate(Precinct=gsub(x=Precinct, pattern='([A-Z]+) 0([0-9])', replacement='\\1 \\2')) %>%
  mutate(Precinct=gsub(x=Precinct, pattern='(.+)\\(\\*\\)', replacement='\\1')) %>%
  mutate(Precinct=case_when(Precinct=='FISH TRAP' ~ 'FISHTRAP', TRUE ~ Precinct)) %>%
  mutate(YesMargin=(Yes/(Yes+No)) - .5) %>%
  inner_join(thurstonPrecinctsData %>% select(id, PRECNAME), by=c('Precinct'='PRECNAME'))

olympiaPlace <- readOGR('/opt/data/Shapefiles/cb_2016_53_place_500k/', 'cb_2016_53_place_500k', stringsAsFactors = FALSE) %>%
  subset(NAME=='Olympia')

thurstonPrecincts <- spTransform(thurstonPrecincts, CRS(proj4string(olympiaPlace)))
thurstonPrecinctsSDF <- tidy(thurstonPrecincts) %>%
  inner_join(osdLevyResults, by='id')

places <- tribble(
  ~lat, ~long, ~Name,
  47.018542, -122.884644, 'Olympia HS',
  47.050339, -122.935446, 'Capital HS',
  47.035788, -122.904338, 'State Capitol',
  47.043508, -122.887683, 'Avanti HS',
  47.135614, -122.885237, 'Boston Harbor ES',
  47.050891, -123.115984, 'Summit Lake',
  47.043534, -122.978881, 'McLane ES',
  47.004830, -122.862132, 'Centennial ES',
  47.070811, -122.927306, 'LP Brown ES'
) %>%
  mutate(Color=case_when(grepl(x=Name, pattern='Centennial|Brown') ~ 'black', TRUE ~ 'white'))

theme_map <- function(...) {
  theme_void() +
    theme(
      text = element_text(family = "Trebuchet MS", size=8),
      legend.title = element_text(size = rel(1)),
      title = element_text(size = rel(1.2)),
      legend.position = 'bottom',
      ...
    )
}

overallMargin <- osdLevyResults %>%
  summarize_at(vars(Yes, No), sum, na.rm=TRUE) %>%
  mutate(YesMargin=(Yes/(Yes+No)) - .5) %>%
  mutate(lat=47.135614, long=-123.115984) %>%
  mutate(labelText=paste0('Overall Margin in Favor of "Yes" Votes: ', percent(YesMargin)))

ggplot() +
  geom_path(data=thurstonPrecinctsSDF, mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon(data=thurstonPrecinctsSDF, mapping=aes(x=long, y=lat, group=group, fill=YesMargin)) +
  geom_text(data=places, mapping=aes(x=long, y=lat, label=Name), nudge_x=.005, hjust='left') +
  geom_point(data=places, mapping=aes(x=long, y=lat)) +
  scale_fill_gradient2(na.value = 'grey70', labels=percent, low='#762a83', high='#1b7837', mid='white', limits=c(-.4, .4)) +
  coord_map() +
  theme_map() +
  labs(fill='Margin of "Yes" Votes', title='Results of Olympia School District Technology and Capital Projects Replacement Levy',
       subtitle='Special Election on February 13, 2018, By Precinct',
       caption=paste0('Source: Thurston County Auditor, http://results.vote.wa.gov/results/20180213/thurston/Precincts-116397.html\n',
                      'Precincts in grey consolidated in results to protect voter privacy\n',
                      'Selected schools in the District shown for reference')) +
  theme(legend.position = 'bottom')



