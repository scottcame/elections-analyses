library(tidyverse)
library(dwapi)

# Script that parses Australian 2016 election results and census data, does some munging, and uploads to data.world

source('AustraliaElectionCensus.R')
source('AustraliaElectionResults.R')

configure(Sys.getenv("DATA_WORLD_RW_API_KEY"))
dwapi::upload_data_frame("scottcame/australian-federal-election-2016", Parties, 'Parties2016.csv')
dwapi::upload_data_frame("scottcame/australian-federal-election-2016", censusData, 'Census2016ElectoralDivision.csv')
dwapi::upload_data_frame("scottcame/australian-federal-election-2016", results, 'ElectionResults2016.csv')


