library(tidyverse)

# downloaded election results from https://results.aec.gov.au/20499/Website/HouseDownloadsMenu-20499-Csv.htm

enrolment <- read_csv('/opt/data/australia/election-2016/GeneralEnrolmentByDivisionDownload-20499.csv', skip=1) %>%
  select(DivisionID, CED=DivisionNm, State=StateAb, Enrolment)

censusData <- censusData %>% semi_join(enrolment, by='CED')

results <- read_csv('/opt/data/australia/election-2016/HouseDopByDivisionDownload-20499.csv', skip=1) %>%
  mutate(
    PartyAb=case_when(is.na(PartyAb) ~ 'None', TRUE ~ PartyAb),
    PartyNm=case_when(is.na(PartyNm) ~ 'None', TRUE ~ PartyNm),
  )

Parties <- results %>%
  select(Party=PartyAb, PartyName=PartyNm) %>%
  group_by(Party) %>%
  filter(row_number()==1)

results <- results %>%
  filter(CalculationType=='Preference Count') %>%
  select(DivisionID, CountNumber, Party=PartyAb, CalculationValue, Surname, Elected) %>%
  group_by(DivisionID) %>%
  filter(CountNumber==max(CountNumber)) %>%
  select(-CountNumber)

turnout <- results %>% summarize(Turnout=sum(CalculationValue, na.rm=TRUE))
winner <- results %>% filter(Elected=='Y') %>% select(DivisionID, WinningParty=Party)

results <- results %>%
  group_by(DivisionID, Party) %>%
  summarize(Votes=sum(CalculationValue)) %>%
  spread(Party, Votes) %>%
  mutate_if(is.numeric, function(v) {
    case_when(is.na(v) ~ 0L, TRUE ~ as.integer(v))
  }) %>% inner_join(enrolment) %>% inner_join(turnout) %>% inner_join(winner) %>%
  ungroup()

headColumns <- c('CED', 'DivisionID', 'State', 'Enrolment', 'Turnout', 'WinningParty', 'ALP', 'LP', 'LNP', 'NP', 'GRN', 'IND')
otherColumns <- base::setdiff(colnames(results), headColumns)

results <- results %>% select(!!c(headColumns, otherColumns))

rm(turnout)




