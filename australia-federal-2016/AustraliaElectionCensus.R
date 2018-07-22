library(tidyverse)

# load census data
# use http://www.abs.gov.au/websitedbs/censushome.nsf/home/tablebuilder to generate tables by Commonwealth Electoral District
#  from the 2016 census

table1 <- read_csv(
  '/opt/data/australia/census-2016/table1.csv',
  skip=11,
  col_names=FALSE,
  n_max = 159
) %>%
  select(
    CED=X1, Family1Child=X2, Family2Children=X3, Family3Children=X4, Family4Children=X5,
    Family5Children=X6, Family6PlusChildren=X7, FamilyChildrenNA=X8
  )

table2 <- read_csv(
  '/opt/data/australia/census-2016/table2.csv',
  skip=11,
  col_names=FALSE,
  n_max = 159
) %>%
  select(
    CED=X1, HHInc_Negative=X2, HHInc_Nil=X3, HHInc_1_149=X4, HHInc_150_299=X5,
    HHInc_300_399=X6, HHInc_400_499=X7, HHInc_500_649=X8, HHInc_650_799=X9,
    HHInc_800_999=X10, HHInc_1000_1249=X11, HHInc_1250_1499=X12,
    HHInc_1500_1749=X13, HHInc_1750_1999=X14, HHInc_2000_2499=X15,
    HHInc_2500_2999=X16, HHInc_3000_3499=X17, HHInc_3500_3999=X18,
    HHInc_4000_4499=X19, HHInc_4500_4999=X20, HHInc_5000_5999=X21,
    HHInc_6000_7999=X22, HHInc_8000_Plus=X23, HHInc_Partial=X24,
    HHInc_All_incomes_not_stated=X25, HHInc_NA=X26, HHTotal=X27
    
  )

table3 <- read_csv(
  '/opt/data/australia/census-2016/table3.csv',
  skip=11,
  col_names=FALSE,
  n_max = 159
) %>%
  select(
    CED=X1, Rent_Nil=X2,
    Rent_1_74=X3,
    Rent_75_99=X4,
    Rent_100_124=X5,
    Rent_125_149=X6,
    Rent_150_174=X7,
    Rent_175_199=X8,
    Rent_200_224=X9,
    Rent_225_249=X10,
    Rent_250_274=X11,
    Rent_275_299=X12,
    Rent_300_324=X13,
    Rent_325_349=X14,
    Rent_350_374=X15,
    Rent_375_399=X16,
    Rent_400_424=X17,
    Rent_425_449=X18,
    Rent_450_549=X19,
    Rent_550_649=X20,
    Rent_650_749=X21,
    Rent_750_849=X22,
    Rent_850_949=X23,
    Rent_950_Plus=X24,
    Rent_Not_stated=X25,
    Rent_Not_applicable=X26
  )

table4 <- read_csv(
  '/opt/data/australia/census-2016/table4.csv',
  skip=11,
  col_names=FALSE,
  n_max = 168
) %>%
  select(
    CED=X1, Age_0_4=X2,
    Age_5_9=X3,
    Age_10_14=X4,
    Age_15_19=X5,
    Age_20_24=X6,
    Age_25_29=X7,
    Age_30_34=X8,
    Age_35_39=X9,
    Age_40_44=X10,
    Age_45_49=X11,
    Age_50_54=X12,
    Age_55_59=X13,
    Age_60_64=X14,
    Age_65_69=X15,
    Age_70_74=X16,
    Age_75_79=X17,
    Age_80_84=X18,
    Age_85_89=X19,
    Age_90_94=X20,
    Age_95_99=X21,
    Age_100_Plus=X22
  )

table5 <- read_csv(
  '/opt/data/australia/census-2016/table5.csv',
  skip=11,
  col_names=FALSE,
  n_max = 168
) %>%
  select(
    CED=X1, NeverMarried=X2, Widowed=X3, Divorced=X4, Separated=X5, Married=X6, MaritalStatusNA=X7
  )

table6 <- read_csv(
  '/opt/data/australia/census-2016/table6.csv',
  skip=11,
  col_names=FALSE,
  n_max = 168
) %>%
  select(
    CED=X1, Male=X2, Female=X3, Population=X4
  )

table7 <- read_csv(
  '/opt/data/australia/census-2016/table7.csv',
  skip=11,
  col_names=FALSE,
  n_max = 168
) %>%
  select(
    CED=X1, Ed_Postgraduate_Degree=X2,
    Ed_Graduate_Diploma_and_Graduate_Certificate=X3,
    Ed_Bachelor_Degree=X4,
    Ed_Advanced_Diploma_and_Diploma=X5,
    Ed_Certificate_III_IV=X6,
    Ed_Secondary_Education_Years_10_and_above=X7,
    Ed_Certificate_I_II=X8,
    Ed_Secondary_Education_Years_9_and_below=X9,
    Ed_Supplementary_Codes=X10,
    Ed_Not_stated=X11,
    Ed_Not_applicable=X12
  )

table8 <- read_csv(
  '/opt/data/australia/census-2016/table8.csv',
  skip=11,
  col_names=FALSE,
  n_max = 168
) %>%
  select(
    CED=X1, LabourForceStatus_Employed_worked_FT=X2,
    LabourForceStatus_Employed_worked_PT=X3,
    LabourForceStatus_Employed_away_from_work=X4,
    LabourForceStatus_Unemployed_looking_for_FT=X5,
    LabourForceStatus_Unemployed_looking_for_PT=X6,
    LabourForceStatus_Not_in_the_labour_force=X7,
    LabourForceStatus_Not_stated=X8,
    LabourForceStatus_Not_applicable=X9
  )

censusData <- map(paste0('table', 1:8), function(tname) {
  get(tname)
}) %>% reduce(full_join, by='CED')

rm(list=paste0('table', 1:8))