# Processing Exposure Estimates from Fred
# ambient - Batch 2
# Tanya Alderete and Maximilian Bailey
# 24 Arp 21

#clear workspace
rm(list=ls())

#load necessary packages
library(plyr)
library(dplyr)
library(naniar)
require(stringi)
library(tidyverse)
meta2 <- "/Volumes/ADORLab/__Users/ditr2320/datasets/"

#read in CALINE data
mm_ambient <- read.csv("/Volumes/ADORLab/HEI Study/Exposures/Exposure Estimates/Exposure Estiamtes From Fred/Batch 2 2020 Files From Fred/Batch 2 with Monthly Lags/MothersMilk_RegAQ_NO2_O3_PM25_PM10_MonthlyAvg_v1c_v762020b_noPWD_DOS.csv", header=TRUE,sep=',',na.strings = c("NA", "")) 

# Change -99 to NA
mm_ambient <- mm_ambient %>%
  replace_with_na(replace = list(NO2.ppb. = -99,
                                 O3.24h.ppb. = -99,
                                 O3.8hm.ppb. = -99,
                                 O3.1hm.ppb. = -99,
                                 PM25.ppb. = -99, 
                                 PM10.ppb. = -99
  ))
#####################################################
# Creating Weighted Averages  
####################################################

# Want to create weighted average the are NOT NA if one estimate from window is missing # Why retain these?
# To do this need to change NA to zero
#mm_ambient_na_to_zero <- mm_ambient %>%
#mutate_at(vars(NO2.ppb.:PM10.Days), ~replace(., is.na(.), 0))
mm_ambient_na_to_zero <- mm_ambient 

# Create average across time by ID (Preg to 1 month)
Preg_1mOnly <- mm_ambient_na_to_zero %>% 
  filter(Period.No. <= 2) %>% 
  group_by(GeoID)

# NO2
ExpTimesDaysTotNO2_Preg_1m <-Preg_1mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotNO2_Preg_1m = sum(NO2.ppb. * NPDays)) %>%
  full_join(Preg_1mOnly)

TotNO2_Preg_1m_weighted <- ExpTimesDaysTotNO2_Preg_1m %>% 
  group_by(GeoID) %>%
  transmute(TotNO2_Preg_1m_weighted = sum(ExpTimesDaysTotNO2_Preg_1m) / sum(NPDays))


#O3.24h.ppb. 
ExpTimesDaysTotO3_24h_Preg_1m <-Preg_1mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotO3_24h_Preg_1m = sum(O3.24h.ppb. * NPDays)) %>%
  full_join(Preg_1mOnly)

TotO3_24h_Preg_1m_weighted <- ExpTimesDaysTotO3_24h_Preg_1m %>% 
  group_by(GeoID) %>%
  transmute(TotO3_24h_Preg_1m_weighted = sum(ExpTimesDaysTotO3_24h_Preg_1m) / sum(NPDays))

#O3.8hm.ppb. 
ExpTimesDaysTotO3_8h_Preg_1m <-Preg_1mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotO3_8h_Preg_1m = sum(O3.8hm.ppb. * NPDays)) %>%
  full_join(Preg_1mOnly)

TotO3_8h_Preg_1m_weighted <- ExpTimesDaysTotO3_8h_Preg_1m %>% 
  group_by(GeoID) %>%
  transmute(TotO3_8h_Preg_1m_weighted = sum(ExpTimesDaysTotO3_8h_Preg_1m) / sum(NPDays))

#O3.1hm.ppb. 
ExpTimesDaysTotO3_1h_Preg_1m <-Preg_1mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotO3_1h_Preg_1m = sum(O3.1hm.ppb. * NPDays)) %>%
  full_join(Preg_1mOnly)

TotO3_1h_Preg_1m_weighted <- ExpTimesDaysTotO3_1h_Preg_1m %>% 
  group_by(GeoID) %>%
  transmute(TotO3_1h_Preg_1m_weighted = sum(ExpTimesDaysTotO3_1h_Preg_1m) / sum(NPDays))

#PM25.ppb. 
ExpTimesDaysTotPM25_Preg_1m <-Preg_1mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotPM25_Preg_1m = sum(PM25.ppb. * NPDays)) %>%
  full_join(Preg_1mOnly)

TotPM25_Preg_1m_weighted <- ExpTimesDaysTotPM25_Preg_1m %>% 
  group_by(GeoID) %>%
  transmute(TotPM25_Preg_1m_weighted = sum(ExpTimesDaysTotPM25_Preg_1m) / sum(NPDays))

#PM10.ppb. 
ExpTimesDaysTotPM10_Preg_1m <-Preg_1mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotPM10_Preg_1m = sum(PM10.ppb. * NPDays)) %>%
  full_join(Preg_1mOnly)

TotPM10_Preg_1m_weighted <- ExpTimesDaysTotPM10_Preg_1m %>% 
  group_by(GeoID) %>%
  transmute(TotPM10_Preg_1m_weighted = sum(ExpTimesDaysTotPM10_Preg_1m) / sum(NPDays))

# Create average across time by ID (Preg to 6 months)
Preg_6mOnly <- mm_ambient_na_to_zero %>% 
  filter(Period.No. <= 3) %>% 
  group_by(GeoID)

# NO2
ExpTimesDaysTotNO2_Preg_6m <-Preg_6mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotNO2_Preg_6m = sum(NO2.ppb. * NPDays)) %>%
  full_join(Preg_6mOnly)

TotNO2_Preg_6m_weighted <- ExpTimesDaysTotNO2_Preg_6m %>% 
  group_by(GeoID) %>%
  transmute(TotNO2_Preg_6m_weighted = sum(ExpTimesDaysTotNO2_Preg_6m) / sum(NPDays))

#O3.24h.ppb. 
ExpTimesDaysTotO3_24h_Preg_6m <-Preg_6mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotO3_24h_Preg_6m = sum(O3.24h.ppb. * NPDays)) %>%
  full_join(Preg_6mOnly)

TotO3_24h_Preg_6m_weighted <- ExpTimesDaysTotO3_24h_Preg_6m %>% 
  group_by(GeoID) %>%
  transmute(TotO3_24h_Preg_6m_weighted = sum(ExpTimesDaysTotO3_24h_Preg_6m) / sum(NPDays))

#O3.8hm.ppb. 
ExpTimesDaysTotO3_8h_Preg_6m <-Preg_6mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotO3_8h_Preg_6m = sum(O3.8hm.ppb. * NPDays)) %>%
  full_join(Preg_6mOnly)

TotO3_8h_Preg_6m_weighted <- ExpTimesDaysTotO3_8h_Preg_6m %>% 
  group_by(GeoID) %>%
  transmute(TotO3_8h_Preg_6m_weighted = sum(ExpTimesDaysTotO3_8h_Preg_6m) / sum(NPDays))

#O3.1hm.ppb. 
ExpTimesDaysTotO3_1h_Preg_6m <-Preg_6mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotO3_1h_Preg_6m = sum(O3.1hm.ppb. * NPDays)) %>%
  full_join(Preg_6mOnly)

TotO3_1h_Preg_6m_weighted <- ExpTimesDaysTotO3_1h_Preg_6m %>% 
  group_by(GeoID) %>%
  transmute(TotO3_1h_Preg_6m_weighted = sum(ExpTimesDaysTotO3_1h_Preg_6m) / sum(NPDays))

#PM25.ppb. 
ExpTimesDaysTotPM25_Preg_6m <-Preg_6mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotPM25_Preg_6m = sum(PM25.ppb. * NPDays)) %>%
  full_join(Preg_6mOnly)

TotPM25_Preg_6m_weighted <- ExpTimesDaysTotPM25_Preg_6m %>% 
  group_by(GeoID) %>%
  transmute(TotPM25_Preg_6m_weighted = sum(ExpTimesDaysTotPM25_Preg_6m) / sum(NPDays))

#PM10.ppb. 
ExpTimesDaysTotPM10_Preg_6m <-Preg_6mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotPM10_Preg_6m = sum(PM10.ppb. * NPDays)) %>%
  full_join(Preg_6mOnly)

TotPM10_Preg_6m_weighted <- ExpTimesDaysTotPM10_Preg_6m %>% 
  group_by(GeoID) %>%
  transmute(TotPM10_Preg_6m_weighted = sum(ExpTimesDaysTotPM10_Preg_6m) / sum(NPDays))

# Create average across time by ID (Birth to 6 months)
Birth_6mOnly <- mm_ambient_na_to_zero %>% 
  filter(Period.No. == 3 |Period.No. == 2) %>% 
  group_by(GeoID)

# NO2
ExpTimesDaysTotNO2_Birth_6m <-Birth_6mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotNO2_Birth_6m = sum(NO2.ppb. * NPDays)) %>%
  full_join(Birth_6mOnly)

TotNO2_Birth_6m_weighted <- ExpTimesDaysTotNO2_Birth_6m %>% 
  group_by(GeoID) %>%
  transmute(TotNO2_Birth_6m_weighted = sum(ExpTimesDaysTotNO2_Birth_6m) / sum(NPDays))

#O3.24h.ppb. 
ExpTimesDaysTotO3_24h_Birth_6m <-Birth_6mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotO3_24h_Birth_6m = sum(O3.24h.ppb. * NPDays)) %>%
  full_join(Birth_6mOnly)

TotO3_24h_Birth_6m_weighted <- ExpTimesDaysTotO3_24h_Birth_6m %>% 
  group_by(GeoID) %>%
  transmute(TotO3_24h_Birth_6m_weighted = sum(ExpTimesDaysTotO3_24h_Birth_6m) / sum(NPDays))

#O3.8hm.ppb. 
ExpTimesDaysTotO3_8h_Birth_6m <-Birth_6mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotO3_8h_Birth_6m = sum(O3.8hm.ppb. * NPDays)) %>%
  full_join(Birth_6mOnly)

TotO3_8h_Birth_6m_weighted <- ExpTimesDaysTotO3_8h_Birth_6m %>% 
  group_by(GeoID) %>%
  transmute(TotO3_8h_Birth_6m_weighted = sum(ExpTimesDaysTotO3_8h_Birth_6m) / sum(NPDays))

#O3.1hm.ppb. 
ExpTimesDaysTotO3_1h_Birth_6m <-Birth_6mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotO3_1h_Birth_6m = sum(O3.1hm.ppb. * NPDays)) %>%
  full_join(Birth_6mOnly)

TotO3_1h_Birth_6m_weighted <- ExpTimesDaysTotO3_1h_Birth_6m %>% 
  group_by(GeoID) %>%
  transmute(TotO3_1h_Birth_6m_weighted = sum(ExpTimesDaysTotO3_1h_Birth_6m) / sum(NPDays))

#PM25.ppb. 
ExpTimesDaysTotPM25_Birth_6m <-Birth_6mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotPM25_Birth_6m = sum(PM25.ppb. * NPDays)) %>%
  full_join(Birth_6mOnly)

TotPM25_Birth_6m_weighted <- ExpTimesDaysTotPM25_Birth_6m %>% 
  group_by(GeoID) %>%
  transmute(TotPM25_Birth_6m_weighted = sum(ExpTimesDaysTotPM25_Birth_6m) / sum(NPDays))

#PM10.ppb. 
ExpTimesDaysTotPM10_Birth_6m <-Birth_6mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotPM10_Birth_6m = sum(PM10.ppb. * NPDays)) %>%
  full_join(Birth_6mOnly)

TotPM10_Birth_6m_weighted <- ExpTimesDaysTotPM10_Birth_6m %>% 
  group_by(GeoID) %>%
  transmute(TotPM10_Birth_6m_weighted = sum(ExpTimesDaysTotPM10_Birth_6m) / sum(NPDays))

# Create average across time by ID (Birth to 12 months)
Birth_12mOnly <- mm_ambient_na_to_zero %>% 
  filter(Period.No. == 3 |Period.No. == 2 | Period.No. == 4 ) %>% 
  group_by(GeoID)

# NO2
ExpTimesDaysTotNO2_Birth_12m <-Birth_12mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotNO2_Birth_12m = sum(NO2.ppb. * NPDays)) %>%
  full_join(Birth_12mOnly)

TotNO2_Birth_12m_weighted <- ExpTimesDaysTotNO2_Birth_12m %>% 
  group_by(GeoID) %>%
  transmute(TotNO2_Birth_12m_weighted = sum(ExpTimesDaysTotNO2_Birth_12m) / sum(NPDays))

#O3.24h.ppb. 
ExpTimesDaysTotO3_24h_Birth_12m <-Birth_12mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotO3_24h_Birth_12m = sum(O3.24h.ppb. * NPDays)) %>%
  full_join(Birth_12mOnly)

TotO3_24h_Birth_12m_weighted <- ExpTimesDaysTotO3_24h_Birth_12m %>% 
  group_by(GeoID) %>%
  transmute(TotO3_24h_Birth_12m_weighted = sum(ExpTimesDaysTotO3_24h_Birth_12m) / sum(NPDays))

#O3.8hm.ppb. 
ExpTimesDaysTotO3_8h_Birth_12m <-Birth_12mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotO3_8h_Birth_12m = sum(O3.8hm.ppb. * NPDays)) %>%
  full_join(Birth_12mOnly)

TotO3_8h_Birth_12m_weighted <- ExpTimesDaysTotO3_8h_Birth_12m %>% 
  group_by(GeoID) %>%
  transmute(TotO3_8h_Birth_12m_weighted = sum(ExpTimesDaysTotO3_8h_Birth_12m) / sum(NPDays))

#O3.1hm.ppb. 
ExpTimesDaysTotO3_1h_Birth_12m <-Birth_12mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotO3_1h_Birth_12m = sum(O3.1hm.ppb. * NPDays)) %>%
  full_join(Birth_12mOnly)

TotO3_1h_Birth_12m_weighted <- ExpTimesDaysTotO3_1h_Birth_12m %>% 
  group_by(GeoID) %>%
  transmute(TotO3_1h_Birth_12m_weighted = sum(ExpTimesDaysTotO3_1h_Birth_12m) / sum(NPDays))

#PM25.ppb. 
ExpTimesDaysTotPM25_Birth_12m <-Birth_12mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotPM25_Birth_12m = sum(PM25.ppb. * NPDays)) %>%
  full_join(Birth_12mOnly)

TotPM25_Birth_12m_weighted <- ExpTimesDaysTotPM25_Birth_12m %>% 
  group_by(GeoID) %>%
  transmute(TotPM25_Birth_12m_weighted = sum(ExpTimesDaysTotPM25_Birth_12m) / sum(NPDays))

#PM10.ppb. 
ExpTimesDaysTotPM10_Birth_12m <-Birth_12mOnly %>% 
  group_by(GeoID, Period.No.) %>%
  transmute(ExpTimesDaysTotPM10_Birth_12m = sum(PM10.ppb. * NPDays)) %>%
  full_join(Birth_12mOnly)

TotPM10_Birth_12m_weighted <- ExpTimesDaysTotPM10_Birth_12m %>% 
  group_by(GeoID) %>%
  transmute(TotPM10_Birth_12m_weighted = sum(ExpTimesDaysTotPM10_Birth_12m) / sum(NPDays))



# Keep just 1 row for each ID
TotNO2_Preg_1m_weighted <-
  TotNO2_Preg_1m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotO3_24h_Preg_1m_weighted <-
  TotO3_24h_Preg_1m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotO3_8h_Preg_1m_weighted <-
  TotO3_8h_Preg_1m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotO3_1h_Preg_1m_weighted <-
  TotO3_1h_Preg_1m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotPM25_Preg_1m_weighted <-
  TotPM25_Preg_1m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotPM10_Preg_1m_weighted <-
  TotPM10_Preg_1m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotNO2_Preg_6m_weighted <-
  TotNO2_Preg_6m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotO3_24h_Preg_6m_weighted <-
  TotO3_24h_Preg_6m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotO3_8h_Preg_6m_weighted <-
  TotO3_8h_Preg_6m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotO3_1h_Preg_6m_weighted <-
  TotO3_1h_Preg_6m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotPM25_Preg_6m_weighted <-
  TotPM25_Preg_6m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotPM10_Preg_6m_weighted <-
  TotPM10_Preg_6m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotNO2_Birth_6m_weighted <-
  TotNO2_Birth_6m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotO3_24h_Birth_6m_weighted <-
  TotO3_24h_Birth_6m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotO3_8h_Birth_6m_weighted <-
  TotO3_8h_Birth_6m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotO3_1h_Birth_6m_weighted <-
  TotO3_1h_Birth_6m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotPM25_Birth_6m_weighted <-
  TotPM25_Birth_6m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotPM10_Birth_6m_weighted <-
  TotPM10_Birth_6m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotNO2_Birth_12m_weighted <-
  TotNO2_Birth_12m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotO3_24h_Birth_12m_weighted <-
  TotO3_24h_Birth_12m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotO3_8h_Birth_12m_weighted <-
  TotO3_8h_Birth_12m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotO3_1h_Birth_12m_weighted <-
  TotO3_1h_Birth_12m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotPM25_Birth_12m_weighted <-
  TotPM25_Birth_12m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

TotPM10_Birth_12m_weighted <-
  TotPM10_Birth_12m_weighted %>% 
  group_by(GeoID) %>% 
  filter(row_number()==1)

# Want to keep and add in Preg period and BL
# Preg
mm_ambient_na_to_zero_preg <-
  mm_ambient_na_to_zero %>% 
  group_by(GeoID) %>% 
  filter(Period.No. ==1)

mm_ambient_na_to_zero_preg <- mm_ambient_na_to_zero_preg %>% 
  dplyr::select(GeoID,NO2.ppb.,O3.24h.ppb.,O3.8hm.ppb.,PM25.ppb.,PM10.ppb.)

mm_ambient_na_to_zero_preg <- mm_ambient_na_to_zero_preg %>% dplyr::rename(PregOnlyPM10ppb = PM10.ppb.,
                                                                           PregOnlyPM25ppb = PM25.ppb.,
                                                                           PregOnlyNO2ppb = NO2.ppb.,
                                                                           PregOnlyO324hppb = O3.24h.ppb.,
                                                                           PregOnlyO38hmppb = O3.8hm.ppb.)

# BL                                                                   

mm_ambient_na_to_zero_bl <-
  mm_ambient_na_to_zero %>% 
  group_by(GeoID) %>% 
  filter(row_number()== 2)

mm_ambient_na_to_zero_bl <- mm_ambient_na_to_zero_bl %>% 
  dplyr::select(GeoID,NO2.ppb.,O3.24h.ppb.,O3.8hm.ppb.,PM25.ppb.,PM10.ppb.)

mm_ambient_na_to_zero_bl <- mm_ambient_na_to_zero_bl %>% dplyr::rename(blOnlyPM10ppb = PM10.ppb.,
                                                                         blOnlyPM25ppb = PM25.ppb.,
                                                                         blOnlyNO2ppb = NO2.ppb.,
                                                                         blOnlyO324hppb = O3.24h.ppb.,
                                                                         blOnlyO38hmppb = O3.8hm.ppb.)

# Merge together Weighted Exposure Estimates
ambient_preg_1m <- TotNO2_Preg_1m_weighted %>% inner_join(TotO3_24h_Preg_1m_weighted, by="GeoID") %>% inner_join(TotO3_8h_Preg_1m_weighted, by="GeoID")  %>% inner_join(TotO3_1h_Preg_1m_weighted, by="GeoID") %>% inner_join(TotPM25_Preg_1m_weighted, by="GeoID") %>% inner_join(TotPM10_Preg_1m_weighted, by="GeoID")

ambient_preg_6m <- TotNO2_Preg_6m_weighted %>% inner_join(TotO3_24h_Preg_6m_weighted, by="GeoID") %>% inner_join(TotO3_8h_Preg_6m_weighted, by="GeoID")  %>% inner_join(TotO3_1h_Preg_6m_weighted, by="GeoID") %>% inner_join(TotPM25_Preg_6m_weighted, by="GeoID") %>% inner_join(TotPM10_Preg_6m_weighted, by="GeoID")

ambient_Birth_6m <- TotNO2_Birth_6m_weighted %>% inner_join(TotO3_24h_Birth_6m_weighted, by="GeoID") %>% inner_join(TotO3_8h_Birth_6m_weighted, by="GeoID")  %>% inner_join(TotO3_1h_Birth_6m_weighted, by="GeoID") %>% inner_join(TotPM25_Birth_6m_weighted, by="GeoID") %>% inner_join(TotPM10_Birth_6m_weighted, by="GeoID")

ambient_Birth_12m <- TotNO2_Birth_12m_weighted %>% inner_join(TotO3_24h_Birth_12m_weighted, by="GeoID") %>% inner_join(TotO3_8h_Birth_12m_weighted, by="GeoID")  %>% inner_join(TotO3_1h_Birth_12m_weighted, by="GeoID") %>% inner_join(TotPM25_Birth_12m_weighted, by="GeoID") %>% inner_join(TotPM10_Birth_12m_weighted, by="GeoID")

ambient_preg_1m_6m <- ambient_preg_1m %>% inner_join(ambient_preg_6m, by="GeoID") %>% inner_join(ambient_Birth_6m, by="GeoID")

ambient_preg_1m_6m_12m <- ambient_preg_1m_6m %>% inner_join(ambient_Birth_12m, by="GeoID") 

ambient_preg_1m_6m_12m_bl <- ambient_preg_1m_6m_12m %>% inner_join(mm_ambient_na_to_zero_bl, by="GeoID") 

ambient_preg_1m_6m_12m_bl_plus_preonly <- ambient_preg_1m_6m_12m_bl %>% inner_join(mm_ambient_na_to_zero_preg, by="GeoID") 



# Extract out dyad ID and change to numeric
ambient_preg_1m_6m_12m_bl_plus_preonly$dyad_id <- stri_sub(ambient_preg_1m_6m_12m_bl_plus_preonly$GeoID,-3,-1)
ambient_preg_1m_6m_12m_bl_plus_preonly$dyad_id <- as.numeric(ambient_preg_1m_6m_12m_bl_plus_preonly$dyad_id)

write.table(ambient_preg_6m,paste0(meta2,"/ambient_preg_6m.csv",sep=""),quote=FALSE, sep=",",append=FALSE, col.names=TRUE, row.names=FALSE)
write.table(ambient_Birth_6m,paste0(meta2,"/ambient_Birth_6m.csv",sep=""),quote=FALSE, sep=",",append=FALSE, col.names=TRUE, row.names=FALSE)
