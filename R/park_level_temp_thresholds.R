library(tidyverse)
library(fetchaquarius)
library(NCBNAqua)

# establish connection to Aquarius
fetchaquarius::connectToAquarius("aqreadonly")

# get water temp for Duck Harbor and Pleasant Bay
caco_seagrass <- get_wl_data(park_code = "CACO", protocol = "ENE_Seagrass") %>%
  filter(Unit == "degC") %>%
  filter(Identifier %in% c("Water Temp.A@CACO_Seagrass_MA20_1", "Water Temp.B@CACO_Seagrass_MA20_1", "Water Temp.C@CACO_Seagrass_MA20_1", "Water Temp.A@CACO_Seagrass_MA20_2", "Water Temp.B@CACO_Seagrass_MA20_2", "Water Temp.C@CACO_Seagrass_MA20_2" ))

# get water temp for Provincetown
caco_pt <- map("National Park Service.Northeast Coastal and Barrier Network", ~fetchaquarius::getLocationInfo(folder = .x)) %>%
  bind_rows() %>%
  filter(Identifier == "NCBN_OP_ERP_MA-PH") %>%
  distinct() %>%
  mutate(parameters = map(Identifier, ~fetchaquarius::getTimeSeriesInfo(.x))) %>%
  select(-c(Identifier, UniqueId, UtcOffset, LastModified, Publish, Tags)) %>%
  unnest(cols = c(parameters)) %>%
  mutate(timeseries = map(Identifier, ~fetchaquarius::getTimeSeries(.x))) %>%
  select(Name, Identifier, LocationIdentifier, Unit, Label, timeseries) %>%
  mutate(data = map(timeseries, ~.x$Points)) %>%
  select(-timeseries) %>%
  unnest(cols = data) %>%
  unnest(cols = Value) %>%
  rename("Value" = Numeric) %>%
  as.data.frame %>%
  separate(., col = Timestamp, into = c("Date", "Time"), sep = " ", remove = FALSE) %>%
  mutate(Time = if_else(is.na(Time), "00:00:00", Time))

# get water temp for East Harbor
caco_eh <- map("National Park Service.Cape Cod National Seashore", ~fetchaquarius::getLocationInfo(folder = .x)) %>%
  bind_rows() %>%
  filter(Identifier == "CACO_EH_Lagoon") %>%
  distinct() %>%
  mutate(parameters = map(Identifier, ~fetchaquarius::getTimeSeriesInfo(.x))) %>%
  select(-c(Identifier, UniqueId, UtcOffset, LastModified, Publish, Tags)) %>%
  unnest(cols = c(parameters)) %>%
  mutate(timeseries = map(Identifier, ~fetchaquarius::getTimeSeries(.x))) %>%
  select(Name, Identifier, LocationIdentifier, Unit, Label, timeseries) %>%
  mutate(data = map(timeseries, ~.x$Points)) %>%
  select(-timeseries) %>%
  unnest(cols = data) %>%
  unnest(cols = Value) %>%
  rename("Value" = Numeric) %>%
  as.data.frame %>%
  separate(., col = Timestamp, into = c("Date", "Time"), sep = " ", remove = FALSE) %>%
  mutate(Time = if_else(is.na(Time), "00:00:00", Time)) %>%
  filter(Unit == "degC")

# get water temp for GSB and Moriches Bay
fiis_seagrass <- get_wl_data(park_code = "FIIS", protocol = "ENE_Seagrass") %>%
  filter(Unit == "degC") %>%
  filter(Identifier %in% c("Water Temp.A@FIIS_Seagrass_MB", "Water Temp.B@FIIS_Seagrass_MB", "Water Temp.C@FIIS_Seagrass_MB", "Water Temp.A@FIIS_Seagrass_GSB", "Water Temp.B@FIIS_Seagrass_GSB", "Water Temp.C@FIIS_Seagrass_GSB"))

# get water temp for Tingles Island
asis_seagrass <- get_wl_data(park_code = "ASIS", protocol = "ENE_Seagrass") %>%
  filter(Unit == "degC") %>%
  filter(Identifier %in% c("Water Temp.A@ASIS_Seagrass_Tingles", "Water Temp.B@ASIS_Seagrass_Tingles", "Water Temp.C@ASIS_Seagrass_Tingles"))

# join water temps from all sites and filter to May - Sept.
seagrass_temp <- bind_rows(list("CACO" = caco_seagrass, "CACO" = caco_pt, "CACO" = caco_eh, "FIIS" = fiis_seagrass, "ASIS" = asis_seagrass), .id = "park_code") %>%
  mutate(Transect = if_else(Name == "East Harbor", "NA", str_extract(Identifier, "(?<=\\.).*(?=@)")),
         Year = as.numeric(str_sub(Date, 1, 4)),
         Month = as.numeric(str_sub(Date, 6, 7)),
         Name = case_when(
           Name == "Moriches Bay" ~ "Moriches Bay (Great Gun)",
           Name == "Great South Bay" ~ "Great South Bay (Ho Hum Beach)",
           Name == "Provincetown Harbor, MA - Outside Park" ~ "Provincetown Harbor",
           T ~ Name
         )) %>%
  filter(Month %in% c(5, 6, 7, 8, 9)) 

# get the 'passing' years for each site/transect
passing_years <- seagrass_temp %>%
  group_by(park_code, Name, Transect, Year, Month) %>%
  summarise(date_count = n_distinct(Date)) %>%
  group_by(park_code, Name, Transect, Year) %>%
  nest() %>% # only include years with 80% data coverage and with at least 10 days of data in a minimum of three of the five focal months
  mutate(may_days = replace_na(as.numeric(map(data, ~.x %>% filter(Month == 5) %>% pull(date_count))), 0),
         may_pass = if_else(may_days >= 10, TRUE, FALSE),
         jun_days = replace_na(as.numeric(map(data, ~.x %>% filter(Month == 6) %>% pull(date_count))), 0),
         jun_pass = if_else(jun_days >= 10, TRUE, FALSE),
         jul_days = replace_na(as.numeric(map(data, ~.x %>% filter(Month == 7) %>% pull(date_count))), 0),
         jul_pass = if_else(jul_days >= 10, TRUE, FALSE),
         aug_days = replace_na(as.numeric(map(data, ~.x %>% filter(Month == 8) %>% pull(date_count))), 0),
         aug_pass = if_else(aug_days >= 10, TRUE, FALSE),
         sep_days = replace_na(as.numeric(map(data, ~.x %>% filter(Month == 9) %>% pull(date_count))), 0),
         sep_pass = if_else(sep_days >= 10, TRUE, FALSE),
         months_check = if_else(rowSums(across(ends_with("pass"))) >= 3, TRUE, FALSE),
         coverage_check = if_else(rowSums(across(ends_with("days"))) >= 122.4, TRUE, FALSE), # 80% data coverage = 122.4 out of 153 days = 80%
         all_check = if_else(months_check == TRUE & coverage_check == TRUE, TRUE, FALSE)) %>%
  filter(all_check)

# filter water temp data to passing years
seagrass_temp_filtered <- seagrass_temp %>%
  left_join(., passing_years %>% select(park_code, Name, Transect, Year, all_check), 
            by = c("park_code", "Name", "Transect", "Year")) %>%
  filter(all_check)

# get mean temp in each month across all years
seasonal_mean_temp <- seagrass_temp_filtered %>%
  group_by(park_code, Name, Transect, Month) %>%
  summarise(mean_temp_month = mean(Value, na.rm = TRUE))

# get max mean monthly temp across all years
max_mean_temp <- seasonal_mean_temp %>%
  group_by(park_code, Name) %>%
  summarise(max_mean_temp = max(mean_temp_month))

# get park-level threshold values by averaging the lowest and highest max mean monthly temp
park_thres_temp <- max_mean_temp %>%
  group_by(park_code) %>%
  summarise(min_max_mean_temp = min(max_mean_temp),
            max_max_mean_temp = max(max_mean_temp),
            park_thres_temp = mean(c(min_max_mean_temp, max_max_mean_temp))) 

# park-level thresholds as of 6/8/2026
# A tibble: 3 × 4
  # park_code min_max_mean_temp max_max_mean_temp park_thres_temp
  #   <chr>                 <dbl>             <dbl>       <num:.6!>
  # 1 ASIS                   28.4              28.4       28.440309
  # 2 CACO                   22.8              26.4       24.618491
  # 3 FIIS                   25.3              27.3       26.271115