library(tidyverse)
library(fetchaquarius)
library(NCBNAqua)
library(here)
library(lubridate)

# -----------------------------------------------------------
# ADD THE UPDATED CSV READER FUNCTION *RIGHT HERE*
# -----------------------------------------------------------

read_temp_csv <- function(path,
                          site_name,
                          transect = "Fixed",
                          tz = "America/New_York") {
  
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  if (!all(c("DateTimeStamp", "Temp") %in% names(df))) {
    stop("CSV must contain: DateTimeStamp and Temp")
  }
  
  # Your exact timestamp format: m/d/Y H:M (no seconds)
  ts <- suppressWarnings(lubridate::parse_date_time(
    df$DateTimeStamp,
    orders = c("mdy HM"),       # <-- your format
    tz = tz
  ))
  
  # fallback patterns if needed
  if (any(is.na(ts))) {
    ts2 <- suppressWarnings(lubridate::parse_date_time(
      df$DateTimeStamp,
      orders = c("mdy HMS", "mdy IM", "Ymd HMS", "ymd HMS"),
      tz = tz
    ))
    ts[is.na(ts)] <- ts2[is.na(ts)]
  }
  
  if (all(is.na(ts))) stop("Could not parse DateTimeStamp in file: ", path)
  
  tibble::tibble(
    Name       = site_name,
    Identifier = NA_character_,
    Unit       = "degC",
    Timestamp  = ts,
    Date       = as.character(as.Date(ts)),
    Time       = format(ts, "%H:%M:%S"),
    Value      = as.numeric(df$Temp),
    Transect   = transect
  ) %>% dplyr::filter(!is.na(Value))
}


# establish connection to Aquarius
fetchaquarius::connectToAquarius("aqreadonly")

# -----------------------
# Existing pulls (unchanged)
# -----------------------

# Duck Harbor & Pleasant Bay (CACO seagrass)
caco_seagrass <- get_wl_data(park_code = "CACO", protocol = "ENE_Seagrass") %>%
  filter(Unit == "degC") %>%
  filter(Identifier %in% c(
    "Water Temp.A@CACO_Seagrass_MA20_1",
    "Water Temp.B@CACO_Seagrass_MA20_1",
    "Water Temp.C@CACO_Seagrass_MA20_1",
    "Water Temp.A@CACO_Seagrass_MA20_2",
    "Water Temp.B@CACO_Seagrass_MA20_2",
    "Water Temp.C@CACO_Seagrass_MA20_2"
  ))

# Provincetown Harbor (NCBN OP ERP)
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
  as.data.frame() %>%
  separate(., col = Timestamp, into = c("Date", "Time"), sep = " ", remove = FALSE) %>%
  mutate(Time = if_else(is.na(Time), "00:00:00", Time)) %>%
  filter(Unit == "degC")

# East Harbor (CACO_EH)
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
  as.data.frame() %>%
  separate(., col = Timestamp, into = c("Date", "Time"), sep = " ", remove = FALSE) %>%
  mutate(Time = if_else(is.na(Time), "00:00:00", Time)) %>%
  filter(Unit == "degC")

# FIIS seagrass (GSB + Moriches)
fiis_seagrass <- get_wl_data(park_code = "FIIS", protocol = "ENE_Seagrass") %>%
  filter(Unit == "degC") %>%
  filter(Identifier %in% c(
    "Water Temp.A@FIIS_Seagrass_MB",
    "Water Temp.B@FIIS_Seagrass_MB",
    "Water Temp.C@FIIS_Seagrass_MB",
    "Water Temp.A@FIIS_Seagrass_GSB",
    "Water Temp.B@FIIS_Seagrass_GSB",
    "Water Temp.C@FIIS_Seagrass_GSB"
  ))

# ASIS seagrass (Tingles Island)
asis_seagrass <- get_wl_data(park_code = "ASIS", protocol = "ENE_Seagrass") %>%
  filter(Unit == "degC") %>%
  filter(Identifier %in% c(
    "Water Temp.A@ASIS_Seagrass_Tingles",
    "Water Temp.B@ASIS_Seagrass_Tingles",
    "Water Temp.C@ASIS_Seagrass_Tingles"
  ))

# CALO Shackleford Banks (existing)
calo_shak <- map("National Park Service.Southeast Coast Network.Fixed Station WQ",
                 ~fetchaquarius::getLocationInfo(folder = .x)) %>%
  bind_rows() %>%
  filter(Identifier == "CALOshak01") %>%
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
  as.data.frame() %>%
  separate(., col = Timestamp, into = c("Date", "Time"), sep = " ", remove = FALSE) %>%
  mutate(Time = if_else(is.na(Time), "00:00:00", Time)) %>%
  filter(Unit == "degC") %>%
  filter(str_detect(Identifier, "Instantaneous")) %>%
  mutate(Transect = "Fixed")

# -----------------------
# NEW pulls
# -----------------------

# CALO Middle Marsh (CALOmidm02)
calo_midm <- map("National Park Service.Southeast Coast Network.Fixed Station WQ",
                 ~fetchaquarius::getLocationInfo(folder = .x)) %>%
  bind_rows() %>%
  filter(Identifier == "CALOmidm02") %>%
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
  as.data.frame() %>%
  separate(., col = Timestamp, into = c("Date", "Time"), sep = " ", remove = FALSE) %>%
  mutate(Time = if_else(is.na(Time), "00:00:00", Time)) %>%
  filter(Unit == "degC") %>%
  filter(str_detect(Identifier, "Instantaneous")) %>%
  mutate(Transect = "Fixed")

# CAHA Oregon Inlet / Old Coast Bridge (CAHAocbr01)
caha_ocbr <- map("National Park Service.Southeast Coast Network.Fixed Station WQ",
                 ~fetchaquarius::getLocationInfo(folder = .x)) %>%
  bind_rows() %>%
  filter(Identifier == "CAHAocbr01") %>%
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
  as.data.frame() %>%
  separate(., col = Timestamp, into = c("Date", "Time"), sep = " ", remove = FALSE) %>%
  mutate(Time = if_else(is.na(Time), "00:00:00", Time)) %>%
  filter(Unit == "degC") %>%
  filter(str_detect(Identifier, "Instantaneous")) %>%
  mutate(Transect = "Fixed")

## Read csv files##

wells_me <- read_temp_csv(
  path      = here("data", "wells_me.csv"),
  site_name = "Wells",
  transect  = "Fixed"
)

greatbay_nh <- read_temp_csv(
  path      = here("data", "greatbay_nh.csv"),
  site_name = "Great Bay",
  transect  = "Fixed"
)

# -----------------------
# Bind & derive
# -----------------------
seagrass_temp <- bind_rows(
  list(
    "CACO" = caco_seagrass,
    "CACO" = caco_pt,
    "CACO" = caco_eh,
    "FIIS" = fiis_seagrass,
    "ASIS" = asis_seagrass,
    "CALO" = calo_shak,
    "CALO" = calo_midm,   # NEW CALO site
    "CAHA" = caha_ocbr,    # NEW park/site
    "Maine" = wells_me,
    "New Hampshire" = greatbay_nh
  ),
  .id = "park_code"
) %>%
  mutate(
    Year  = as.integer(str_sub(Date, 1, 4)),
    Month = as.integer(str_sub(Date, 6, 7)),
    Name  = case_when(
      Name == "Moriches Bay" ~ "Moriches Bay (Great Gun)",
      Name == "Great South Bay" ~ "Great South Bay (Ho Hum Beach)",
      Name == "Provincetown Harbor, MA - Outside Park" ~ "Provincetown Harbor",
      TRUE ~ Name
    ),
    Transect = case_when(
      park_code %in% c("CALO", "CAHA") ~ "Fixed",            # SECN fixed stations
      Name %in% c("East Harbor", "Provincetown Harbor") ~ NA_character_,
      TRUE ~ str_extract(Identifier, "(?<=\\.).*(?=@)")       # A/B/C for NCBN seagrass
    )
  ) %>%
  filter(Month %in% c(5, 6, 7, 8, 9))

# Passing years (unchanged, but robust map_dbl)
passing_years <- seagrass_temp %>%
  group_by(park_code, Name, Transect, Year, Month) %>%
  summarise(date_count = n_distinct(Date), .groups = "drop") %>%
  group_by(park_code, Name, Transect, Year) %>%
  nest() %>%
  mutate(
    may_days = map_dbl(data, ~ { d <- filter(.x, Month == 5); if (nrow(d) == 0) 0 else sum(d$date_count, na.rm = TRUE) }),
    jun_days = map_dbl(data, ~ { d <- filter(.x, Month == 6); if (nrow(d) == 0) 0 else sum(d$date_count, na.rm = TRUE) }),
    jul_days = map_dbl(data, ~ { d <- filter(.x, Month == 7); if (nrow(d) == 0) 0 else sum(d$date_count, na.rm = TRUE) }),
    aug_days = map_dbl(data, ~ { d <- filter(.x, Month == 8); if (nrow(d) == 0) 0 else sum(d$date_count, na.rm = TRUE) }),
    sep_days = map_dbl(data, ~ { d <- filter(.x, Month == 9); if (nrow(d) == 0) 0 else sum(d$date_count, na.rm = TRUE) }),
    months_check   = rowSums(cbind(may_days >= 10,
                                   jun_days >= 10,
                                   jul_days >= 10,
                                   aug_days >= 10,
                                   sep_days >= 10)) >= 3,
    coverage_check = (may_days + jun_days + jul_days + aug_days + sep_days) >= 122.4,
    all_check      = months_check & coverage_check
  ) %>%
  filter(all_check)

# Filter to passing years
seagrass_temp_filtered <- seagrass_temp %>%
  left_join(passing_years %>% select(park_code, Name, Transect, Year, all_check),
            by = c("park_code", "Name", "Transect", "Year")) %>%
  filter(all_check)

# Monthly means
seasonal_mean_temp <- seagrass_temp_filtered %>%
  group_by(park_code, Name, Transect, Month) %>%
  summarise(mean_temp_month = mean(Value, na.rm = TRUE), .groups = "drop")

# Site-level max of monthly means
max_mean_temp <- seasonal_mean_temp %>%
  group_by(park_code, Name) %>%
  summarise(max_mean_temp = max(mean_temp_month), .groups = "drop")

# Park-level thresholds (across sites per park)
park_thres_temp <- max_mean_temp %>%
  group_by(park_code) %>%
  summarise(
    min_max_mean_temp = min(max_mean_temp),
    max_max_mean_temp = max(max_mean_temp),
    park_thres_temp   = mean(c(min_max_mean_temp, max_max_mean_temp)),
    .groups = "drop"
  )

print(park_thres_temp)

## CHECKS##
# Confirm new sites are in the raw bind
seagrass_temp %>% 
  filter(park_code %in% c("CALO", "CAHA")) %>% 
  distinct(park_code, Name, Identifier) %>% 
  arrange(park_code, Name)

# Make sure they survive the passing-year filter
passing_years %>% 
  filter(park_code %in% c("CALO", "CAHA")) %>% 
  distinct(park_code, Name, Year)

# See monthly means to verify variation
seasonal_mean_temp %>% 
  filter(park_code %in% c("CALO", "CAHA")) %>% 
  arrange(park_code, Name, Month)

# ---- Count passing years per site (per park) ----
n_years_by_site <- seagrass_temp_filtered %>%
  distinct(park_code, Name, Transect, Year) %>%
  count(park_code, Name, Transect, name = "n_passing_years")

# ---- Overall mean (across months) per site ----
site_overall_means <- seasonal_mean_temp %>%
  group_by(park_code, Name, Transect) %>%
  summarise(mean_site_temp = mean(mean_temp_month, na.rm = TRUE), .groups = "drop")

# ---- Max of monthly means per site ----
site_max_monthly_mean <- seasonal_mean_temp %>%
  group_by(park_code, Name, Transect) %>%
  summarise(max_monthly_mean = max(mean_temp_month, na.rm = TRUE), .groups = "drop")

# ---- Monthly means pivoted wide for easy reporting (May–Sep columns) ----
site_monthly_wide <- seasonal_mean_temp %>%
  mutate(Month_abbr = month.abb[Month]) %>%
  select(park_code, Name, Transect, Month_abbr, mean_temp_month) %>%
  pivot_wider(names_from = Month_abbr, values_from = mean_temp_month) %>%
  arrange(park_code, Name, Transect)

# ---- Final site summary table ----
site_summary <- site_monthly_wide %>%
  left_join(site_overall_means,      by = c("park_code", "Name", "Transect")) %>%
  left_join(site_max_monthly_mean,   by = c("park_code", "Name", "Transect")) %>%
  left_join(n_years_by_site,         by = c("park_code", "Name", "Transect")) %>%
  arrange(park_code, Name, Transect)

print(site_summary, n = Inf)

# ---- Site averages (concise view) ----
site_averages <- site_summary %>%
  select(
    park_code, Name, Transect,
    mean_site_temp,          # overall mean across May–Sep monthly means
    max_monthly_mean,        # hottest monthly mean across passing years
    n_passing_years          # number of years meeting coverage rule
  ) %>%
  arrange(park_code, desc(mean_site_temp), Name)

print(site_averages, n = Inf)

# --- Build park-level min/max/threshold (same logic as your current code) ---
park_thres_temp <- max_mean_temp %>%
  group_by(park_code) %>%
  summarise(
    min_max_mean_temp = min(max_mean_temp),
    max_max_mean_temp = max(max_mean_temp),
    park_thres_temp   = mean(c(min_max_mean_temp, max_max_mean_temp)),
    .groups = "drop"
  )

# --- Attach those park-level values to every site in the park ---
park_thresholds_with_sites <- max_mean_temp %>%
  left_join(park_thres_temp, by = "park_code") %>%
  # optional helpers to flag which sites are the park min or max
  mutate(
    is_park_min_site = max_mean_temp == min_max_mean_temp,
    is_park_max_site = max_mean_temp == max_max_mean_temp
  ) %>%
  arrange(park_code, desc(max_mean_temp), Name)

# --- Print the full list (all sites, all parks) ---
print(park_thresholds_with_sites, n = Inf)

library(ggplot2)

ggplot(park_thresholds_with_sites,
       aes(x = reorder(Name, max_mean_temp), y = max_mean_temp,
           fill = case_when(
             is_park_max_site ~ "Park Max Site",
             is_park_min_site ~ "Park Min Site",
             TRUE ~ "Other Site"
           ))) +
  geom_col() +
  geom_hline(aes(yintercept = park_thres_temp), linetype = "dashed", color = "black") +
  facet_wrap(~ park_code, scales = "free_x") +
  scale_fill_manual(values = c("Park Max Site" = "#d73027", "Park Min Site" = "#4575b4", "Other Site" = "#aaaaaa")) +
  labs(x = "Site", y = "Site Max Monthly Mean Temp (°C)",
       fill = NULL,
       title = "Site-level maxima vs. park threshold (min–max average)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
