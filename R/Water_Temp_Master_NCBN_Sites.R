###############################################################################
# Generate all figures for ASIS, FIIS, CACO with a single run
# - Uses Site_Summary_Master_b.xlsx, sheet "Site_Year"
# - Ensures consistent color palette and legend order across all plots
###############################################################################

# Libraries (load once)
library(tidyverse)
library(readxl)
library(ggrepel)
library(glue)
library(fs)
library(scales)   # for label_number
library(here) # use to load data from relative paths

# -------------------------------
# 0) GLOBAL STYLE + PALETTE
# -------------------------------
theme_set(theme_classic(base_size = 14))

# Locked legend order for Park_Code
park_levels <- c("ASIS", "FIIS", "CACO")

# Consistent colors and shapes everywhere
park_pal    <- c("ASIS"="#1f77b4","FIIS"="#ff7f00","CACO"="#2ca02c")
park_shapes <- c("ASIS"=16,"FIIS"=17,"CACO"=15)

# Accent colors for thresholds/extreme markers (not mapped to Park_Code)
grey_grid     <- "grey40"
accent_red    <- "#d62728"
accent_blue   <- "#1f78b4"
accent_purple <- "#8b3fb9"

# Reusable scales—use these in every plot that maps color/fill/shape to Park_Code
scale_park_color <- function(...) scale_color_manual(values = park_pal, breaks = park_levels, ...)
scale_park_fill  <- function(...) scale_fill_manual(values  = park_pal, breaks = park_levels, ...)
scale_park_shape <- function(...) scale_shape_manual(values = park_shapes, breaks = park_levels, ...)

# -------------------------------
# 1) READ + STANDARDIZE DATA ONCE
# -------------------------------
dat <- read_excel(here::here("data", "Site_Summary_Master_b.xlsx"), sheet = "Site_Year") %>%
  rename_with(~ gsub(" ", "_", .)) %>%
  mutate(
    Park_Code              = toupper(Park_Code),
    Park_Code              = factor(Park_Code, levels = park_levels), # lock legend order
    Mean_HDH_daily         = suppressWarnings(as.numeric(Mean_HDH_daily)),
    Mean_CDH_daily         = suppressWarnings(as.numeric(Mean_CDH_daily)),
    `%_abv_thres`          = suppressWarnings(as.numeric(`%_abv_thres`)),
    Number_warming_events  = suppressWarnings(as.numeric(Number_warming_events)),
    Year                   = suppressWarnings(as.integer(Year)),
    Transect               = toupper(gsub("[^A-Za-z]", "", trimws(Transect)))
  )

# -------------------------------
# 2) HELPER FUNCTIONS
# -------------------------------

# Site-year (avg across transects) -> Site-level (avg across years)
make_site_level <- function(df, metrics = c("Mean_HDH_daily","Mean_CDH_daily","%_abv_thres")) {
  df %>%
    dplyr::group_by(Park_Code, Site_Name, Year) %>%
    dplyr::summarise(
      HDH_year     = if ("Mean_HDH_daily" %in% metrics) mean(Mean_HDH_daily, na.rm = TRUE) else NA_real_,
      CDH_year     = if ("Mean_CDH_daily" %in% metrics) mean(Mean_CDH_daily, na.rm = TRUE) else NA_real_,
      pct_hot_year = if ("%_abv_thres"    %in% metrics) mean(`%_abv_thres`,    na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    dplyr::group_by(Park_Code, Site_Name) %>%
    dplyr::summarise(
      mean_HDH = mean(HDH_year,     na.rm = TRUE),
      mean_CDH = mean(CDH_year,     na.rm = TRUE),
      pct_hot  = mean(pct_hot_year, na.rm = TRUE),
      n_years  = dplyr::n_distinct(Year),
      .groups  = "drop"
    ) %>%
    dplyr::mutate(Label = paste0(Site_Name, " (n=", n_years, ")"))
}

# Flag extreme years per site (e.g., 90th percentile of metric within each site)
flag_extreme_by_site <- function(df, value_col, prob = 0.9, flag_name = "Extreme") {
  df %>%
    dplyr::group_by(Site_Name) %>%
    dplyr::mutate(
      thresh = stats::quantile(.data[[value_col]], prob, na.rm = TRUE),
      "{flag_name}" := .data[[value_col]] >= .data[["thresh"]]
    ) %>%
    dplyr::ungroup()
}

# -------------------------------
# 3) PER-PARK PLOT FUNCTIONS
# -------------------------------

# A) High Heat Years (HDH) by site (facetted), one park
plot_hdh_extreme_by_site <- function(df, park_code) {
  site_year <- df %>%
    dplyr::filter(Park_Code == park_code) %>%
    dplyr::group_by(Site_Name, Year) %>%
    dplyr::summarise(Mean_HDH = mean(Mean_HDH_daily, na.rm = TRUE), .groups = "drop") %>%
    flag_extreme_by_site(value_col = "Mean_HDH", prob = 0.9, flag_name = "Extreme")
  
  ggplot(site_year, aes(x = Year, y = Mean_HDH)) +
    geom_line(color = "black", linewidth = 1) +
    geom_point(size = 2) +
    geom_point(data = subset(site_year, Extreme), color = accent_red, size = 3.5) +
    geom_text_repel(
      data = subset(site_year, Extreme),
      aes(label = Year), color = accent_red, size = 4, show.legend = FALSE,
      nudge_x = 0.4, nudge_y = 0.6, hjust = 0, direction = "y",
      box.padding = 0.3, point.padding = 0.2
    ) +
    geom_hline(aes(yintercept = thresh), linetype = "dashed", color = accent_red) +
    facet_wrap(~ Site_Name, ncol = 1, scales = "free_y") +
    scale_y_continuous(labels = label_number(accuracy = 1),
                       expand = expansion(mult = c(0.1, 0.2))) +
    scale_x_continuous(breaks = seq(min(site_year$Year, na.rm = TRUE), max(site_year$Year, na.rm = TRUE), by = 2)) +
    labs(
      title    = glue("High Heat Years by Site ({park_code})"),
      subtitle = "Red dashed line = site-specific threshold; red points = extreme heat years",
      x = "Year", y = "Mean Heating Degree Hours per Day"
    )
}

# B) Reduced Cooling Years (CDH) by site (facetted), one park
plot_cdh_lowcool_by_site <- function(df, park_code) {
  site_year <- df %>%
    dplyr::filter(Park_Code == park_code) %>%
    dplyr::group_by(Site_Name, Year) %>%
    dplyr::summarise(Mean_CDH = mean(Mean_CDH_daily, na.rm = TRUE), .groups = "drop") %>%
    flag_extreme_by_site(value_col = "Mean_CDH", prob = 0.9, flag_name = "Low_Cooling")
  
  ggplot(site_year, aes(x = Year, y = Mean_CDH)) +
    geom_line(color = "black", linewidth = 1) +
    geom_point(size = 2) +
    geom_point(data = subset(site_year, Low_Cooling), color = accent_blue, size = 3.5) +
    geom_text_repel(
      data = subset(site_year, Low_Cooling),
      aes(label = Year), color = accent_blue, size = 4, show.legend = FALSE,
      nudge_x = 0.4, nudge_y = 0.6, hjust = 0, direction = "y",
      box.padding = 0.3, point.padding = 0.2
    ) +
    geom_hline(aes(yintercept = thresh), linetype = "dashed", color = accent_blue) +
    facet_wrap(~ Site_Name, ncol = 1, scales = "free_y") +
    scale_y_continuous(labels = label_number(accuracy = 1),
                       expand = expansion(mult = c(0.1, 0.2))) +
    scale_x_continuous(breaks = seq(min(site_year$Year, na.rm = TRUE), max(site_year$Year, na.rm = TRUE), by = 2)) +
    labs(
      title    = glue("Reduced Cooling Years by Site ({park_code})"),
      subtitle = "Blue dashed line = site-specific threshold; blue points = low cooling years",
      x = "Year", y = "Mean Cooling Degree Hours per Day"
    )
}

# C) Double-Stress years (HDH high & CDH high), one park
plot_double_stress_by_site <- function(df, park_code) {
  site_year <- df %>%
    dplyr::filter(Park_Code == park_code) %>%
    dplyr::group_by(Site_Name, Year) %>%
    dplyr::summarise(
      Mean_HDH = mean(Mean_HDH_daily, na.rm = TRUE),
      Mean_CDH = mean(Mean_CDH_daily, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(Site_Name) %>%
    dplyr::mutate(
      hdh_thresh = stats::quantile(Mean_HDH, 0.9, na.rm = TRUE),
      cdh_thresh = stats::quantile(Mean_CDH, 0.9, na.rm = TRUE),
      High_Heat   = Mean_HDH >= hdh_thresh,
      Low_Cooling = Mean_CDH >= cdh_thresh,
      Double_Stress = High_Heat & Low_Cooling
    ) %>% dplyr::ungroup()
  
  ggplot(site_year, aes(x = Year, y = Mean_HDH)) +
    geom_line(color = "black", linewidth = 1) +
    geom_point(size = 2) +
    geom_point(data = subset(site_year, Double_Stress), color = accent_purple, size = 4) +
    geom_text_repel(
      data = subset(site_year, Double_Stress),
      aes(label = Year), color = accent_purple, size = 4, show.legend = FALSE,
      nudge_x = 0.4, nudge_y = 0.6, hjust = 0, direction = "y",
      box.padding = 0.3, point.padding = 0.2
    ) +
    geom_hline(aes(yintercept = hdh_thresh), linetype = "dashed", color = accent_red) +
    facet_wrap(~ Site_Name, ncol = 1, scales = "free_y") +
    scale_y_continuous(labels = label_number(accuracy = 1),
                       expand = expansion(mult = c(0.1, 0.2))) +
    scale_x_continuous(breaks = seq(min(site_year$Year, na.rm = TRUE), max(site_year$Year, na.rm = TRUE), by = 2)) +
    labs(
      title    = glue("Thermal Stress Years by Site ({park_code})"),
      subtitle = "Purple points = years with high heat AND reduced cooling",
      x = "Year", y = "Mean Heating Degree Hours per Day"
    )
}

# -------------------------------
# 4) REGIONAL (ALL-PARKS) PLOT FUNCTIONS
# -------------------------------

# D) Site-level thermal stress across years (lines colored by Park_Code)
plot_sitelevel_thermal_stress_lines <- function(df) {
  site_year <- df %>%
    dplyr::filter(Park_Code %in% park_levels) %>%
    dplyr::group_by(Park_Code, Site_Name, Year) %>%
    dplyr::summarise(Mean_HDH = mean(Mean_HDH_daily, na.rm = TRUE), .groups = "drop")
  
  threshold <- stats::quantile(site_year$Mean_HDH, 0.9, na.rm = TRUE)
  
  site_labels <- site_year %>%
    dplyr::group_by(Site_Name) %>%
    dplyr::filter(Year == max(Year, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  ggplot(site_year,
         aes(x = Year, y = Mean_HDH, group = Site_Name, color = Park_Code)) +
    geom_line(alpha = 0.7, linewidth = 1) +
    geom_point(size = 2) +
    geom_text_repel(
      data = site_labels,
      aes(label = Site_Name),
      size = 3.5, direction = "y", hjust = 0,
      nudge_x = 1, box.padding = 0.5, point.padding = 0.3,
      segment.color = "grey60", show.legend = FALSE
    ) +
    geom_point(
      data = subset(site_year, Mean_HDH >= threshold),
      color = accent_red, size = 3
    ) +
    geom_text_repel(
      data = subset(site_year, Mean_HDH >= threshold),
      aes(label = Year),
      size = 3, color = accent_red, show.legend = FALSE
    ) +
    geom_hline(yintercept = threshold, linetype = "dashed", color = accent_red) +
    scale_park_color() +
    scale_x_continuous(breaks = seq(min(site_year$Year, na.rm = TRUE),
                                    max(site_year$Year, na.rm = TRUE), by = 2)) +
    expand_limits(x = max(site_year$Year, na.rm = TRUE) + 8) +
    labs(
      title    = "Site-Level Thermal Stress Across Years",
      subtitle = "Lines = sites; labels show site identity and extreme heat years",
      x = "Year", y = "Mean Heating Degree Hours per Day",
      color = "Park"
    )
}

# E) Thermal stress vs cooling (site-level scatter with size = % hot)
plot_stress_vs_cooling <- function(df) {
  site_level <- make_site_level(df, metrics = c("Mean_HDH_daily","Mean_CDH_daily","%_abv_thres"))
  
  cluster_df <- site_level %>% # create df with only Events and Exposure for fitting k-means
    mutate(id = paste0(Park_Code, "_", Site_Name)) %>%
    column_to_rownames(var = "id") %>%
    ungroup() %>%
    dplyr::select(mean_HDH, mean_CDH)
  
  set.seed(42)
  km_fit <- kmeans(cluster_df, centers = 4, nstart = 2) # fit k-means clusters
  HDH_split <- mean(km_fit$centers[1:4])
  CDH_split <- mean(km_fit$centers[5:8])
  
  plot <- ggplot(site_level, aes(x = mean_HDH, y = mean_CDH, color = Park_Code, size = pct_hot)) +
    geom_point(alpha = 0.9) +
    geom_text_repel(aes(label = Label), size = 4, point.padding = 0.4, box.padding = 0.5, force = 2) +
    geom_vline(xintercept = HDH_split, linetype = "dashed", color = grey_grid) +
    geom_hline(yintercept = CDH_split, linetype = "dashed", color = grey_grid) +
    # expand_limits(x = max(site_level$mean_HDH, na.rm = TRUE) * 1.2,
    #               y = min(site_level$mean_CDH, na.rm = TRUE) * 1.2) +
    scale_park_color() +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 4))) +
    labs(
      title = "Thermal Stress vs Cooling Relief Across Coastal Monitoring Sites",
      subtitle = "Points represent site-level averages across monitoring years",
      x = "Mean Heating Degree Hours per Day (Thermal Stress)",
      y = "Mean Cooling Degree Hours per Day (Thermal Recovery)",
      color = "Park",
      size  = "% Time Above Threshold"
    )
  
  plot_build <- ggplot_build(plot)
  x_limits <- plot_build$layout$panel_params[[1]]$x$continuous_range
  y_limits <- plot_build$layout$panel_params[[1]]$y$continuous_range
  label_down <- 0.10 * diff(y_limits)
  
  plot +
    annotate("text", x = x_limits[1] + ((HDH_split - x_limits[1])/2), y = CDH_split + ((y_limits[2] - CDH_split)/2), vjust = 0.5, hjust = 0.5, label = "Moderately Buffered", size = 4.5, fontface = "bold") +
    annotate("text", x = x_limits[1] + ((HDH_split - x_limits[1])/2), y = y_limits[1] + ((CDH_split - y_limits[1])/2), vjust = 0.5, hjust = 0.5, label = "Thermal Refugia", size = 4.5, fontface = "bold") +
    annotate("text", x = HDH_split + ((x_limits[2] - HDH_split)/2), y = CDH_split + ((y_limits[2] - CDH_split)/2), vjust = 0.5, hjust = 0.5, label = "Thermally Vulnerable", size = 4.5, fontface = "bold") +
    annotate("text", x = HDH_split + ((x_limits[2] - HDH_split)/2), y = y_limits[1] + ((CDH_split - y_limits[1])/2), vjust = 0.5, hjust = 0.5, label = "Thermally Resilient", size = 4.5, fontface = "bold")
  
}


# F) Thermal balance (exposure vs cooling, site-level)
plot_thermal_balance <- function(df) {
  thermal_balance <- df %>%
    dplyr::group_by(Park_Code, Site_Name, Year) %>%
    dplyr::summarise(
      Exposure_year = mean(`%_abv_thres`, na.rm = TRUE),
      Cooling_year  = mean(Mean_CDH_daily,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(Park_Code, Site_Name) %>%
    dplyr::summarise(
      MeanExposure = mean(Exposure_year, na.rm = TRUE),
      MeanCooling  = mean(Cooling_year,  na.rm = TRUE),
      n_years      = dplyr::n_distinct(Year),
      .groups      = "drop"
    ) %>%
    dplyr::mutate(Label = paste0(Site_Name, " (n=", n_years, ")"))
  
  ggplot(thermal_balance, aes(x = MeanExposure, y = MeanCooling, color = Park_Code)) +
    geom_point(size = 4, alpha = 0.9) +
    geom_text_repel(aes(label = Label), size = 3) +
    scale_park_color() +
    labs(
      title    = "Thermal Balance Across Coastal Monitoring Sites",
      subtitle = "Exposure vs cooling using consistent site-level averaging",
      x = "Mean % Time Above Threshold (Thermal Exposure)",
      y = "Mean Cooling Degree Hours (Thermal Recovery)",
      color = "Park"
    )
}

# G) Heat Event Structure (site-level; optional interpretive quadrants)
plot_heat_event_structure_site <- function(df) {
  site_level_events <- df %>%
    dplyr::group_by(Park_Code, Site_Name, Year) %>%
    dplyr::summarise(
      Events_year   = mean(Number_warming_events, na.rm = TRUE),
      Exposure_year = mean(`%_abv_thres`,        na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(Park_Code, Site_Name) %>%
    dplyr::summarise(
      MeanEvents   = mean(Events_year,   na.rm = TRUE),
      MeanExposure = mean(Exposure_year, na.rm = TRUE),
      n_years      = dplyr::n_distinct(Year),
      .groups      = "drop"
    ) %>%
    dplyr::mutate(Label = paste0(Site_Name, " (n=", n_years, ")"))
  
  cluster_df <- site_level_events %>% # create df with only Events and Exposure for fitting k-means
    mutate(id = paste0(Park_Code, "_", Site_Name)) %>%
    column_to_rownames(var = "id") %>%
    ungroup() %>%
    dplyr::select(MeanEvents, MeanExposure)
  
  set.seed(42)
  km_fit <- kmeans(cluster_df, centers = 4, nstart = 2) # fit k-means clusters
  event_split <- mean(km_fit$centers[1:4])
  exposure_split <- mean(km_fit$centers[5:8])
  
  plot <- ggplot(site_level_events, aes(x = MeanEvents, y = MeanExposure, color = Park_Code, shape = Park_Code)) +
    geom_point(size = 4) +
    geom_text_repel(aes(label = Label), size = 4, max.overlaps = 30, point.padding = 0.6, box.padding = 0.7, force = 2) +
    geom_vline(xintercept = event_split,   linetype = "dashed", color = grey_grid) +
    geom_hline(yintercept = exposure_split,linetype = "dashed", color = grey_grid) +
    scale_park_color() +
    scale_park_shape() +
    labs(
      title    = "Thermal Stress Across Coastal Parks",
      subtitle = "Points represent site-level averages across monitoring years",
      x = "Mean Number of Warming Events (Frequency)",
      y = "Mean % Time Above Threshold (Duration)",
      color = "Park", shape = "Park"
    )
  
  plot_build <- ggplot_build(plot)
  x_limits <- plot_build$layout$panel_params[[1]]$x$continuous_range
  y_limits <- plot_build$layout$panel_params[[1]]$y$continuous_range
  
  quad_shift <- 0.15 * diff(y_limits)
  
  
  plot +
    annotate("text",
             x = x_limits[1] + ((event_split - x_limits[1]) / 2),
             y = exposure_split + ((y_limits[2] - exposure_split) / 2) - quad_shift,
             label = "Episodic Heat Waves",
             size = 4, fontface = "bold") +
    annotate("text",
             x = x_limits[1] + ((event_split - x_limits[1]) / 2),
             y = y_limits[1] + ((exposure_split - y_limits[1]) / 2) + quad_shift,
             label = "Low Thermal Stress",
             size = 4, fontface = "bold") +
    annotate("text",
             x = event_split + ((x_limits[2] - event_split) / 2),
             y = exposure_split + ((y_limits[2] - exposure_split) / 2) - quad_shift,
             label = "Sustained Stressed",
             size = 4, fontface = "bold") +
    annotate("text",
             x = event_split + ((x_limits[2] - event_split) / 2),
             y = y_limits[1] + ((exposure_split - y_limits[1]) / 2) + quad_shift,
             label = "Frequent Pulsed Warming",
             size = 4, fontface = "bold")
  
}

# (Optional) H) Thermal balance at TRANSECT level (no interpretation grids)
plot_thermal_balance_transect <- function(df) {
  thermal_balance <- df %>%
    dplyr::filter(Park_Code %in% park_levels) %>%
    dplyr::group_by(Park_Code, Site_Name, Transect) %>%
    dplyr::summarise(
      MeanExposure = mean(`%_abv_thres`,    na.rm = TRUE),
      MeanCooling  = mean(Mean_CDH_daily,   na.rm = TRUE),
      n_years      = dplyr::n(),
      .groups      = "drop"
    ) %>%
    dplyr::mutate(Label = paste(Site_Name, Transect))
  
  ggplot(thermal_balance, aes(x = MeanExposure, y = MeanCooling, color = Park_Code)) +
    geom_point(size = 4) +
    geom_text_repel(aes(label = Label), size = 3) +
    scale_park_color() +
    labs(
      title = "Thermal Balance Across Parks (Transect Level)",
      x = "Mean % Time Above Threshold",
      y = "Mean Cooling Degree Hours",
      color = "Park"
    )
}


# -------------------------------
# 5) OUTPUT: CREATE ALL FIGURES
# -------------------------------
dir_create("figs")

# Per-park plots (loop through ASIS, FIIS, CACO) — no manual filter changes needed
purrr::walk(park_levels, function(pk) {
  message(glue("Generating plots for {pk}..."))
  
  g1 <- plot_hdh_extreme_by_site(dat, pk)
  ggsave(glue("figs/hdh_extreme_{pk}.png"), g1, width = 12, height = 9, dpi = 300)
  
  g2 <- plot_cdh_lowcool_by_site(dat, pk)
  ggsave(glue("figs/cdh_lowcool_{pk}.png"), g2, width = 12, height = 9, dpi = 300)
  
  g3 <- plot_double_stress_by_site(dat, pk)
  ggsave(glue("figs/double_stress_{pk}.png"), g3, width = 12, height = 9, dpi = 300)
})

# Regional (all parks) figures
gr1 <- plot_sitelevel_thermal_stress_lines(dat)
ggsave("figs/sitelevel_thermal_stress_lines_ALL.png", gr1, width = 11, height = 7, dpi = 300)

gr2 <- plot_stress_vs_cooling(dat)
ggsave("figs/stress_vs_cooling_ALL.png", gr2, width = 12, height = 9, dpi = 300)

gr3 <- plot_thermal_balance(dat)
ggsave("figs/thermal_balance_ALL.png", gr3, width = 12, height = 10, dpi = 300)

gr4 <- plot_heat_event_structure_site(dat)
ggsave("figs/heat_event_structure_site_ALL.png", gr4, width = 12, height = 10, dpi = 300)

# Optional: transect-level thermal balance
gr5 <- plot_thermal_balance_transect(dat)
ggsave("figs/thermal_balance_transect_ALL.png", gr5, width = 12, height = 10, dpi = 300)

message("All figures written to ./figs/")
