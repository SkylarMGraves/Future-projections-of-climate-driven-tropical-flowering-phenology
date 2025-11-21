
########### CH3 #########################################
# Load climate data
max_df <- read.csv("Guinnea_Max_GCM.csv")      # dim: [n_months x GCMs]
min_df <- read.csv("Guinnea_Min_GCM.csv")
precip_df <- read.csv("Guinnea_Precip_GCM.csv")

# Load necessary libraries
library(tidyr)
library(dplyr)

# Create time sequence (assumes first row = Jan 2015, monthly data)
time_seq <- seq(as.Date("2015-01-01"), by = "month", length.out = nrow(max_df))

# Pivot each dataset to long format and add time column
max_long <- max_df %>% mutate(Time = time_seq) %>%
  pivot_longer(-Time, names_to = "GCM", values_to = "Max")

min_long <- min_df %>% mutate(Time = time_seq) %>%
  pivot_longer(-Time, names_to = "GCM", values_to = "Min")

precip_long <- precip_df %>% mutate(Time = time_seq) %>%
  pivot_longer(-Time, names_to = "GCM", values_to = "Precip")


# Merge all into a single dataframe with only complete overlap
future_climate <- max_long %>%
  inner_join(min_long, by = c("Time", "GCM")) %>%
  inner_join(precip_long, by = c("Time", "GCM"))


# Define model coefficients (species-specific climate effects on flowering date)
Max_effect_days_per_C <- -4.644
Min_effect_days_per_C <- 4.781
Precip_effect_days_per_mm <- 0.012

# Calculate total flowering shift and extract year
future_climate <- future_climate %>%
  mutate(
    FloweringShift_days = Max * Max_effect_days_per_C +
      Min * Min_effect_days_per_C +
      Precip * Precip_effect_days_per_mm,
    Year = format(Time, "%Y")
  )

# Compute 2015 baseline shift per GCM
baseline_shift <- future_climate %>%
  filter(Year == "2015") %>%
  group_by(GCM) %>%
  summarise(BaselineShift = mean(FloweringShift_days, na.rm = TRUE), .groups = "drop")

# Join baseline back and compute shift relative to 2015
future_climate <- future_climate %>%
  left_join(baseline_shift, by = "GCM") %>%
  mutate(Shift_relative_to_2015 = FloweringShift_days - BaselineShift)


annual_shift <- future_climate %>%
  group_by(GCM, Year) %>%
  summarise(MeanShift = mean(Shift_relative_to_2015, na.rm = TRUE)) %>%
  ungroup()

# Fit the same LOESS model used by geom_smooth
#loess_fit <- loess(MeanShift ~ as.numeric(Year), data = annual_shift)

# Get fitted values for each year
#loess_pred <- data.frame(
#  Year = annual_shift$Year,
#  Fitted_MeanShift = predict(loess_fit)
#)

# One fitted value per year (averaged across GCMs)
#loess_summary <- loess_pred %>%
#  group_by(Year) %>%
#  summarize(Fitted_MeanShift = mean(Fitted_MeanShift, na.rm = TRUE))

# Save to CSV if you want
#write.csv(loess_summary, "Tetrorchidium_didymostemon_Time_Series.csv", row.names = FALSE)





# Create the plot object
#p <- ggplot(annual_shift, aes(x = as.numeric(Year), y = MeanShift, color = GCM)) +
 # geom_line(alpha = 0.5) +
 # geom_smooth(aes(group = 1), method = "loess", color = "black", se = FALSE) +
 # labs(
  #  y = "Flowering Shift (days relative to 2015)",
   # x = "Year",
  #  title = "Stylosanthes erecta Flowering date"
 # ) +
 # theme_minimal()
#p

# Save the plot as a PNG
#ggsave("Stylosanthes_erecta.png", plot = p, width = 10, height = 6, dpi = 300)






# You can also save as PDF
# ggsave("Flowering_Shift_Relative_to_2015.pdf", plot = p, width = 10, height = 6)

library(dplyr)
library(tidyr)

# Step 1: Coerce Year to numeric
annual_shift_clean <- annual_shift %>%
  mutate(Year = as.numeric(as.character(Year)))

# Step 2: Define year ranges around 2020 and 2095
early_range <- 2015:2025
late_range  <- 2090:2100

# Step 3: Filter only years within those ranges
subset_years <- annual_shift_clean %>%
  filter(Year %in% c(early_range, late_range))

# Step 4: Count entries per GCM and year
gcm_year_counts <- subset_years %>%
  group_by(GCM, Year) %>%
  summarise(n = n(), .groups = "drop")

# Step 5: Filter out GCMs that don't have full data across both time windows
gcm_valid <- gcm_year_counts %>%
  group_by(GCM) %>%
  summarise(
    has_early = any(Year %in% early_range),
    has_late  = any(Year %in% late_range),
    .groups = "drop"
  ) %>%
  filter(has_early & has_late) %>%
  pull(GCM)

# Step 6: Average duplicates, compute mean flowering per window, and difference
annual_filtered <- subset_years %>%
  filter(GCM %in% gcm_valid) %>%
  group_by(GCM, Year) %>%
  summarise(MeanShift = mean(MeanShift, na.rm = TRUE), .groups = "drop") %>%
  mutate(TimeWindow = case_when(
    Year %in% early_range ~ "Early",
    Year %in% late_range  ~ "Late"
  )) %>%
  group_by(GCM, TimeWindow) %>%
  summarise(MeanShift_Window = mean(MeanShift, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = TimeWindow, values_from = MeanShift_Window) %>%
  mutate(Shift_future_minus_early = Late - Early)

# Step 7: Add SSP label
annual_filtered <- annual_filtered %>%
  mutate(
    SSP = case_when(
      grepl("_370", GCM) ~ "SSP370",
      grepl("_585", GCM) ~ "SSP585",
      TRUE ~ "Other"
    )
  )

# Step 8: Summary stats
summary_by_ssp <- annual_filtered %>%
  group_by(SSP) %>%
  summarise(
    Mean_change = mean(Shift_future_minus_early, na.rm = TRUE),
    Min_change = min(Shift_future_minus_early, na.rm = TRUE),
    Max_change = max(Shift_future_minus_early, na.rm = TRUE),
    SD_change = sd(Shift_future_minus_early, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Step 9: Show which GCMs were excluded
missing_gcms <- setdiff(unique(annual_shift$GCM), unique(annual_filtered$GCM))

# Output
print(summary_by_ssp)
print(missing_gcms)

