### Rim Fire Code ###
# code to add Rim Fire to plots
## some figures to show Rim vs Primary plots

# Load the libraries
library(tidyverse)
library(tmap)
library(sf)
library(dplyr)

# Load plot data
plot_beg <- read_csv("Plot_beg_UTMs_66sr22.csv")

# Add a new column `plotNum` to `plot_beg` with sequential numbers from 1 to 59
plot_beg <- plot_beg %>%
  mutate(plotNum = 1:59)

# Drop rows with plotNum 7, 8, and 25
plot_beg <- plot_beg %>%
  filter(!plotNum %in% c(7, 8, 25))


# Reorder plotNum sequentially from 1 to 56
plot_beg <- plot_beg %>%
  mutate(plotNum = row_number())

# Subset data by UTM zone
plot_beg_zone10 <- plot_beg %>% filter(utm_zone == 10)
plot_beg_zone11 <- plot_beg %>% filter(utm_zone == 11)

# Convert UTM Zone 10 to an sf object with appropriate EPSG
epsg_zone10 <- ifelse(unique(plot_beg_zone10$datum) == "NAD83", 26910, 32610)
plot_beg_zone10_sf <- st_as_sf(plot_beg_zone10, coords = c("plot_beg_UTM_E", "plot_beg_UTM_N"), crs = epsg_zone10)

# Convert UTM Zone 11 to an sf object with appropriate EPSG
epsg_zone11 <- ifelse(unique(plot_beg_zone11$datum) == "NAD83", 26911, 32611)
plot_beg_zone11_sf <- st_as_sf(plot_beg_zone11, coords = c("plot_beg_UTM_E", "plot_beg_UTM_N"), crs = epsg_zone11)

# Reproject both zones to WGS84 (EPSG: 4326) and combine them
plot_beg_combined <- rbind(
  st_transform(plot_beg_zone10_sf, crs = 4326),
  st_transform(plot_beg_zone11_sf, crs = 4326)
)


# Load additional dataset for WPBR presence and climate data
pilalist_climate_wpbr <- read_csv("pila_report_df.csv")

# Merge with plot_beg_combined on plotID
plot_beg_combined <- plot_beg_combined %>%
  left_join(pilalist_climate_wpbr %>% select(plotID, DBH_cm, alive_status, activeBranchCanker, inactiveBranchCanker, activeBoleCanker, inactiveBoleCanker, wpbr_present, fire_sev, beetles_present, plot_type, total_pila, alive_trees), by = "plotID")


# Group WPBR, Fire Severity, and Beetles by plotID
plot_beg_combined <- plot_beg_combined %>%
  group_by(plotID) %>%
  mutate(
    wpbr_grouped = ifelse(any(wpbr_present == 1), "Present", "Absent"),
    fire_grouped = ifelse(any(fire_sev %in% c(1, 2, 3)), "High", "Low"),
    beetles_grouped = ifelse(any(beetles_present == 1), "Present", "Absent")
  ) %>%
  ungroup()

# Load fire and park boundary shapefiles
rimfireshp <- st_read("mtbs/2013/ca3785712008620130817/ca3785712008620130817_20130714_20140701_burn_bndy.shp")
yoseshp <- st_read("yose_tracts/yose_boundary.shp")

# Set tmap mode to "view" for an interactive map
tmap_mode("view")


# Set tmap options to automatically fix invalid geometries
tmap_options(check.and.fix = TRUE)

# Now, try to plot again
tm_shape(plot_beg_combined) +
  tm_dots(size = 0.05, col = "wpbr_grouped", palette = c("blue", "green"), border.col = "black", popup.vars = c("fire_sev", "beetles_present", "plot_type", "alive_trees")) +
  tm_shape(rimfireshp) +
  tm_borders(col = "red", lwd = 2) +  # Rimfire boundary in red
  tm_shape(yoseshp) +
  tm_borders(col = "black", lwd = 2) +  # Yosemite boundary in black
  tm_basemap("OpenStreetMap") +
  tm_layout(title = "Plot Locations with WPBR Presence", legend.outside = TRUE)


##################### RIM FIRE PLOTS
##########%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Ensure both datasets use the same CRS (WGS84, EPSG: 4326)
plot_beg_combined <- st_transform(plot_beg_combined, crs = 4326)
rimfireshp <- st_transform(rimfireshp, crs = 4326)

# Identify which plots are within the Rim Fire boundary
plots_in_rim_fire <- plot_beg_combined %>%
  mutate(in_rim_fire = st_within(plot_beg_combined, rimfireshp, sparse = FALSE)[,1])

# Filter to see only plots within the Rim Fire boundary
plots_in_rim_fire_within <- plots_in_rim_fire %>% filter(in_rim_fire)

# View the results
print(plots_in_rim_fire_within)

# Ensure both datasets use the same CRS (WGS84, EPSG: 4326)
plot_beg_combined <- st_transform(plot_beg_combined, crs = 4326)
rimfireshp <- st_transform(rimfireshp, crs = 4326)

# Identify plots within the Rim Fire boundary
plots_in_rim_fire <- plot_beg_combined %>%
  mutate(in_rim_fire = st_within(plot_beg_combined, rimfireshp, sparse = FALSE)[,1])

# Filter to keep only plots within the Rim Fire boundary
plots_in_rim_fire_within <- plots_in_rim_fire %>% filter(in_rim_fire)

# Set tmap mode to "view" for an interactive map
tmap_mode("view")

# Create the map with only plots within the Rim Fire boundary
tm_shape(plots_in_rim_fire_within) +
  tm_dots(size = 0.05, col = "wpbr_grouped", palette = c("blue", "green"), border.col = "black", popup.vars = c("fire_sev", "beetles_present", "plot_type", "alive_trees")) +
  tm_shape(rimfireshp) +
  tm_borders(col = "red", lwd = 2) +  # Rim Fire boundary in red
  tm_basemap("OpenStreetMap") +
  tm_layout(title = "Plots Within the Rim Fire Boundary", legend.outside = TRUE)

# Create a list of plotIDs within the Rim Fire boundary
plotIDs_in_rim_fire <- plots_in_rim_fire_within %>%
  pull(plotID)

# Print the list of plotIDs
print(plotIDs_in_rim_fire)

######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###########
######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###########
######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###########
### Rim Fire vs Primary plots ###
######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###########
######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###########
######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###########

# plots within the rim fire
rim_fire_plots <- plots_in_rim_fire_within

### Now we want to do extent, severity, and infection rate analysis

######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###########
######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###########
# Calculate Extent for Rim Fire plots
### Formula: Extent = Number of plots with at least one infection / Total number of plots surveyed (within rim fire)

# Group by plotID in rim_fire_plots to determine if each plot has any infection
rim_fire_infection_summary <- rim_fire_plots %>%
  group_by(plotID) %>%
  summarize(has_infection = any(wpbr_present == 1)) %>%  # TRUE if any row in the plot has wpbr_present == 1
  ungroup()

# Calculate the total number of unique plots within the Rim Fire boundary
total_rim_fire_plots <- n_distinct(rim_fire_plots$plotID)

# Calculate the number of plots with infections within the Rim Fire boundary
infected_rim_fire_plots <- rim_fire_infection_summary %>%
  filter(has_infection) %>%
  nrow()

# Calculate the extent of infection as a percentage for Rim Fire plots
extent_infection_rim_fire <- (infected_rim_fire_plots / total_rim_fire_plots) * 100

# Print the results for Rim Fire plots
cat("Extent of infection in Rim Fire plots:", extent_infection_rim_fire, "%\n")
cat("Infected plots in Rim Fire:", infected_rim_fire_plots, "\n")
cat("Total plots in Rim Fire:", total_rim_fire_plots, "\n")

# Calculate Extent for Primary plots
### Formula: Extent = Number of plots with at least one infection / Total number of plots surveyed (OUTSIDE rim fire)
primary_plots <- plots_in_rim_fire

# Ensure both datasets use the same CRS (WGS84, EPSG: 4326)
primary_plots <- st_transform(primary_plots, crs = 4326)
rimfireshp <- st_transform(rimfireshp, crs = 4326)

# Identify plots within the Rim Fire boundary
plots_within_rim_fire <- primary_plots %>%
  filter(st_within(., rimfireshp, sparse = FALSE)[,1])

# Exclude plots within the Rim Fire boundary to get only the outside plots
plots_outside_rim_fire <- primary_plots %>%
  filter(!plotID %in% plots_within_rim_fire$plotID)

# Now you can calculate the infection extent for only the primary plots outside the Rim Fire boundary
# Group by plotID in plots_outside_rim_fire to determine if each plot has any infection
outside_infection_summary <- plots_outside_rim_fire %>%
  group_by(plotID) %>%
  summarize(has_infection = any(wpbr_present == 1)) %>%  # TRUE if any row in the plot has wpbr_present == 1
  ungroup()

# Calculate the total number of unique plots outside the Rim Fire boundary
total_outside_rim_fire_plots <- n_distinct(plots_outside_rim_fire$plotID)

# Calculate the number of plots with infections outside the Rim Fire boundary
infected_outside_rim_fire_plots <- outside_infection_summary %>%
  filter(has_infection) %>%
  nrow()

# Calculate the extent of infection as a percentage for plots outside the Rim Fire
extent_infection_outside_rim_fire <- (infected_outside_rim_fire_plots / total_outside_rim_fire_plots) * 100

# Print the results for plots outside the Rim Fire boundary
cat("Extent of infection in plots outside Rim Fire:", extent_infection_outside_rim_fire, "%\n")
cat("Infected plots outside Rim Fire:", infected_outside_rim_fire_plots, "\n")
cat("Total plots outside Rim Fire:", total_outside_rim_fire_plots, "\n")

######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###########
######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###########
# Calculate Severity for Rim Fire plots and primary plots
### Formula: Severity = cs + (50 - DBH inches.) / 5
#### Canker Severity (cs) ranking: cs 0 = no branch or bole cankers
# cs 1 = 1 branch canker
# cs 2 = 2 branch cankers
# cs 3 = 3 - 4 branch cankers
# cs 4 = 5 or more branch cankers
# cs 5 = 1 or more bole cankers

# Step 1: Convert DBH from cm to inches
rim_fire_plots <- rim_fire_plots %>%
  mutate(DBH_in = DBH_cm * 0.393701)  # Convert cm to inches

# Step 2: Cap DBH at 50 for the severity formula
rim_fire_plots <- rim_fire_plots %>%
  mutate(DBH_sevformula = ifelse(DBH_in > 50, 50, DBH_in))

# View the updated data with capped DBH
print(rim_fire_plots %>% select(plotID, DBH_cm, DBH_in, DBH_sevformula))

# Step 3: Assign `cs` based on canker severity ranking
rim_fire_plots <- rim_fire_plots %>%
  mutate(
    cs = case_when(
      activeBranchCanker == 0 & activeBoleCanker == 0 ~ 0,  # No cankers
      activeBranchCanker == 1 & activeBoleCanker == 0 ~ 1,  # 1 branch canker
      activeBranchCanker == 2 & activeBoleCanker == 0 ~ 2,  # 2 branch cankers
      activeBranchCanker >= 3 & activeBranchCanker <= 4 & activeBoleCanker == 0 ~ 3,  # 3-4 branch cankers
      activeBranchCanker >= 5 & activeBoleCanker == 0 ~ 4,  # 5 or more branch cankers
      activeBoleCanker >= 1 ~ 5  # 1 or more bole cankers
    )
  )

# Step 4: Calculate Severity using the updated formula
rim_fire_plots <- rim_fire_plots %>%
  mutate(severity = if_else(cs == 0, 0, (cs + (50 - DBH_sevformula)) / 5))

# View the final data with severity
print(rim_fire_plots %>% select(plotID, DBH_in, DBH_sevformula, cs, severity))


# Calculate Severity for Primary plots

# Step 1: Convert DBH from cm to inches
primary_plots <- primary_plots %>%
  mutate(DBH_in = DBH_cm * 0.393701)  # Convert cm to inches

# Step 2: Cap DBH at 50 for the severity formula
primary_plots <- primary_plots %>%
  mutate(DBH_sevformula = ifelse(DBH_in > 50, 50, DBH_in))

# View the updated data with capped DBH
print(primary_plots %>% select(plotID, DBH_cm, DBH_in, DBH_sevformula))

# Step 3: Assign `cs` based on canker severity ranking
primary_plots <- primary_plots %>%
  mutate(
    cs = case_when(
      activeBranchCanker == 0 & activeBoleCanker == 0 ~ 0,  # No cankers
      activeBranchCanker == 1 & activeBoleCanker == 0 ~ 1,  # 1 branch canker
      activeBranchCanker == 2 & activeBoleCanker == 0 ~ 2,  # 2 branch cankers
      activeBranchCanker >= 3 & activeBranchCanker <= 4 & activeBoleCanker == 0 ~ 3,  # 3-4 branch cankers
      activeBranchCanker >= 5 & activeBoleCanker == 0 ~ 4,  # 5 or more branch cankers
      activeBoleCanker >= 1 ~ 5  # 1 or more bole cankers
    )
  )

# Step 4: Calculate Severity using the updated formula
primary_plots <- primary_plots %>%
  mutate(severity = if_else(cs == 0, 0, (cs + (50 - DBH_sevformula)) / 5))

# View the final data with severity
print(primary_plots %>% select(plotID, DBH_in, DBH_sevformula, cs, severity))


###################
#################################################

# Calculate extent for Rim Fire plots if not already done
rim_fire_infection_summary <- rim_fire_plots %>%
  group_by(plotID) %>%
  summarize(has_infection = any(wpbr_present == 1)) %>%
  ungroup()

total_rim_fire_plots <- n_distinct(rim_fire_plots$plotID)
infected_rim_fire_plots <- rim_fire_infection_summary %>%
  filter(has_infection) %>%
  nrow()

extent_infection_rim_fire <- (infected_rim_fire_plots / total_rim_fire_plots) * 100

# Calculate extent for Primary plots outside the Rim Fire boundary if not already done
outside_infection_summary <- primary_plots %>%
  group_by(plotID) %>%
  summarize(has_infection = any(wpbr_present == 1)) %>%
  ungroup()

total_primary_plots <- n_distinct(primary_plots$plotID)
infected_primary_plots <- outside_infection_summary %>%
  filter(has_infection) %>%
  nrow()

extent_infection_primary <- (infected_primary_plots / total_primary_plots) * 100

# Create a data frame with extent values for both plot types
extent_data <- data.frame(
  Plot_Type = c("Rim Fire", "Primary"),
  Extent = c(extent_infection_rim_fire, extent_infection_primary)
)

### VISUAL
# Load necessary packages
library(ggplot2)
library(viridis)


#########
# Add a `Plot_Type` column to each dataframe
rim_fire_plots <- rim_fire_plots %>% mutate(Plot_Type = "Rim Fire")
primary_plots <- primary_plots %>% mutate(Plot_Type = "Primary")

# Combine the two data frames
severity_data <- bind_rows(
  rim_fire_plots %>% select(plotID, severity, Plot_Type),
  primary_plots %>% select(plotID, severity, Plot_Type)
)


# Filter out rows with zero severity
severity_data_non_zero <- severity_data %>%
  filter(severity > 0)



######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###########
######$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###########
# Calculate Infection Rate for Rim Fire plots and primary plots
### Formula: Infection Rate = number of trees in the plot with infection / total number of trees in the plot

# Calculate infection rate for Rim Fire plots
rim_fire_infection_rate <- rim_fire_plots %>%
  group_by(plotID) %>%
  summarize(
    total_trees = n(),  # Total number of trees in each plot
    infected_trees = sum(wpbr_present == 1),  # Count of infected trees
    infection_rate = infected_trees / total_trees * 100  # Infection rate as a percentage
  ) %>%
  ungroup()

# View the infection rate data for Rim Fire plots
print(rim_fire_infection_rate)

# Calculate infection rate for Primary plots
primary_infection_rate <- primary_plots %>%
  group_by(plotID) %>%
  summarize(
    total_trees = n(),  # Total number of trees in each plot
    infected_trees = sum(wpbr_present == 1),  # Count of infected trees
    infection_rate = infected_trees / total_trees * 100  # Infection rate as a percentage
  ) %>%
  ungroup()

# View the infection rate data for Primary plots
print(primary_infection_rate)

# Add Plot_Type column to each data frame
rim_fire_infection_rate <- rim_fire_infection_rate %>%
  mutate(Plot_Type = "Rim Fire")

primary_infection_rate <- primary_infection_rate %>%
  mutate(Plot_Type = "Primary")

# Combine both data frames
infection_rate_data <- bind_rows(rim_fire_infection_rate, primary_infection_rate)



### MPB Attack Analysis in Rim Fire vs Primary Plots ###

# Group by plotID in Rim Fire plots to determine if each plot has any MPB attack
rim_fire_mpb_summary <- rim_fire_plots %>%
  group_by(plotID) %>%
  summarize(has_mpb_attack = any(beetles_present == 1)) %>%  # TRUE if any row in the plot has mpb_attack == 1
  ungroup()

# Calculate the number of unique plots within the Rim Fire boundary
total_rim_fire_plots <- n_distinct(rim_fire_plots$plotID)

# Calculate the number of plots with MPB attacks within the Rim Fire boundary
mpb_rim_fire_plots <- rim_fire_mpb_summary %>%
  filter(has_mpb_attack) %>%
  nrow()

# Calculate the proportion of Rim Fire plots with MPB attacks
mpb_extent_rim_fire <- (mpb_rim_fire_plots / total_rim_fire_plots) * 100

# Print results for Rim Fire plots
cat("MPB attack extent in Rim Fire plots:", mpb_extent_rim_fire, "%\n")
cat("Rim Fire plots with MPB attacks:", mpb_rim_fire_plots, "\n")
cat("Total Rim Fire plots:", total_rim_fire_plots, "\n")


# Repeat for Primary plots
# Ensure CRS is consistent for spatial operations (WGS84, EPSG: 4326)
primary_plots <- st_transform(primary_plots, crs = 4326)
rimfireshp <- st_transform(rimfireshp, crs = 4326)

# Identify plots outside the Rim Fire boundary
plots_within_rim_fire <- primary_plots %>%
  filter(st_within(., rimfireshp, sparse = FALSE)[,1])

plots_outside_rim_fire <- primary_plots %>%
  filter(!plotID %in% plots_within_rim_fire$plotID)

# Group by plotID in Primary plots outside Rim Fire to check for MPB attacks
primary_mpb_summary <- plots_outside_rim_fire %>%
  group_by(plotID) %>%
  summarize(has_mpb_attack = any(beetles_present == 1)) %>%  # TRUE if any row in the plot has mpb_attack == 1
  ungroup()

# Calculate the total number of unique plots outside the Rim Fire boundary
total_primary_plots <- n_distinct(plots_outside_rim_fire$plotID)

# Calculate the number of plots with MPB attacks outside the Rim Fire boundary
mpb_primary_plots <- primary_mpb_summary %>%
  filter(has_mpb_attack) %>%
  nrow()

# Calculate the proportion of Primary plots with MPB attacks
mpb_extent_primary <- (mpb_primary_plots / total_primary_plots) * 100

# Print results for Primary plots
cat("MPB attack extent in Primary plots:", mpb_extent_primary, "%\n")
cat("Primary plots with MPB attacks:", mpb_primary_plots, "\n")
cat("Total Primary plots:", total_primary_plots, "\n")



# Create a data frame for the plot
mpb_data <- data.frame(
  plot_type = c("Rim Fire", "Primary"),
  mpb_extent = c(mpb_extent_rim_fire, mpb_extent_primary)  # Use percentages calculated earlier
)



# Create a data frame with counts of MPB attacks for each plot type
mpb_data_counts <- data.frame(
  plot_type = c("Rim Fire", "Primary"),
  mpb_count = c(mpb_rim_fire_plots, mpb_primary_plots)  # Use counts of plots with MPB attacks
)

pila_report_df <- pilalist_climate_wpbr
# Calculate the number of individual trees with MPB attacks by plot type
mpb_tree_counts <- pila_report_df %>%
  filter(beetles_treespresent == 1) %>%  # Filter for trees with MPB attacks
  group_by(plot_type) %>%  # Group by plot type (Rim Fire or Primary)
  summarize(mpb_tree_count = n())  # Count the number of trees with MPB attacks in each plot type

# View results
print(mpb_tree_counts)

# Calculate the count and proportion of trees with MPB attacks by plot type
mpb_tree_summary <- pila_report_df %>%
  group_by(plot_type) %>%
  summarize(
    mpb_tree_count = sum(beetles_treespresent == 1),  # Count of trees with MPB attacks
    total_trees = n()  # Total number of trees in each plot type
  ) %>%
  mutate(mpb_tree_proportion = mpb_tree_count / total_trees)  # Proportion of trees with MPB attacks


# Define the list of plotIDs that correspond to Rim Fire plots
rim_fire_plot_ids <- c(5, 14, 15, 16, 17, 21, 24, 25, 26, 30, 31, 32, 34, 37, 39, 44, 50, 55, 57, 58, 59, 64, 65)  # Replace with actual plot IDs for Rim Fire plots

# Add a new column "plot_type" to classify each plot as "Rim Fire" or "Primary"
pila_report_df <- pila_report_df %>%
  mutate(
    plot_rim = ifelse(plotID %in% rim_fire_plot_ids, "Rim Fire", "Primary")
  )



# Calculate the count and proportion of trees with MPB attacks by plot type (Primary vs Rim Fire)
mpb_tree_summary <- pila_report_df %>%
  filter(plot_rim %in% c("Primary", "Rim Fire")) %>%  # Filter for Primary and Rim Fire plots
  group_by(plot_rim) %>%
  summarize(
    mpb_tree_count = sum(beetles_treespresent == 1),  # Count of trees with MPB attacks
    total_trees = n()  # Total number of trees in each plot type
  ) %>%
  mutate(mpb_tree_proportion = mpb_tree_count / total_trees)  # Proportion of trees with MPB attacks

# Plot 1: Actual number of trees with MPB attacks
plot_count <- ggplot(mpb_tree_summary, aes(x = plot_rim, y = mpb_tree_count, fill = plot_rim)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_viridis_d(name = "Plot Type", option = "E") +
  labs(
    title = "Number of Trees with MPB Attacks by Plot Type",
    x = "Plot Type",
    y = "Number of Trees with MPB Attacks"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

plot_count
# Plot 2: Proportion of trees with MPB attacks
plot_proportion <- ggplot(mpb_tree_summary, aes(x = plot_rim, y = mpb_tree_proportion, fill = plot_rim)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_viridis_d(name = "Plot Type", option = "E") +
  labs(
    title = "Proportion of Trees with MPB Attacks by Plot Type",
    x = "Plot Type",
    y = "Proportion of Trees with MPB Attacks"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

plot_proportion
# Combine the two plots side by side
plot_count + plot_proportion


# Count the number of trees in Rim Fire vs Primary plots
tree_counts <- pila_report_df %>%
  group_by(plot_rim) %>%  # Group by plot type (Rim Fire or Primary)
  summarize(total_trees = n())  # Count the number of trees in each plot type

# View results
print(tree_counts)

1132 + 598

# Filter for live trees and count by plot type
live_tree_counts <- pila_report_df %>%
  filter(alive_status == "Alive") %>%  # Filter only live trees
  group_by(plot_rim) %>%  # Group by plot type (Rim Fire or Primary)
  summarize(total_live_trees = n())  # Count the number of live trees in each plot type

# View results
print(live_tree_counts)

1016 + 457


# Calculate the min and max severity values for each plot_rim group
severity_stats <- pila_report_df %>%
  group_by(plot_rim) %>%
  summarize(min_severity = min(severity, na.rm = TRUE),
            max_severity = max(severity, na.rm = TRUE))

# Display the results
print(severity_stats)

# Count the number of trees with infections in Rim Fire and Primary plots
infection_counts <- pila_report_df %>%
  filter(wpbr_present == 1) %>%
  filter(alive_status == "Alive") %>%# Assuming 1 indicates presence of infection
  group_by(plot_rim) %>%
  summarize(infected_trees = n())

# Display the results
print(infection_counts)



57/1473 * 100

