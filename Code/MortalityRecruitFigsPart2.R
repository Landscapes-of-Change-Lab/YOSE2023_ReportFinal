## Mortality and Recruits Figures ###
## Date 11/04/2024
# Michelle D. Mohr

# Mortality
# of SD for plot level
# of SD primary vs rim fire
# of SD across fire severity

# Count dead trees at the plot level
library(dplyr)
library(ggplot2)

# Summarize dead trees by plot
plot_dead_count <- pila_report_df %>%
  filter(alive_status == "Dead") %>%
  group_by(plotNum) %>%
  summarise(dead_count = n())


# Summarize dead trees by DBH class
dbh_class_dead_count <- pila_report_df %>%
  filter(alive_status == "Dead") %>%
  group_by(dbh_class_cm) %>%
  summarise(dead_count = n())

# Filter out NA values in DBH_class_cm
dbh_class_dead_count <- dbh_class_dead_count %>%
  filter(!is.na(dbh_class_cm))

# Plotting with viridis color palette, opacity, and a legend
ggplot(dbh_class_dead_count, aes(x = dbh_class_cm, y = dead_count, fill = as.factor(dbh_class_cm))) +
  geom_bar(stat = "identity", alpha = 0.7) +  # Adjust alpha for opacity
  scale_fill_viridis_d(option = "D", direction = 1, name = "DBH Class (cm)") +  # Use discrete viridis palette with legend
  labs(title = "Count of Dead Trees Across DBH Classes", x = "DBH Class (cm)", y = "Dead Tree Count") +
  theme_minimal()

# Plotting with viridis color palette, opacity, and a customized x-axis
mortality_dbh <- ggplot(dbh_class_dead_count, aes(x = dbh_class_cm, y = dead_count, fill = as.factor(dbh_class_cm))) +
  geom_bar(stat = "identity", alpha = 0.7) +  # Adjust alpha for opacity
  scale_fill_viridis_d(option = "D", direction = 1, name = "Diameter Class (cm)") +  # Use discrete viridis palette with legend
  labs(
    # title = "Count of Dead Trees Across DBH Classes",
    x = "Diameter Class (cm)", y = "Dead stems (#)") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank()  # Remove x-axis ticks
  )
mortality_dbh

# Filter out NA values in fire_sev
fire_sev_dead_count <- pila_report_df %>%
  filter(alive_status == "Dead", !is.na(fire_sev)) %>%
  group_by(fire_sev) %>%
  summarise(dead_count = n())



# Plotting with grey bars, opacity, and a customized x-axis
mortality_firesev <- ggplot(fire_sev_dead_count, aes(x = fire_sev, y = dead_count)) +
  geom_bar(stat = "identity", fill = "grey") +  # Set fill to grey and adjust alpha for opacity
  labs(
    # title = "Count of Dead Trees Across Fire Severity Levels",
    x = "Fire Severity", y = "Mortality Count") +
  theme_minimal() +
  theme(
    # axis.text.x = element_blank(),  # Remove x-axis text
    # axis.ticks.x = element_blank()  # Remove x-axis ticks
  )
mortality_firesev

# Filter out NA values in plot_rim and count dead trees
plot_rim_dead_count <- pila_report_df %>%
  filter(alive_status == "Dead", !is.na(plot_rim)) %>%
  group_by(plot_rim) %>%
  summarise(dead_count = n())

# Plotting with grey bars, opacity, and a customized x-axis
mortality_rimprimary <- ggplot(plot_rim_dead_count, aes(x = plot_rim, y = dead_count)) +
  geom_bar(stat = "identity", fill = "grey") +  # Set fill to grey and adjust alpha for opacity
  labs(
    # title = "Count of Dead Trees by Plot Rim Status",
    x = "",
    y = "Mortality Count") +
  theme_minimal() +
  theme(
    # axis.text.x = element_blank(),  # Remove x-axis text
    # axis.ticks.x = element_blank()  # Remove x-axis ticks
  )

mortality_rimprimary
# Summarize dead trees by plot rim status
rim_vs_primary_dead_count <- pila_report_df %>%
  filter(alive_status == "Dead", !is.na(plot_rim)) %>%
  group_by(plot_rim) %>%
  summarise(dead_count = n())

# Display the result
print(rim_vs_primary_dead_count)



# Create bins from 12 to 17 in intervals of 1, with a final bin for 17+
pila_report_df <- pila_report_df %>%
  filter(alive_status == "Dead", !is.na(vpdmax)) %>%
  mutate(vpdmax_bin = cut(
    vpdmax,
    breaks = c(12, 13, 14, 15, 16, 17, Inf),  # Define bin edges, with Inf for 17+
    labels = c("12 - 13", "13 - 14", "14 - 15", "15 - 16", "16 - 17", "17+"),  # Bin labels
    include.lowest = TRUE,
    right = FALSE  # Make intervals left-inclusive
  ))


# Summarize dead count across vpdmax bins
vpdmax_dead_count <- pila_report_df %>%
  group_by(vpdmax_bin) %>%
  summarise(dead_count = n())



mortality_rimprimary + mortality_dbh + mortality_firesev + mortality_vpd + plot_annotation(tag_levels = "A")


#### Mortality
# Make the figures the same but make it relative to # of live trees per plot
## Mortality (%) y-axis
# For each plot calculate the % dead (#dead/#live)
# And the average with SE bars for Primary and Rim Fire, Fire Sev, and VPDmax
# DBH class figure keep as is



# Calculate the number of dead and live trees per plot
plot_mortality_summary <- pila_report_df_clean %>%
  filter(!is.na(alive_status), !is.na(plotNum)) %>%  # Remove rows with NA in alive_status or plotNum
  group_by(plotNum, alive_status) %>%
  summarise(count = n(), .groups = "drop") %>%  # Summarise with .groups = "drop" to suppress grouping message
  pivot_wider(names_from = alive_status, values_from = count, values_fill = list(count = 0)) %>%  # Reshape data
  mutate(
    Alive = coalesce(`Alive`, 0),  # Replace NA with 0 if Alive column is missing
    Dead = coalesce(`Dead`, 0),    # Replace NA with 0 if Dead column is missing
    total_trees = Alive + Dead,  # Calculate total trees per plot
    mortality_ratio = if_else(Alive == 0, NA_real_, Dead / Alive)  # Avoid division by zero
  )

# Display the result
print(plot_mortality_summary)




# Calculate the number of dead and live trees per plot
plot_mortality_summary <- pila_report_df_clean %>%
  filter(!is.na(alive_status), !is.na(plotNum)) %>%  # Remove rows with NA in alive_status or plotNum
  group_by(plotNum, alive_status) %>%
  summarise(count = n(), .groups = "drop") %>%  # Summarise with .groups = "drop" to suppress grouping message
  pivot_wider(names_from = alive_status, values_from = count, values_fill = list(count = 0)) %>%  # Reshape data
  mutate(
    Alive = coalesce(`Alive`, 0),  # Replace NA with 0 if Alive column is missing
    Dead = coalesce(`Dead`, 0),    # Replace NA with 0 if Dead column is missing
    total_trees = Alive + Dead,  # Calculate total trees per plot
    mortality_percentage = if_else(Alive == 0, NA_real_, (Dead / Alive) * 100)  # Calculate percentage
  )




# Step 1: Calculate mortality percentage for each plot
plot_mortality_summary <- pila_report_df_clean %>%
  filter(!is.na(alive_status), !is.na(plotNum), !is.na(plot_rim)) %>%  # Remove rows with NA in relevant columns
  group_by(plotNum, plot_rim, alive_status) %>%
  summarise(count = n(), .groups = "drop") %>%  # Summarise with .groups = "drop" to suppress grouping message
  pivot_wider(names_from = alive_status, values_from = count, values_fill = list(count = 0)) %>%  # Reshape data
  mutate(
    Alive = coalesce(`Alive`, 0),  # Replace NA with 0 if Alive column is missing
    Dead = coalesce(`Dead`, 0),    # Replace NA with 0 if Dead column is missing
    mortality_percentage = if_else(Alive == 0, NA_real_, (Dead / Alive ) * 100)  # Calculate mortality percentage
  )

# Step 2: Calculate mean and standard error of mortality percentage by plot_rim
plot_rim_summary <- plot_mortality_summary %>%
  group_by(plot_rim) %>%
  summarise(
    mean_mortality_percentage = mean(mortality_percentage, na.rm = TRUE),  # Calculate mean
    se_mortality_percentage = sd(mortality_percentage, na.rm = TRUE) / sqrt(n()),  # Calculate standard error
    .groups = "drop"
  )


#### FIGURE 5A #####
# Step 3: Plot with SE bars
rimandprimary_mortalityavg <- ggplot(plot_rim_summary, aes(x = plot_rim, y = mean_mortality_percentage)) +
  geom_bar(stat = "identity", fill = "grey", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_mortality_percentage - se_mortality_percentage,
                    ymax = mean_mortality_percentage + se_mortality_percentage),
                width = 0.2, color = "black") +  # Add SE bars
  labs(
    # title = "Average Mortality Percentage by Plot Type",
       x = "",
       y = "Mean Mortality (%)") +
  theme_minimal()

rimandprimary_mortalityavg


# Step 1: Calculate mortality percentage for each plot
plot_mortality_summary <- pila_report_df_clean %>%
  filter(!is.na(alive_status), !is.na(plotNum), !is.na(fire_sev)) %>%  # Remove rows with NA in relevant columns
  group_by(plotNum, fire_sev, alive_status) %>%
  summarise(count = n(), .groups = "drop") %>%  # Summarise with .groups = "drop" to suppress grouping message
  pivot_wider(names_from = alive_status, values_from = count, values_fill = list(count = 0)) %>%  # Reshape data
  mutate(
    Alive = coalesce(`Alive`, 0),  # Replace NA with 0 if Alive column is missing
    Dead = coalesce(`Dead`, 0),    # Replace NA with 0 if Dead column is missing
    mortality_percentage = if_else(Alive == 0, NA_real_, (Dead / Alive) * 100)  # Calculate mortality percentage
  )

# Step 2: Calculate mean and standard error of mortality percentage by fire severity
fire_sev_summary <- plot_mortality_summary %>%
  group_by(fire_sev) %>%
  summarise(
    mean_mortality_percentage = mean(mortality_percentage, na.rm = TRUE),  # Calculate mean
    se_mortality_percentage = sd(mortality_percentage, na.rm = TRUE) / sqrt(n()),  # Calculate standard error
    .groups = "drop"
  )


#### FIGURE 5C ####
# Step 3: Plot with SE bars
firesev_mortalityaverage <- ggplot(fire_sev_summary, aes(x = fire_sev, y = mean_mortality_percentage)) +
  geom_bar(stat = "identity", fill = "grey", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_mortality_percentage - se_mortality_percentage,
                    ymax = mean_mortality_percentage + se_mortality_percentage),
                width = 0.2, color = "black") +  # Add SE bars
  labs(
    # title = "Average Mortality Percentage by Fire Severity",
       x = "Fire Severity",
       y = "Mean Mortality (%)") +
  theme_minimal()

firesev_mortalityaverage



# Step 1: Create 8 equal-width bins for `vpdmax` and calculate mortality percentage for each plot
plot_mortality_summary <- pila_report_df_clean %>%
  filter(!is.na(alive_status), !is.na(plotNum), !is.na(vpdmax)) %>%  # Remove rows with NA in relevant columns
  mutate(vpdmax_bin = cut(vpdmax, breaks = 8, include.lowest = TRUE)) %>%  # Create 8 bins
  group_by(plotNum, vpdmax_bin, alive_status) %>%
  summarise(count = n(), .groups = "drop") %>%  # Summarise with .groups = "drop" to suppress grouping message
  pivot_wider(names_from = alive_status, values_from = count, values_fill = list(count = 0)) %>%  # Reshape data
  mutate(
    Alive = coalesce(`Alive`, 0),  # Replace NA with 0 if Alive column is missing
    Dead = coalesce(`Dead`, 0),    # Replace NA with 0 if Dead column is missing
    mortality_percentage = if_else(Alive == 0, NA_real_, (Dead / Alive) * 100)  # Calculate mortality percentage
  )

# Step 2: Calculate mean and standard error of mortality percentage by `vpdmax` bin
vpdmax_summary <- plot_mortality_summary %>%
  group_by(vpdmax_bin) %>%
  summarise(
    mean_mortality_percentage = mean(mortality_percentage, na.rm = TRUE),  # Calculate mean
    se_mortality_percentage = sd(mortality_percentage, na.rm = TRUE) / sqrt(n()),  # Calculate standard error
    .groups = "drop"
  )




# Step 1: Create custom bins for `vpdmax` and calculate mortality percentage for each plot
plot_mortality_summary <- pila_report_df_clean %>%
  filter(!is.na(alive_status), !is.na(plotNum), !is.na(vpdmax)) %>%  # Remove rows with NA in relevant columns
  mutate(vpdmax_bin = cut(vpdmax, breaks = seq(12, ceiling(max(vpdmax)), by = 1), include.lowest = TRUE)) %>%  # Create bins with interval 1
  mutate(vpdmax_bin = factor(vpdmax_bin, labels = paste0(seq(12, 12 + length(levels(vpdmax_bin)) - 1), " -"))) %>%  # Custom labels
  group_by(plotNum, vpdmax_bin, alive_status) %>%
  summarise(count = n(), .groups = "drop") %>%  # Summarise with .groups = "drop" to suppress grouping message
  pivot_wider(names_from = alive_status, values_from = count, values_fill = list(count = 0)) %>%  # Reshape data
  mutate(
    Alive = coalesce(`Alive`, 0),  # Replace NA with 0 if Alive column is missing
    Dead = coalesce(`Dead`, 0),    # Replace NA with 0 if Dead column is missing
    mortality_percentage = if_else(Alive == 0, NA_real_, (Dead / Alive) * 100)  # Calculate mortality percentage
  )

# Step 2: Calculate mean and standard error of mortality percentage by `vpdmax` bin
vpdmax_summary <- plot_mortality_summary %>%
  group_by(vpdmax_bin) %>%
  summarise(
    mean_mortality_percentage = mean(mortality_percentage, na.rm = TRUE),  # Calculate mean
    se_mortality_percentage = sd(mortality_percentage, na.rm = TRUE) / sqrt(n()),  # Calculate standard error
    .groups = "drop"
  )



# Step 1: Create custom bins for `vpdmax` and calculate mortality percentage for each plot
plot_mortality_summary <- pila_report_df_clean %>%
  filter(!is.na(alive_status), !is.na(plotNum), !is.na(vpdmax)) %>%  # Remove rows with NA in relevant columns
  mutate(vpdmax_bin = cut(
    vpdmax,
    breaks = c(12, 13, 14, 15, 16, 17, Inf),  # Define bins with the last bin for 17+
    labels = c("12 - 13", "13 - 14", "14 - 15", "15 - 16", "16 - 17", "17+"),  # Custom labels for bins
    include.lowest = TRUE,
    right = FALSE  # Left-inclusive intervals
  )) %>%
  group_by(plotNum, vpdmax_bin, alive_status) %>%
  summarise(count = n(), .groups = "drop") %>%  # Summarise with .groups = "drop" to suppress grouping message
  pivot_wider(names_from = alive_status, values_from = count, values_fill = list(count = 0)) %>%  # Reshape data
  mutate(
    Alive = coalesce(`Alive`, 0),  # Replace NA with 0 if Alive column is missing
    Dead = coalesce(`Dead`, 0),    # Replace NA with 0 if Dead column is missing
    mortality_percentage = if_else(Alive == 0, NA_real_, (Dead / Alive) * 100)  # Calculate mortality percentage
  )


# Step 2: Calculate mean and standard error of mortality percentage by `vpdmax` bin
vpdmax_summary <- plot_mortality_summary %>%
  group_by(vpdmax_bin) %>%
  summarise(
    mean_mortality_percentage = mean(mortality_percentage, na.rm = TRUE),  # Calculate mean
    se_mortality_percentage = sd(mortality_percentage, na.rm = TRUE) / sqrt(n()),  # Calculate standard error
    .groups = "drop"
  )


#### FIGURE 5D ####
# Step 3: Plot with SE bars and viridis color palette
vpdmax_mortalityavg <- ggplot(vpdmax_summary, aes(x = vpdmax_bin, y = mean_mortality_percentage, fill = vpdmax_bin)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_mortality_percentage - se_mortality_percentage,
                    ymax = mean_mortality_percentage + se_mortality_percentage),
                width = 0.2, color = "black") +  # Add SE bars
  scale_fill_viridis_d(option = "D", direction = 1, name = "VPDmax Range") +  # Apply viridis palette and add legend
  labs(
    # title = "Average Mortality Percentage by VPDmax",
       x = "VPDmax Range",
       y = "Mean Mortality (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text labels
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.title.x = element_text()    # Keep x-axis title
  )

vpdmax_mortalityavg



#### All figures for mortality
mortality_dbh
rimandprimary_mortalityavg
firesev_mortalityaverage
vpdmax_mortalityavg


##### FIGURE 5 ####
rimandprimary_mortalityavg + mortality_dbh + firesev_mortalityaverage + vpdmax_mortalityavg + plot_annotation(tag_levels = "A")

# Recruitment ( 0 - 3 DBH class)
# of recruits for plot level
# of recruits primary vs rim fire
# of recruits across fire severity



# Filter data for recruitment (DBH between 0 and 3 cm) and calculate recruitment count per plot
recruitment_by_plot <- pila_report_df_clean %>%
  filter(DBH_cm >= 0 & DBH_cm <= 3, !is.na(plotNum)) %>%  # Filter for recruitment and remove rows with NA in plotNum
  group_by(plotNum) %>%
  summarise(recruitment_count = n(), .groups = "drop")  # Count recruitment per plot

# Plot recruitment count for each plot
plotlevel_recruit <- ggplot(recruitment_by_plot, aes(x = factor(plotNum), y = recruitment_count)) +
  geom_bar(stat = "identity", fill = "grey", alpha = 0.7) +
  labs(
    # title = "Recruitment (DBH 0-3 cm) Across Plots",
       x = "Plot Number",
       y = "Recruitment Count") +
  theme_minimal()
plotlevel_recruit

# Assuming recruitment_by_plot has been calculated already




# Filter data for recruitment (DBH 0-3 cm) and calculate recruitment count per plot
recruitment_by_plot <- pila_report_df_clean %>%
  filter(DBH_cm >= 0 & DBH_cm <= 3, !is.na(plotNum), !is.na(plot_rim)) %>%  # Filter for recruitment and remove rows with NA in plotNum or plot_rim
  group_by(plotNum, plot_rim) %>%
  summarise(recruitment_count = n(), .groups = "drop")  # Count recruitment per plot

# Calculate mean and standard error of recruitment by plot type (plot_rim)
recruitment_by_plot_type <- recruitment_by_plot %>%
  group_by(plot_rim) %>%
  summarise(
    mean_recruitment = mean(recruitment_count, na.rm = TRUE),  # Mean recruitment count
    se_recruitment = sd(recruitment_count, na.rm = TRUE) / sqrt(n()),  # Standard error
    .groups = "drop"
  )


# Step 1: Filter data for recruitment (DBH 0-3 cm) and calculate recruitment count per plot
recruitment_by_plot <- pila_report_df_clean %>%
  filter(DBH >= 0 & DBH <= 3, !is.na(plotNum), !is.na(plot_rim)) %>%  # Filter for recruitment and remove rows with NA in plotNum or plot_rim
  group_by(plotNum, plot_rim) %>%
  summarise(recruitment_count = n(), .groups = "drop")  # Count recruitment per plot

# Step 2: Calculate mean and standard error of recruitment by plot type (plot_rim)
recruitment_by_plot_type <- recruitment_by_plot %>%
  group_by(plot_rim) %>%
  summarise(
    mean_recruitment = mean(recruitment_count, na.rm = TRUE),  # Mean recruitment count
    se_recruitment = sd(recruitment_count, na.rm = TRUE) / sqrt(n()),  # Standard error
    .groups = "drop"
  )



### FIGURE 6A ####
# Step 3: Plot average recruitment by plot type with standard error bars
recruitavg_rimprim <- ggplot(recruitment_by_plot_type, aes(x = plot_rim, y = mean_recruitment)) +
  geom_bar(stat = "identity", fill = "grey", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_recruitment - se_recruitment,
                    ymax = mean_recruitment + se_recruitment),
                width = 0.2, color = "black") +  # Add SE bars
  labs(
    # title = "Average Recruitment by Plot Type (DBH 0-3 cm)",
       x = "",
       y = "Mean Recruitment (# of stems)") +
  theme_minimal()

recruitavg_rimprim


### Recruitment Fire Sev

# Filter data for recruitment (DBH 0-3 cm) and calculate recruitment count per plot and fire severity
recruitment_by_fire_sev <- pila_report_df_clean %>%
  filter(DBH_cm >= 0 & DBH_cm <= 3, !is.na(plotNum), !is.na(fire_sev)) %>%  # Filter for recruitment and remove rows with NA in plotNum or fire_sev
  group_by(plotNum, fire_sev) %>%
  summarise(recruitment_count = n(), .groups = "drop")  # Count recruitment per plot

# Calculate mean and standard error of recruitment by fire severity (fire_sev)
recruitment_by_fire_sev_summary <- recruitment_by_fire_sev %>%
  group_by(fire_sev) %>%
  summarise(
    mean_recruitment = mean(recruitment_count, na.rm = TRUE),  # Mean recruitment count
    se_recruitment = sd(recruitment_count, na.rm = TRUE) / sqrt(n()),  # Standard error
    .groups = "drop"
  )

#### FIGURE 6C ####
# Plot average recruitment by fire severity with standard error bars
recruitment_firesev <- ggplot(recruitment_by_fire_sev_summary, aes(x = fire_sev, y = mean_recruitment)) +
  geom_bar(stat = "identity", fill = "grey", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_recruitment - se_recruitment,
                    ymax = mean_recruitment + se_recruitment),
                width = 0.2, color = "black") +  # Add SE bars
  labs(
    # title = "Average Recruitment by Fire Severity (DBH 0-3 cm)",
       x = "Fire Severity",
       y = "Mean Recruitment (# of stems)") +
  theme_minimal()



recruitment_firesev



# Filter data for recruitment (DBH 0-3 cm) and bin `vpdmax`
recruitment_by_vpdmax <- pila_report_df_clean %>%
  filter(DBH_cm >= 0 & DBH_cm <= 3, !is.na(plotNum), !is.na(vpdmax)) %>%  # Filter for recruitment and remove rows with NA in plotNum or vpdmax
  mutate(vpdmax_bin = cut(vpdmax, breaks = seq(12, ceiling(max(vpdmax)), by = 1), include.lowest = TRUE)) %>%  # Create bins for vpdmax
  group_by(plotNum, vpdmax_bin) %>%
  summarise(recruitment_count = n(), .groups = "drop")  # Count recruitment per plot and vpdmax bin

# Calculate mean and standard error of recruitment by `vpdmax` bin
recruitment_by_vpdmax_summary <- recruitment_by_vpdmax %>%
  group_by(vpdmax_bin) %>%
  summarise(
    mean_recruitment = mean(recruitment_count, na.rm = TRUE),  # Mean recruitment count
    se_recruitment = sd(recruitment_count, na.rm = TRUE) / sqrt(n()),  # Standard error
    .groups = "drop"
  )



# Step 1: Create custom bins for `vpdmax` and calculate recruitment count for each plot
plot_recruitment_summary <- pila_report_df_clean %>%
  filter(DBH_cm >= 0 & DBH_cm <= 3, !is.na(plotNum), !is.na(vpdmax)) %>%  # Filter for recruitment and remove rows with NA in plotNum or vpdmax
  mutate(vpdmax_bin = cut(vpdmax, breaks = seq(12, ceiling(max(vpdmax)), by = 1), include.lowest = TRUE)) %>%  # Create bins with interval 1
  mutate(vpdmax_bin = factor(vpdmax_bin, labels = paste0(seq(12, 12 + length(levels(vpdmax_bin)) - 1), " -"))) %>%  # Custom labels
  group_by(plotNum, vpdmax_bin) %>%
  summarise(recruitment_count = n(), .groups = "drop")  # Count recruitment per plot and vpdmax bin

# Step 2: Calculate mean and standard error of recruitment count by `vpdmax` bin
vpdmax_recruitment_summary <- plot_recruitment_summary %>%
  group_by(vpdmax_bin) %>%
  summarise(
    mean_recruitment_count = mean(recruitment_count, na.rm = TRUE),  # Calculate mean
    se_recruitment_count = sd(recruitment_count, na.rm = TRUE) / sqrt(n()),  # Calculate standard error
    .groups = "drop"
  )



# Figures for recruitment

recruitavg_rimprim
recruitment_firesev



##### Recruitment across infection rate


# Filter for recruitment (DBH 0-3 cm), categorize infection rates, and set order of infection categories
recruitment_by_infection <- pila_report_df_clean %>%
  filter(DBH_cm >= 0 & DBH_cm <= 3, !is.na(incidencepercent)) %>%  # Filter for recruitment and remove rows with NA in incidencepercent
  mutate(infection_category = case_when(
    incidencepercent == 0 ~ "0 %",
    incidencepercent > 0 & incidencepercent <= 3 ~ "0.01-3%",  # Adjusted for the range 0.01-3%
    incidencepercent > 3 & incidencepercent <= 6 ~ "3.01-6%",  # Adjusted for the range 3.01-6%
    incidencepercent > 6 & incidencepercent <= 10 ~ "6.01-10%",  # Adjusted for the range 6.01-10%
    incidencepercent > 10 ~ "> 10%"  # Greater than 10%
  ),
  infection_category = factor(infection_category, levels = c("0 %", "0.01-3%", "3.01-6%", "6.01-10%", "> 10%"))) %>%  # Set ordered levels
  group_by(infection_category) %>%
  summarise(recruitment_count = n(), .groups = "drop")  # Count recruitment per infection category


#### FIGURE 6B ####

# Plot recruitment counts by infection rate category with ordered x-axis and hidden x-axis labels/ticks
recruitment_infectionrate <- ggplot(recruitment_by_infection, aes(x = infection_category, y = recruitment_count, fill = infection_category)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_viridis_d(option = "D", direction = 1, name = "Infection Rate") +  # Apply viridis palette with discrete scale
  labs(
    # title = "Recruitment Across Infection Rate Categories (DBH 0-3 cm)",
    x = "Infection Rate Category",
    y = "Recruitment Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Hide x-axis text labels
    axis.ticks.x = element_blank(),  # Hide x-axis ticks
    axis.title.x = element_text()  # Keep x-axis title
  )

recruitment_infectionrate




# Filter for dead individuals and categorize infection rates
mortality_by_infection <- pila_report_df_clean %>%
  filter(alive_status == "Dead", !is.na(incidencepercent)) %>%  # Filter for dead individuals and remove rows with NA in incidencepercent
  mutate(infection_category = case_when(
    incidencepercent == 0 ~ "0 %",
    incidencepercent > 0 & incidencepercent <= 3 ~ "0.01-3%",  # Adjusted for the range 0.01-3%
    incidencepercent > 3 & incidencepercent <= 6 ~ "3.01-6%",  # Adjusted for the range 3.01-6%
    incidencepercent > 6 & incidencepercent <= 10 ~ "6.01-10%",  # Adjusted for the range 6.01-10%
    incidencepercent > 10 ~ "> 10%"  # Greater than 10%
  ),
  infection_category = factor(infection_category, levels = c("0 %", "0.01-3%", "3.01-6%", "6.01-10%", "> 10%"))) %>%  # Set ordered levels
  group_by(infection_category) %>%
  summarise(mortality_count = n(), .groups = "drop")  # Count mortality per infection category



#### AVERAGE
# Recruitment



# Calculate recruitment count per infection category and then get mean and SE
recruitment_by_infection <- pila_report_df_clean %>%
  filter(DBH_cm >= 0 & DBH_cm <= 3, !is.na(incidencepercent)) %>%  # Filter for recruitment and remove rows with NA in incidencepercent
  mutate(infection_category = case_when(
    incidencepercent == 0 ~ "0 %",
    incidencepercent > 0 & incidencepercent <= 3 ~ "0.01-3%",  # Adjusted for the range 0.01-3%
    incidencepercent > 3 & incidencepercent <= 6 ~ "3.01-6%",  # Adjusted for the range 3.01-6%
    incidencepercent > 6 & incidencepercent <= 10 ~ "6.01-10%",  # Adjusted for the range 6.01-10%
    incidencepercent > 10 ~ "> 10%"  # Greater than 10%
  ),
  infection_category = factor(infection_category, levels = c("0 %", "0.01-3%", "3.01-6%", "6.01-10%", "> 10%"))) %>%  # Set ordered levels
  group_by(plotNum, infection_category) %>%
  summarise(recruitment_count = n(), .groups = "drop")  # Count recruitment per plot and infection category

# Calculate mean and standard error of recruitment by infection category
recruitment_summary <- recruitment_by_infection %>%
  group_by(infection_category) %>%
  summarise(
    mean_recruitment = mean(recruitment_count, na.rm = TRUE),  # Calculate mean recruitment count
    se_recruitment = sd(recruitment_count, na.rm = TRUE) / sqrt(n()),  # Calculate standard error
    .groups = "drop"
  )




# Calculate mortality count per infection category and then get mean and SE
mortality_by_infection <- pila_report_df_clean %>%
  filter(alive_status == "Dead", !is.na(incidencepercent)) %>%  # Filter for dead individuals and remove rows with NA in incidencepercent
  mutate(infection_category = case_when(
    incidencepercent == 0 ~ "0 %",
    incidencepercent > 0 & incidencepercent <= 3 ~ "0.01-3%",  # Adjusted for the range 0.01-3%
    incidencepercent > 3 & incidencepercent <= 6 ~ "3.01-6%",  # Adjusted for the range 3.01-6%
    incidencepercent > 6 & incidencepercent <= 10 ~ "6.01-10%",  # Adjusted for the range 6.01-10%
    incidencepercent > 10 ~ "> 10%"  # Greater than 10%
  ),
  infection_category = factor(infection_category, levels = c("0 %", "0.01-3%", "3.01-6%", "6.01-10%", "> 10%"))) %>%  # Set ordered levels
  group_by(plotNum, infection_category) %>%
  summarise(mortality_count = n(), .groups = "drop")  # Count mortality per plot and infection category

# Calculate mean and standard error of mortality by infection category
mortality_summary <- mortality_by_infection %>%
  group_by(infection_category) %>%
  summarise(
    mean_mortality = mean(mortality_count, na.rm = TRUE),  # Calculate mean mortality count
    se_mortality = sd(mortality_count, na.rm = TRUE) / sqrt(n()),  # Calculate standard error
    .groups = "drop"
  )



###########$$$$$$$$$$$$$$$$##########
# Adjust recruitment plot
recruitmentavg_infectionrate <- ggplot(recruitment_summary, aes(x = infection_category, y = mean_recruitment, fill = infection_category)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_recruitment - se_recruitment, ymax = mean_recruitment + se_recruitment),
                width = 0.2, color = "black") +
  scale_fill_viridis_d(option = "D", direction = 1, name = "Infection Rate (%)") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +  # Set integer breaks
  labs(
    x = "Plot-level infection rate",
    y = "Mean Recruitment (# of stems)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_text()
  )
recruitmentavg_infectionrate
# Adjust mortality plot
mortalityavg_infectionrate <- ggplot(mortality_summary, aes(x = infection_category, y = mean_mortality, fill = infection_category)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_mortality - se_mortality, ymax = mean_mortality + se_mortality),
                width = 0.2, color = "black") +
  scale_fill_viridis_d(option = "D", direction = 1, name = "Infection Rate (%)") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +  # Set integer breaks
  labs(
    x = "Plot-level infection rate",
    y = "Mean Mortality (# of stems)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_text()
  )
mortalityavg_infectionrate




# Define VPDmax ranges and count unique plots within each range
vpdmax_plot_counts <- pila_report_df_clean %>%
  filter(!is.na(vpdmax), !is.na(plotNum)) %>%  # Remove rows with NA in vpdmax or plotNum
  mutate(vpdmax_range = cut(vpdmax, breaks = seq(12, ceiling(max(vpdmax)), by = 1), include.lowest = TRUE)) %>%  # Define bins for vpdmax
  group_by(vpdmax_range) %>%
  summarise(plot_count = n_distinct(plotNum), .groups = "drop")  # Count unique plots in each vpdmax bin


# Plot number of plots across VPDmax ranges
ggplot(vpdmax_plot_counts, aes(x = vpdmax_range, y = plot_count, fill = vpdmax_range)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_viridis_d(option = "D", direction = 1, name = "VPDmax Range") +  # Apply viridis palette with discrete scale
  labs(
    title = "Number of Plots Across VPDmax Ranges",
    x = "VPDmax Range",
    y = "Plot Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.ticks.x = element_blank()
  )

#### Join plots

recruitavg_rimprim + recruitment_infectionrate + recruitmentavg_firesev + recruitment_vpd + plot_annotation(tag_levels = "A")





