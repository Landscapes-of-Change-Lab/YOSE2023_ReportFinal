#### FIGURES ####

### Report Figure Code ###

# Load required libraries
library(ggplot2)
library(dplyr)
library(viridis)


# Sort the dataframe by plotID
pila_report_df <- pila_report_df %>%
  arrange(plotID) %>%
  mutate(plot_type = ifelse(plot_type == "high severity", "highseverity", plot_type))

plots_to_exclude <- c(7, 8, 25)

# Create a new dataframe excluding the specified plots
pila_report_df <- pila_report_df %>%
  filter(!plotNum %in% plots_to_exclude)


# Assign a unique plotNum from 1 to 56 for each unique plotID
pila_report_df <- pila_report_df %>%
  arrange(plotID) %>%  # Arrange by plotID if needed for ordering
  mutate(plotNum = as.integer(factor(plotID, levels = unique(plotID))))  # Assign unique values 1 to 56 based on unique plotID


# Aggregate data by plotID
aggregated_df <- pila_report_df %>%
  group_by(plotNum) %>%
  summarise(
    plotNum = first(plotNum),
    plot_type = first(plot_type),
    slope = first(slope),
    plot_elevation_ft = first(plot_elevation_ft),
    ribes = first(ribes),
    wpbr_presentplot = first(wpbr_presentplot),
    beetles_present = first(beetles_present),
    vpdmax = first(vpdmax),
    tmean = first(tmean),
    ppt = first(ppt),
    incidencepercent = first(incidencepercent),
    infectionrate = first(infectionrate),
    fire_present = first(fire_present),
    fire_sev = first(fire_sev),
    high_sev = first(high_sev),
    # pila_area = first(pila_area),
    pilaalive_plot = first(pilaalive_plot),
    piladead_plot = first(piladead_plot),
    total_pila = first(total_pila),
    # pilaalive_density = first(pilaalive_density),
    # pilatotal_density = first(pilatotal_density)
  )



#install.packages("gt")
library(gt)

#### aggregated_df

# Create a new dataframe with the specified columns
aggregated_table <- aggregated_df %>%
  select(
    plotNum,
    slope,
    plot_elevation_ft,
    ribes,
    wpbr_presentplot,
    beetles_present,
    fire_sev,
    vpdmax,
    tmean,
    ppt,
    pilaalive_plot,
    piladead_plot,
    incidencepercent
  )


# Create a table using gt
aggregated_table %>%
  gt() %>%
  tab_header(
    title = md("**Plot Characteristics Table**"),
    subtitle = md("Ribes, WPBR, Beetles, and Tree Status by Plot")
  ) %>%
  cols_label(
    plotNum = "Plot Number",
    ribes = "Ribes Present",
    wpbr_presentplot = "WPBR Present",
    beetles_present = "Beetles Present",
    pilaalive_plot = "Alive Trees",
    piladead_plot = "Dead Trees",
    incidencepercent = "Incidence %"
  ) %>%
  fmt_number(
    columns = vars(pilaalive_plot, piladead_plot),  # Updated column references to match labels
    decimals = 0
  ) %>%
  tab_options(
    table.width = pct(80),
    table.font.size = 12
  )



table_df <- aggregated_df %>%
  distinct(plotNum, fire_sev, pilaalive_plot, piladead_plot, total_pila, beetles_present, ribes, wpbr_presentplot, incidencepercent)

# Create a table using gt
table_df %>%
  gt() %>%
  tab_header(
    title = "Plot Characteristics Table",
    subtitle = "Fire Severity, Ribes, WPBR, Beetles, and Tree Status by Plot"
  ) %>%
  cols_label(
    plotNum = "Plot Number",
    ribes = "Ribes Present",
    wpbr_presentplot = "WPBR Present",
    beetles_present = "Beetles Present",
    pilaalive_plot = "Alive Trees",
    piladead_plot = "Dead Trees",
    total_pila = "Total Trees",
    fire_sev = "Fire Severity",
    incidencepercent = "Incidence %"
  ) %>%
  fmt_number(columns = vars(pilaalive_plot, piladead_plot), decimals = 0) %>%
  tab_options(
    table.width = pct(80),
    table.font.size = 12
  )


########## Table
# Recode columns and create the table
table_df %>%
  mutate(
    ribes = ifelse(ribes == 1, "Present", "Absent"),
    wpbr_presentplot = ifelse(wpbr_presentplot == 1, "Present", "Absent"),
    beetles_present = ifelse(beetles_present == 1, "Present", "Absent")
  ) %>%
  gt() %>%
  tab_header(
    title = "Plot Characteristics Table",
    subtitle = "Fire Severity, Ribes, WPBR, Beetles, and Tree Status by Plot"
  ) %>%
  cols_label(
    plotNum = "Plot Number",
    ribes = "Ribes Present",
    wpbr_presentplot = "WPBR Present",
    beetles_present = "Beetles Present",
    pilaalive_plot = "Alive Trees",
    piladead_plot = "Dead Trees",
    total_pila = "Total Trees",
    fire_sev = "Fire Severity",
    incidencepercent = "Incidence %"
  ) %>%
  fmt_number(columns = vars(pilaalive_plot, piladead_plot), decimals = 0) %>%
  tab_options(
    table.width = pct(80),
    table.font.size = 12
  )


############


# Summarize data
summary_df <- table_df %>%
  mutate(
    ribes = ifelse(ribes == 1, "Present", "Absent"),
    wpbr_presentplot = ifelse(wpbr_presentplot == 1, "Present", "Absent"),
    beetles_present = ifelse(beetles_present == 1, "Present", "Absent")
  ) %>%
  summarise(
    # Count of Plots
    total_plots = n(),
    # Ribes Presence Counts
    ribes_present = sum(ribes == "Present"),
    ribes_absent = sum(ribes == "Absent"),
    # WPBR Presence Counts
    wpbr_present = sum(wpbr_presentplot == "Present"),
    wpbr_absent = sum(wpbr_presentplot == "Absent"),
    # Beetles Presence Counts
    beetles_present = sum(beetles_present == "Present"),
    beetles_absent = sum(beetles_present == "Absent"),
    # Tree Counts
    alive_trees_total = sum(pilaalive_plot, na.rm = TRUE),
    dead_trees_total = sum(piladead_plot, na.rm = TRUE),
    total_trees = sum(total_pila, na.rm = TRUE),
    # Fire Severity Distribution
    fire_severity_summary = paste(names(table(table_df$fire_sev)), table(table_df$fire_sev), collapse = ", ")
  )

# Create summary table with gt
summary_df %>%
  gt() %>%
  tab_header(
    title = "Summary of Plot Characteristics",
    subtitle = "Counts and Totals for Ribes, WPBR, Beetles, and Tree Status"
  ) %>%
  cols_label(
    total_plots = "Total Plots",
    ribes_present = "Ribes Present",
    ribes_absent = "Ribes Absent",
    wpbr_present = "WPBR Present",
    wpbr_absent = "WPBR Absent",
    beetles_present = "Beetles Present",
    beetles_absent = "Beetles Absent",
    alive_trees_total = "Total Alive Trees",
    dead_trees_total = "Total Dead Trees",
    total_trees = "Total Trees",
    fire_severity_summary = "Fire Severity Distribution"
  ) %>%
  tab_options(
    table.width = pct(80),
    table.font.size = 12
  )


#########

# Convert columns and check unique values for debugging
table_df <- table_df %>%
  mutate(
    ribes = ifelse(ribes == 1, "Present", "Absent"),
    wpbr_presentplot = ifelse(wpbr_presentplot == 1, "Present", "Absent"),
    beetles_present = ifelse(beetles_present == 1, "Present", "Absent")
  )

# Debug: Print unique values to confirm conversion
print("Unique values in beetles_present:")
print(unique(table_df$beetles_present))  # Should print "Present" and "Absent"

# Summarize data with corrected counts
summary_df <- table_df %>%
  summarise(
    # Count of Plots
    total_plots = n(),
    # Ribes Presence Counts
    ribes_present = sum(ribes == "Present"),
    ribes_absent = sum(ribes == "Absent"),
    # WPBR Presence Counts
    wpbr_present = sum(wpbr_presentplot == "Present"),
    wpbr_absent = sum(wpbr_presentplot == "Absent"),
    # Beetles Presence Counts
    beetles_present = sum(beetles_present == "Present"),
    beetles_absent = sum(beetles_present == "Absent"),
    # Tree Counts
    alive_trees_total = sum(pilaalive_plot, na.rm = TRUE),
    dead_trees_total = sum(piladead_plot, na.rm = TRUE),
    total_trees = sum(total_pila, na.rm = TRUE),
    # Fire Severity Distribution
    fire_severity_summary = paste(names(table(table_df$fire_sev)), table(table_df$fire_sev), collapse = ", ")
  )

# Create summary table with gt
summary_df %>%
  gt() %>%
  tab_header(
    title = "Summary of Plot Characteristics",
    subtitle = "Counts and Totals for Ribes, WPBR, Beetles, and Tree Status"
  ) %>%
  cols_label(
    total_plots = "Total Plots",
    ribes_present = "Ribes Present",
    ribes_absent = "Ribes Absent",
    wpbr_present = "WPBR Present",
    wpbr_absent = "WPBR Absent",
    beetles_present = "Beetles Present",
    beetles_absent = "Beetles Absent",
    alive_trees_total = "Total Alive Trees",
    dead_trees_total = "Total Dead Trees",
    total_trees = "Total Trees",
    fire_severity_summary = "Fire Severity Distribution"
  ) %>%
  tab_options(
    table.width = pct(80),
    table.font.size = 12
  )


#########################################################
library(ggplot2)
library(tidyverse)
library(viridis)  # For color-blind friendly palettes

# Arrange the data by incidencepercent
table_df_ordered <- table_df %>%
  arrange(incidencepercent)


# Filter for infected plots only (incidencepercent > 0)
filtered_table_df <- table_df_ordered %>%
  filter(incidencepercent > 0)



#### FIGURE 2A ####
# Create the bar plot without color
hist_nocolor_infectrate_nonzero <- ggplot(filtered_table_df, aes(x = reorder(factor(plotNum), incidencepercent), y = incidencepercent)) +
  geom_bar(stat = "identity", color = "black", fill = "grey") +  # Set a neutral fill color
  labs(
    # title = "Infection Rate Across Plots",
    x = "Plot Number",
    y = "Infection Rate (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate x-axis labels for readability
  )

# Display the plot
hist_nocolor_infectrate_nonzero

# Create 11 bins for the 'severity' column
pila_report_df$severity_bins <- cut(
  pila_report_df$severity,
  breaks = 11,
  labels = paste(1:11)
)




# Exclude rows where severity is zero
pila_report_df_filtered <- subset(pila_report_df, severity != 0)

# Create the bar plot for the filtered dataframe
sevacrosstrees <- ggplot(pila_report_df_filtered, aes(x = severity_bins)) +
  geom_bar(fill = "grey", color = "black") +  # Bar plot with neutral color
  labs(
    x = "Severity Class",
    y = "Number of Trees"
    # title = "Number of Trees by Severity Bins (Excluding Zeros)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1)  # Rotate x-axis labels for readability
  )
sevacrosstrees

pila_alive_infected <- pila_report_df %>%
  filter(alive_status == "Alive", wpbr_present == 1)


#### FIGURE 2B ####
sev_histplot <- ggplot(pila_alive_infected, aes(x = severity)) +
  geom_histogram(fill = "#303077", color = "black", alpha = 0.5) +
  labs(
    x = "Infection Severity",
    y = "Number of Trees"
    # title = "Number of Trees by Severity Bins (Excluding Zeros)"
  ) +
  scale_x_continuous(breaks = seq(0, max(pila_alive_infected$severity, na.rm = TRUE), by = 2)) +  # Set x-axis breaks incrementing by 2
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1)  # Rotate x-axis labels for readability
  )

sev_histplot



# Convert DBH from cm to inches and create the new column DBH_in
pila_report_df <- pila_report_df %>%
  mutate(DBH_in = DBH_cm * 0.393701)

# Create dbh_class based on the specified DBH_in ranges in inches
pila_report_df <- pila_report_df %>%
  mutate(
    dbh_class = case_when(
      DBH_in >= 0 & DBH_in <= 1.5 ~ "0 - 1.5",
      DBH_in > 1.5 & DBH_in <= 4 ~ "1.51 - 4",
      DBH_in > 4 & DBH_in <= 20 ~ "4.01 - 20",
      DBH_in > 20 & DBH_in <= 40 ~ "20.01 - 40",
      DBH_in > 40 & DBH_in <= 50 ~ "40.01 - 50",
      DBH_in > 50 ~ "50.01 - 90"
    )
  )

# Ensure DBH_in is numeric
pila_report_df <- pila_report_df %>%
  mutate(DBH_in = as.numeric(DBH_in)) %>%  # Convert DBH_in to numeric if it's not
  mutate(
    dbh_class_cm = case_when(
      DBH_in >= 0 & DBH_in <= 1.5 ~ "0 - 3.81",
      DBH_in > 1.5 & DBH_in <= 4 ~ "3.82 - 10.16",
      DBH_in > 4 & DBH_in <= 20 ~ "10.17 - 50.8",
      DBH_in > 20 & DBH_in <= 40 ~ "50.81 - 101.6",
      DBH_in > 40 & DBH_in <= 50 ~ "101.61 - 127",
      DBH_in > 50 ~ "127.01 - 228.6",
      TRUE ~ NA_character_  # Assign NA if DBH_in does not fit any category
    )
  )

# Define the correct order of DBH classes
pila_report_df <- pila_report_df %>%
  mutate(
    dbh_class = factor(dbh_class, levels = c("0 - 1.5", "1.51 - 4",
                                             "4.01 - 20", "20.01 - 40",
                                             "40.01 - 50", "50.01 - 90"))
  )

# Define the correct order of DBH classes in centimeters
pila_report_df <- pila_report_df %>%
  mutate(
    dbh_class_cm = factor(dbh_class_cm, levels = c("0 - 3.81", "3.82 - 10.16",
                                                   "10.17 - 50.8", "50.81 - 101.6",
                                                   "101.61 - 127", "127.01 - 228.6"))
  )




# Filter out non-zero severity for the box plot
non_zero_df <- pila_report_df %>% filter(severity > 0)

# Count zeros for each dbh_class
zero_counts <- pila_report_df %>%
  filter(severity == 0) %>%
  count(dbh_class)





# Filter out NA values in dbh_class for the zero count plot
zero_counts <- pila_report_df %>%
  filter(severity == 0) %>%
  drop_na(dbh_class) %>%
  count(dbh_class)







# Create a new column to separate zero and non-zero severity values
pila_report_df <- pila_report_df %>%
  mutate(severity_group = ifelse(severity == 0, "Zero Severity", "Non-Zero Severity"))



# Filter out zero severity values
non_zero_df <- pila_report_df %>%
  filter(severity > 0)





# Separate data into non-zero and zero severity datasets
non_zero_df <- pila_report_df %>%
  filter(severity > 0)

zero_df <- pila_report_df %>%
  filter(severity == 0)






# Filter for only live trees
live_trees_df <- pila_report_df %>% filter(alive_status == "Alive")


# Create subsets based on high_sev for only live trees
high_severity_plots <- live_trees_df %>% filter(high_sev == 1)
non_high_severity_plots <- live_trees_df %>% filter(high_sev == 0)

# Count unique plot IDs in high_severity_plots
num_unique_high_severity_plots <- high_severity_plots %>%
  summarise(num_plots = n_distinct(plotID)) %>%
  pull(num_plots)

# Print the result
num_unique_high_severity_plots

### 11 high sev plots with live trees

# Count the number of plots with infections in high_severity_plots
infected_high_severity_plots <- high_severity_plots %>%
  filter(wpbr_presentplot == 1) %>%   # Filter for plots with infection present
  summarise(num_infected_plots = n_distinct(plotID)) %>%   # Count unique plot IDs
  pull(num_infected_plots)

# Print the result
infected_high_severity_plots

### 1/11 highsev plots have infections = 9.09%

# Count the total number of live trees in non-high-severity plots
total_trees_high_severity <- nrow(high_severity_plots)

### 1/133 high sev trees have infections = 0.75%

total_trees_high_severity
# Count unique plot IDs in non_high_severity_plots
num_unique_nonhigh_severity_plots <- non_high_severity_plots %>%
  summarise(num_plots = n_distinct(plotID)) %>%
  pull(num_plots)

# Print the result
num_unique_nonhigh_severity_plots

## 42 nonhighsev plots with live trees

# Count the number of plots with infections in non-high-severity plots
infected_non_high_severity_plots <- non_high_severity_plots %>%
  filter(wpbr_presentplot == 1) %>%   # Filter for plots with infection present
  summarise(num_infected_plots = n_distinct(plotID)) %>%   # Count unique plot IDs
  pull(num_infected_plots)

# Print the result
infected_non_high_severity_plots

### 26/42 non high sev plots have infections = 61.9%

# Count the number of live trees with infections in non-high-severity plots
infected_trees_non_high_severity <- non_high_severity_plots %>%
  filter(wpbr_present == 1) %>%   # Filter for trees with infection present
  nrow()   # Count the number of rows, each representing an infected tree

# Print the result
infected_trees_non_high_severity

## 53/1340 live trees nonhigh sev plots with infection = 3.96%

# Count the total number of live trees in non-high-sevƒerity plots
total_trees_non_high_severity <- nrow(non_high_severity_plots)

# Print the result
total_trees_non_high_severity

#####$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ SEVERITY

# Calculate the range of infection severity for each category

# 1. High-severity fire plots
range_infection_severity_high_sev <- pila_report_df %>%
  filter(high_sev == 1) %>%
  summarize(min_severity = min(severity, na.rm = TRUE),
            max_severity = max(severity, na.rm = TRUE))

# 2. Non-high-severity fire plots
range_infection_severity_non_high_sev <- pila_report_df %>%
  filter(high_sev == 0) %>%
  summarize(min_severity = min(severity, na.rm = TRUE),
            max_severity = max(severity, na.rm = TRUE))

# 3. Plots with live trees only (using "Alive" in alive_status column)
range_infection_severity_live_trees <- pila_report_df %>%
  filter(alive_status == "Alive") %>%
  summarize(
    min_severity = ifelse(all(is.na(severity)), NA, min(severity, na.rm = TRUE)),
    max_severity = ifelse(all(is.na(severity)), NA, max(severity, na.rm = TRUE))
  )

# 4. All plots
range_infection_severity_all_plots <- pila_report_df %>%
  summarize(min_severity = min(severity, na.rm = TRUE),
            max_severity = max(severity, na.rm = TRUE))

# Print the results
cat("Range of Infection Severity:\n")
cat("High severity fire plots: Min =", range_infection_severity_high_sev$min_severity,
    ", Max =", range_infection_severity_high_sev$max_severity, "\n")
cat("Non-high severity fire plots: Min =", range_infection_severity_non_high_sev$min_severity,
    ", Max =", range_infection_severity_non_high_sev$max_severity, "\n")
cat("Plots with live trees only: Min =", range_infection_severity_live_trees$min_severity,
    ", Max =", range_infection_severity_live_trees$max_severity, "\n")
cat("All plots: Min =", range_infection_severity_all_plots$min_severity,
    ", Max =", range_infection_severity_all_plots$max_severity, "\n")


########################################################

# Filter for DEPO attacks only
depo_attacks <- pila_report_df %>%
  filter(beetles_treespresent == 1)



# Calculate the proportion of trees with beetles within each fire severity class
fire_sev_proportion <- pila_report_df %>%
  group_by(fire_sev) %>%
  summarize(
    total_trees = n(),  # Total number of trees in each fire severity class
    beetle_trees = sum(beetles_treespresent == 1)  # Trees with beetles present
  ) %>%
  mutate(proportion = beetle_trees / total_trees *100)  # Calculate proportion of trees with beetles



# Calculate the proportion of live trees with beetles within each fire severity class
fire_sev_proportion_live <- pila_report_df %>%
  filter(alive_status == "Alive") %>%  # Filter for live trees only
  group_by(fire_sev) %>%
  summarize(
    total_live_trees = n(),  # Total number of live trees in each fire severity class
    beetle_live_trees = sum(beetles_treespresent == 1)  # Live trees with beetles present
  ) %>%
  mutate(proportion = beetle_live_trees / total_live_trees)  # Calculate proportion of live trees with beetles




#####################################################################


# Create a new data frame with proportions by fire severity class
fire_sev_proportion_live <- fire_sev_proportion_live %>%
  mutate(proportion = count_live_mpb / total_live)  # Calculate proportion of live trees with MPB




# Calculate mean and standard error by fire severity class
summary_stats <- fire_sev_proportion_live %>%
  group_by(fire_sev) %>%
  summarise(
    mean_proportion = mean(proportion),
    se_proportion = sd(proportion) / sqrt(n())
  )




# Calculate the proportion of beetle-attacked trees for each plot
proportion_beetle_attacks <- pila_report_df %>%
  group_by(plotNum) %>%
  summarise(
    total_trees = n(),  # Total number of trees in each plot
    beetle_trees = sum(beetles_treespresent == 1),  # Count trees with beetle signs
    proportion_beetle_attacks = beetle_trees / total_trees  # Calculate the proportion
  )

# Display the result
print(proportion_beetle_attacks)



######## PART 2 #######
##### Figures Part 2

pila_report_df_figs <- pila_report_df

### I need to create new figures showing the average infection rate across dbh_class_cm

install.packages("plotrix")
library(plotrix)

# Calculate infection rate and standard error for each DBH class
pila_report_summary <- pila_report_df_figs %>%
  group_by(dbh_class_cm) %>%
  summarise(
    total_infected = mean(incidencepercent > 0, na.rm = TRUE),  # Count of infected trees
    total_trees = n(),                                         # Total count of trees
    infection_rate = (total_infected / total_trees) * 100,     # Infection rate as a percentage
    std_error = std.error(incidencepercent, na.rm = TRUE)   # Standard error of incidence percent
  )

pila_report_summary <- pila_report_df_figs %>%
  group_by(dbh_class_cm) %>%
  summarise(
    total_infected = sum(wpbr_present == 1, na.rm = TRUE),  # Count of infected trees where wpbr_present is 1
    total_trees = n(),                                      # Total count of trees
    infection_rate = (total_infected / total_trees) * 100,  # Infection rate as a percentage
    std_error = std.error(infection_rate, na.rm = TRUE)     # Standard error of the infection rate
  )





# Filter out rows with NA in incidencepercent column
pila_report_df_clean <- pila_report_df_figs %>%
  filter(!is.na(incidencepercent))

# Calculate average infection rate and standard error for each DBH class, excluding NA in incidencepercent
pila_report_summary <- pila_report_df_figs %>%
  filter(!is.na(incidencepercent)) %>%
  group_by(dbh_class_cm) %>%
  summarise(
    avg_infection_rate = mean(incidencepercent, na.rm = TRUE),
    std_error = std.error(incidencepercent)
  )


# Filter out rows with NA in either incidencepercent or dbh_class_cm, then summarize
pila_report_summary <- pila_report_df_figs %>%
  filter(!is.na(incidencepercent), !is.na(dbh_class_cm)) %>%  # Remove NA values in both columns
  group_by(dbh_class_cm) %>%                                  # Group by DBH class
  summarise(
    avg_infection_rate = mean(incidencepercent, na.rm = TRUE),  # Average infection rate
    std_error = std.error(incidencepercent, na.rm = TRUE)       # Standard error of the mean infection rate
  )


library(viridis)  # For the viridis color palette

# Filter out rows with NA in either incidencepercent or dbh_class_cm, then summarize
pila_report_summary <- pila_report_df_figs %>%
  filter(!is.na(incidencepercent), !is.na(dbh_class_cm)) %>%  # Remove NA values in both columns
  group_by(dbh_class_cm) %>%                                  # Group by DBH class
  summarise(
    avg_infection_rate = mean(incidencepercent, na.rm = TRUE),  # Average infection rate
    std_error = std.error(incidencepercent, na.rm = TRUE)       # Standard error of the mean infection rate
  )




### FIGURE 2D ####
# Plot 1: Boxplot with jittered points for infection severity by DBH class
p3 <- ggplot(non_zero_df, aes(x = factor(dbh_class_cm), y = severity, fill = factor(dbh_class_cm))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +           # Boxplot with adjusted opacity
  geom_jitter(width = 0.2, color = "black", size = 1, alpha = 0.7) +  # Jittered points with the same opacity
  scale_fill_viridis_d(name = "DBH Class (cm)", option = "D", direction = -1) +       # Consistent viridis palette
  labs(y = "Infection Severity", x = "Diameter Class (cm)") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),        # Remove x-axis text
    #axis.title.x = element_blank(),       # Remove x-axis title
    legend.position = "none"              # Hide legend in this individual plot
  )
p3





##$$$$$$$$$$$$#
# Define the individual plots, removing x-axis title from each
infectionsev_firesev <- ggplot(non_zero_df, aes(x = factor(fire_sev), y = severity, fill = factor(fire_sev))) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Semi-transparent box plot without outliers
  geom_jitter(color = "black", size = 1, width = 0.2, alpha = 0.6) +  # Jittered points for individual data
  scale_fill_viridis_d(name = "Fire Severity", option = "D") +  # Color-blind friendly palette
  labs(
    y = "Infection Severity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Place legend on the right
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank()  # Remove x-axis ticks
  )
infectionsev_firesev

infectionrate_firesev <- ggplot(table_df, aes(x = factor(fire_sev), y = incidencepercent, fill = factor(fire_sev))) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Semi-transparent box plot without outliers
  geom_jitter(color = "black", size = 1, width = 0.2, alpha = 0.6) +  # Jittered points for individual data
  scale_fill_viridis_d(name = "Fire Severity", option = "D") +  # Color-blind friendly palette
  labs(
    y = "Infection Rate (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Place legend on the right
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank()  # Remove x-axis ticks
  )
infectionrate_firesev
# Combine the plots and collect the legend
combined_plot <- infectionrate_firesev + infectionsev_firesev +
  plot_layout(guides = "collect") +  # Collects and shows only one shared legend
  plot_annotation(tag_levels = 'A')  # Automatically labels the plots as "A" and "B"

combined_plot


# Filter out rows with NA in either incidencepercent or fire_sev, then summarize
pila_report_summary_fire <- pila_report_df_figs %>%
  filter(!is.na(incidencepercent), !is.na(fire_sev)) %>%  # Remove NA values in both columns
  group_by(fire_sev) %>%                                 # Group by fire severity
  summarise(
    avg_infection_rate = mean(incidencepercent, na.rm = TRUE),  # Average infection rate
    std_error = std.error(incidencepercent, na.rm = TRUE)       # Standard error of the mean infection rate
  )




# Filter out rows with NA in either beetles_treespresent or dbh_class_cm, then summarize
beetle_summary_dbh <- pila_report_df_figs %>%
  filter(!is.na(beetles_treespresent), !is.na(dbh_class_cm)) %>%  # Remove NA values in both columns
  group_by(dbh_class_cm) %>%                                      # Group by DBH class
  summarise(
    avg_beetle_attack = mean(beetles_treespresent, na.rm = TRUE),  # Average beetle presence
    std_error = std.error(beetles_treespresent, na.rm = TRUE)      # Standard error of the mean
  )

# Create the plot with error bars and a viridis color palette
ggplot(beetle_summary_dbh, aes(x = factor(dbh_class_cm), y = avg_beetle_attack, fill = avg_beetle_attack)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = avg_beetle_attack - std_error, ymax = avg_beetle_attack + std_error),
                width = 0.2) +
  scale_fill_viridis(option = "D", name = "Avg Beetle Attack") +  # Use viridis and add a legend
  labs(
    # title = "Average Beetle Attack by DBH Class with Standard Error",
    x = "Diameter class (cm)",
    y = "Average Beetle Attack Presence (%)") +
  theme_minimal()


# Filter out rows with NA in either beetles_treespresent or dbh_class_cm, then summarize
beetle_summary_dbh <- pila_report_df_figs %>%
  filter(!is.na(beetles_treespresent), !is.na(dbh_class_cm)) %>%  # Remove NA values in both columns
  group_by(dbh_class_cm) %>%                                      # Group by DBH class
  summarise(
    avg_beetle_presence = mean(beetles_treespresent, na.rm = TRUE) * 100,  # Proportion * 100 for percentage
    std_error = std.error(beetles_treespresent, na.rm = TRUE) * 100        # Standard error as percentage
  )



# Create the plot with error bars and a viridis color palette
avgmpb_dbh <- ggplot(beetle_summary_dbh, aes(x = factor(dbh_class_cm), y = avg_beetle_presence, fill = factor(dbh_class_cm))) +
  geom_bar(stat = "identity", alpha = 0.7) +                            # Set opacity for bars
  geom_errorbar(aes(ymin = avg_beetle_presence - std_error, ymax = avg_beetle_presence + std_error),
                width = 0.2, alpha = 0.7) +                            # Set opacity for error bars
  scale_fill_viridis_d(option = "D", name = "Diameter class (cm)") +        # Use viridis discrete color scale
  labs(
    x = "Diameter class (cm)",
    y = "Average MPB Presence (%)"
  ) +
  theme_minimal() + theme(
    legend.position = "right",  # Place legend on the right
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank()  # Remove x-axis ticks
  )

avgmpb_dbh

# Filter out rows with NA in either beetles_treespresent or fire_sev, then summarize
beetle_summary_fire <- pila_report_df_figs %>%
  filter(!is.na(beetles_treespresent), !is.na(fire_sev)) %>%  # Remove NA values in both columns
  group_by(fire_sev) %>%                                      # Group by fire severity
  summarise(
    avg_beetle_presence = mean(beetles_treespresent, na.rm = TRUE) * 100,  # Proportion * 100 for percentage
    std_error = std.error(beetles_treespresent, na.rm = TRUE) * 100        # Standard error as percentage
  )


avgmpb_firesev <- ggplot(beetle_summary_fire, aes(x = factor(fire_sev), y = avg_beetle_presence)) +
  geom_bar(stat = "identity", alpha = 0.7, color = "black", fill = "grey") +  # Set a neutral fill color
  geom_errorbar(aes(ymin = avg_beetle_presence - std_error, ymax = avg_beetle_presence + std_error),
                width = 0.2) +
  labs(
    x = "Fire Severity",
    y = "Average Beetle Presence (%)"
  ) +
  theme_minimal()

avgmpb_firesev

avgmpb_dbh + avgmpb_firesev +
  plot_layout(guides = "collect") +  # Collects and shows only one shared legend
  plot_annotation(tag_levels = 'A')  # Automatically labels the plots as "A" and "B"


avginfectionrate_fire <- ggplot(pila_report_summary_fire, aes(x = factor(fire_sev), y = avg_infection_rate)) +
  geom_bar(stat = "identity", alpha = 0.7, color = "black", fill = "grey") +  # Set a neutral fill color
  geom_errorbar(aes(ymin = avg_infection_rate - std_error, ymax = avg_infection_rate + std_error),
                width = 0.2, alpha = 0.7) +
  labs(
    x = "Fire Severity",
    y = "Average Infection Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend

# Display the plot
avginfectionrate_fire

##### FIGURE 3B ####
infection_severity_fire <- ggplot(non_zero_df, aes(x = factor(fire_sev), y = severity)) +
  geom_boxplot(alpha = 0.7, color = "black", fill = "grey", outlier.shape = NA) +  # Set a neutral fill color
  geom_jitter(width = 0.2, color = "black", size = 1, alpha = 0.5) +  # Add jittered points in black
  labs(
    x = "Fire Severity",
    y = "Infection Severity"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend
infection_severity_fire

# Display the plot
infection_severity_fire +
  plot_layout(guides = "collect") +  # Collects and shows only one shared legend
  plot_annotation(tag_levels = 'A')  # Automatically labels the plots as "A" and "B"


avginfectionrate_fire + infection_severity_fire +
  plot_layout(guides = "collect") +  # Collects and shows only one shared legend
  plot_annotation(tag_levels = 'A')  # Automatically labels the plots as "A" and "B"




pila_report_summary <- pila_report_df_figs %>%
  filter(!is.na(dbh_class_cm)) %>%
  group_by(dbh_class_cm) %>%
  summarise(
    total_infected = sum(wpbr_present == 1, na.rm = TRUE),  # Count of infected trees where wpbr_present is 1
    total_trees = n(),                                      # Total count of trees
    infection_rate = (total_infected / total_trees) * 100,  # Infection rate as a percentage
    std_error = std.error(infection_rate, na.rm = TRUE)     # Standard error of the infection rate
  )




##### FIGURE 2C ########
newinfectrate_dbhclass <- ggplot(pila_report_summary, aes(x = factor(dbh_class_cm), y = infection_rate, fill = factor(dbh_class_cm))) +
  geom_col(alpha = 0.7) +  # Add transparency with alpha
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +  # Use the viridis color palette for discrete values
  labs(
    fill = "Diameter Class (cm)",  # Add legend title
    y = "Infection Rate (%)",
    x = "Diameter Class (cm)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Hide x-axis text if desired
    # axis.title.x = element_blank()   # Hide x-axis title if desired
  )

newinfectrate_dbhclass

pila_report_summary <- pila_report_df_figs %>%
  filter(!is.na(dbh_class_cm), alive_status == "Alive") %>%
  group_by(dbh_class_cm) %>%
  summarise(
    total_beetles_present = sum(beetles_treespresent == 1, na.rm = TRUE),  # Count of trees with beetles present
    total_livetrees = n(),                                                     # Total count of trees
    beetle_presence_rate = (total_beetles_present / total_livetrees) * 100,    # Beetle presence rate as a percentage
    std_error = std.error(beetle_presence_rate, na.rm = TRUE)              # Standard error of the beetle presence rate
  )

pila_report_summary_firesev_live <- pila_report_df_figs %>%
  filter(alive_status == "Alive") %>%  # Filter for live trees
  group_by(fire_sev) %>%
  summarise(
    total_livetrees = n(),                                                    # Total count of live trees
    total_beetles_present = sum(beetles_treespresent == 1, na.rm = TRUE),     # Count of live trees with beetles present
    beetle_presence_rate = (total_beetles_present / total_livetrees) * 100,   # Beetle presence rate as a percentage
    std_error = std.error(beetle_presence_rate, na.rm = TRUE)                 # Standard error of the beetle presence rate
  )

##### FIGURE 4A ####
newbeetlerate_dbhclass <- ggplot(pila_report_summary, aes(x = factor(dbh_class_cm), y = beetle_presence_rate, fill = factor(dbh_class_cm))) +
  geom_col(alpha = 0.7) +  # Add transparency with alpha
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +  # Use the viridis color palette for discrete values
  labs(
    fill = "Diameter Class (cm)",  # Add legend title
    y = "Beetle Attack (%)", # Update y-axis label
    x = "Diameter class (cm)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Hide x-axis text if desired
    # axis.title.x = element_blank()   # Hide x-axis title if desired
  )

newbeetlerate_dbhclass

### FIGURE 4B ###
# Create the histogram plot with the same gray color for all bars
beetle_histogram <- ggplot(pila_report_summary_firesev_live, aes(x = fire_sev, y = beetle_presence_rate)) +
  geom_col(fill = "grey50", alpha = 0.7) +  # Set a uniform gray color and add transparency
  labs(
    # title = "Beetle Presence Rate by Fire Severity",
    x = "Fire Severity",
    y = "Beetle Attack (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1)  # Rotate x-axis labels for readability
  )
beetle_histogram



####### FIGURE 2 #####
hist_nocolor_infectrate_nonzero + sev_histplot + newinfectrate_dbhclass + p3 + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')


newmpb_firesev <- ggplot(beetle_summary_fire, aes(x = factor(fire_sev), y = avg_beetle_presence)) +
  geom_bar(stat = "identity", alpha = 0.7, color = "black", fill = "grey") +  # Set a neutral fill color
  geom_errorbar(aes(ymin = avg_beetle_presence - std_error, ymax = avg_beetle_presence + std_error),
                width = 0.2) +
  labs(
    x = "Fire Severity",
    y = "Average Beetle Presence (%)"
  ) +
  theme_minimal()

newmpb_firesev

avgmpb_firesev







#######%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######
##### FIGURE 2 ######
#######%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######
hist_nocolor_infectrate_nonzero + sev_histplot + newinfectrate_dbhclass + p3 + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')


##### FIGURE 4 ####
newbeetlerate_dbhclass + beetle_histogram + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A')





pila_report_summary_fire <- pila_report_df_figs %>%
  filter(!is.na(fire_sev)) %>%  # Ensure no NAs in fire_sev
  group_by(fire_sev) %>%
  summarise(
    total_infected = sum(wpbr_present == 1, na.rm = TRUE),  # Count of trees with infections
    total_trees = n(),                                              # Total count of trees
    infection_rate = (total_infected / total_trees) * 100,          # Infection rate as a percentage
    std_error = std.error(infection_rate, na.rm = TRUE)             # Standard error of the infection rate
  )



#### FIGURE 3A ####
# Create the plot with error bars and a uniform gray color
newinfectionrate_fire <- ggplot(pila_report_summary_fire, aes(x = factor(fire_sev), y = infection_rate)) +
  geom_bar(stat = "identity", fill = "grey50", alpha = 0.7, color = "black") +  # Use a uniform gray color
  geom_errorbar(aes(ymin = infection_rate - std_error, ymax = infection_rate + std_error),
                width = 0.2) +
  labs(
    x = "Fire Severity",
    y = "Infection Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend
newinfectionrate_fire


#### FIGURE 3 #####
newinfectionrate_fire + infection_severity_fire + plot_annotation(tag_levels = "A")


####### PART 3 ##########
## Mortality and Recruits Figures ###


######
#########
###########
################
# Recruitment across plot type (Rim vs Primary)

pila_report_df_clean <- pila_report_df
# Create a full list of plots with plot type (plot_rim)
all_plots <- pila_report_df_clean %>%
  select(plotNum, plot_rim) %>%
  distinct()  # Ensure unique plotNum and plot_rim combinations

# Calculate recruitment for each plot that has DBH_cm between 0 and 3
recruitment_per_plot <- pila_report_df_clean %>%
  filter(DBH_cm >= 0 & DBH_cm <= 3) %>%  # Filter for recruitment (DBH 0-3)
  group_by(plotNum) %>%
  summarise(total_recruitment = n(), .groups = "drop")  # Count recruitment per plot

# Merge with the full list of plots to ensure all plots are included
total_recruitment_per_plot <- all_plots %>%
  left_join(recruitment_per_plot, by = "plotNum") %>%  # Left join to keep all plots
  mutate(total_recruitment = coalesce(total_recruitment, 0))  # Fill NA with 0 for plots with no recruitment


# Calculate average recruitment and SE by plot type
recruitment_summary <- total_recruitment_per_plot %>%
  group_by(plot_rim) %>%
  summarise(
    mean_recruitment = mean(total_recruitment),  # Calculate mean recruitment per plot type
    se_recruitment = sd(total_recruitment) / sqrt(n()),  # Calculate standard error
    .groups = "drop"
  )


#### FIGURE 6A ####
# Plot average recruitment across plot types with SE bars
recruitavg_rimprim <- ggplot(recruitment_summary, aes(x = plot_rim, y = mean_recruitment, fill = plot_rim)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_recruitment - se_recruitment, ymax = mean_recruitment + se_recruitment),
                width = 0.2, color = "black") +  # Add SE bars
  scale_fill_manual(values = c("grey", "grey")) +  # Use grey for both columns and remove legend
  labs(
    y = "Mean Recruitment"  # Only y-axis title
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",               # Remove legend
    axis.title.x = element_blank(),         # Remove x-axis title
    axis.text.x = element_text(angle = 0),  # Keep x-axis text without rotation
    axis.ticks.x = element_blank()          # Remove x-axis ticks
  )



recruitavg_rimprim



######
#########
###########
################
# Recruitment across Fire Sev

# Create a full list of plots with fire severity levels (fire_sev)
all_plots_fire_sev <- pila_report_df_clean %>%
  select(plotNum, fire_sev, plot_rim) %>%
  distinct()  # Ensure unique plotNum and fire_sev combinations

recruitment <- total_recruitment_per_plot %>%
  select(plotNum, total_recruitment) %>%
  full_join(all_plots_fire_sev)

# Calculate recruitment for each plot that has DBH_cm between 0 and 3
recruitment_per_plot_fire_sev <- pila_report_df_clean %>%
  filter(DBH_cm >= 0 & DBH_cm <= 3) %>%  # Filter for recruitment (DBH 0-3)
  group_by(plotNum) %>%
  select(-fire_sev)# Count recruitment per plot


# Merge with the full list of plots to ensure all plots are included
recruitment_per_plot_fire_sev <- all_plots_fire_sev %>%
  full_join(recruitment_per_plot_fire_sev, by = "plotNum") # Left join to keep all plots


# Calculate average recruitment and SE by fire severity
recruitment_df <- recruitment %>%
  group_by(fire_sev) %>%
  summarise(
    mean_recruitment = mean(total_recruitment, na.rm = TRUE),  # Calculate mean recruitment per fire severity level
    se_recruitment = sd(total_recruitment, na.rm = TRUE) / sqrt(n()),  # Calculate standard error
    .groups = "drop"
  )


#### FIGURE 6C ####
# Plot average recruitment across fire severity levels with SE bars
recruitmentavg_firesev <- ggplot(recruitment_df, aes(x = fire_sev, y = mean_recruitment, fill = fire_sev)) +
  geom_bar(stat = "identity", alpha = 0.7, fill = "grey") +  # Set all bars to grey
  geom_errorbar(aes(ymin = mean_recruitment - se_recruitment, ymax = mean_recruitment + se_recruitment),
                width = 0.2, color = "black") +  # Add SE bars
  labs(
    y = "Mean Recruitment",
    x = "Fire Severity"
    # Only y-axis title
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",               # Remove legend
    # axis.title.x = element_blank(),         # Remove x-axis title
    axis.text.x = element_text(angle = 0),  # Keep x-axis text without rotation
    axis.ticks.x = element_blank()          # Remove x-axis ticks
  )


recruitmentavg_firesev


######
#########
###########
################
# Recruitment across VPD

# Define custom bins using `cut()` with explicit intervals for vpdmax
all_plots_vpdmax <- pila_report_df_clean %>%
  filter(!is.na(vpdmax)) %>%
  mutate(vpdmax_bin = cut(
    vpdmax,
    breaks = c(12, 13, 14, 15, 16, 17, Inf),  # Define bin edges
    labels = c("12 - 13", "13 - 14", "14 - 15", "15 - 16", "16 - 17", "17+"),  # Bin labels
    include.lowest = TRUE,
    right = FALSE  # Make intervals left-inclusive, e.g., [12, 13)
  )) %>%
  select(plotNum, vpdmax_bin) %>%
  distinct()  # Ensure unique plotNum and vpdmax_bin combinations


# Calculate recruitment for plots with trees in DBH 0-3 range
recruitment_per_plot_vpdmax <- pila_report_df_clean %>%
  filter(DBH_cm >= 0 & DBH_cm <= 3) %>%
  group_by(plotNum) %>%
  summarise(total_recruitment = n(), .groups = "drop")  # Count recruitment per plot

# Calculate recruitment only for plots that have trees with DBH_cm between 0 and 3
recruitment_per_plot_vpdmax <- pila_report_df_clean %>%
  filter(DBH_cm >= 0 & DBH_cm <= 3) %>%
  group_by(plotNum) %>%
  summarise(total_recruitment = n(), .groups = "drop")  # Count recruitment per plot

# Merge with the full list of plots to include all plots, setting recruitment to 0 where missing
total_recruitment_per_plot_vpdmax <- all_plots_vpdmax %>%
  left_join(recruitment_per_plot_vpdmax, by = "plotNum") %>%
  mutate(total_recruitment = coalesce(total_recruitment, 0))  # Fill NAs with 0 for plots with no recruitment


# Calculate average recruitment and SE by vpdmax bin, keeping zero-recruitment plots in the calculation
recruitment_summary_vpdmax <- total_recruitment_per_plot_vpdmax %>%
  group_by(vpdmax_bin) %>%
  summarise(
    mean_recruitment = mean(total_recruitment, na.rm = TRUE),  # Mean recruitment per vpdmax bin
    se_recruitment = sd(total_recruitment, na.rm = TRUE) / sqrt(n()),  # Standard error
    .groups = "drop"
  )

#### FIGURE 6D #####
# Plot average recruitment across vpdmax bins with SE bars, viridis palette, and customized axis
recruitment_vpd <- ggplot(recruitment_summary_vpdmax, aes(x = vpdmax_bin, y = mean_recruitment, fill = vpdmax_bin)) +
  geom_bar(stat = "identity", alpha = 0.7) +  # Use default width with alpha
  geom_errorbar(aes(ymin = mean_recruitment - se_recruitment, ymax = mean_recruitment + se_recruitment),
                width = 0.2, color = "black") +  # Add SE bars
  scale_fill_viridis_d(option = "D", direction = -1, name = "VPDmax Range") +  # Apply viridis palette with legend
  labs(
    x = "VPDmax Range",  # x-axis title only
    y = "Mean Recruitment"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),         # Remove x-axis text labels
    axis.ticks.x = element_blank(),        # Remove x-axis ticks
    legend.position = "right"              # Position legend on the right
  )

recruitment_vpd


######


# Create a new column with the summed total_recruitment by plot_rim
recruitment <- recruitment %>%
  group_by(plot_rim) %>%
  mutate(total_recruitment_by_plot_rim = sum(total_recruitment, na.rm = TRUE)) %>%
  ungroup()


# Create a new column with the count of plots by plot_rim
recruitment <- recruitment %>%
  group_by(plot_rim) %>%
  mutate(plottype_total = n_distinct(plotNum)) %>%  # Count distinct plots in each plot_rim group
  ungroup()

# Create a new column with the total recruitment count by fire_sev class
recruitment <- recruitment %>%
  group_by(fire_sev) %>%
  mutate(totalrecruit_fire = sum(total_recruitment, na.rm = TRUE)) %>%
  ungroup()

# Create a new column with the count of plots by fire_sev
recruitment <- recruitment %>%
  group_by(fire_sev) %>%
  mutate(firetype_total = n_distinct(plotNum)) %>%  # Count distinct plots within each fire_sev group
  ungroup()


write.csv(recruitment, "recruitment_data.csv", row.names = FALSE)




# Arrange the plots with two on top and one on the bottom
(recruitavg_rimprim + recruitmentavg_firesev) / recruitment_by_infection + recruitment_vpd +
  plot_annotation(tag_levels = "A")


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
  scale_fill_viridis_d(option = "D", direction = -1, name = "Diameter Class (cm)") +  # Use discrete viridis palette with legend
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
  scale_fill_viridis_d(option = "D", direction = -1, name = "VPDmax Range") +  # Apply viridis palette and add legend
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
  scale_fill_viridis_d(option = "D", direction = -1, name = "Infection Rate") +  # Apply viridis palette with discrete scale
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
  scale_fill_viridis_d(option = "D", direction = -1, name = "Infection Rate (%)") +
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
  scale_fill_viridis_d(option = "D", direction = -1, name = "Infection Rate (%)") +
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
  scale_fill_viridis_d(option = "D", direction = -1, name = "VPDmax Range") +  # Apply viridis palette with discrete scale
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
## FIGURE 6
recruitavg_rimprim + recruitment_infectionrate + recruitmentavg_firesev + recruitment_vpd + plot_annotation(tag_levels = "A")


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
  scale_fill_viridis_d(option = "D", direction = -1, name = "DBH Class (cm)") +  # Use discrete viridis palette with legend
  labs(title = "Count of Dead Trees Across DBH Classes", x = "DBH Class (cm)", y = "Dead Tree Count") +
  theme_minimal()

# Plotting with viridis color palette, opacity, and a customized x-axis
mortality_dbh <- ggplot(dbh_class_dead_count, aes(x = dbh_class_cm, y = dead_count, fill = as.factor(dbh_class_cm))) +
  geom_bar(stat = "identity", alpha = 0.7) +  # Adjust alpha for opacity
  scale_fill_viridis_d(option = "D", direction = -1, name = "Diameter Class (cm)") +  # Use discrete viridis palette with legend
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
  scale_fill_viridis_d(option = "D", direction = -1, name = "VPDmax Range") +  # Apply viridis palette and add legend
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
  scale_fill_viridis_d(option = "D", direction = -1, name = "Infection Rate") +  # Apply viridis palette with discrete scale
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
  scale_fill_viridis_d(option = "D", direction = -1, name = "Infection Rate (%)") +
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
  scale_fill_viridis_d(option = "D", direction = -1, name = "Infection Rate (%)") +
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
  scale_fill_viridis_d(option = "D", direction = -1, name = "VPDmax Range") +  # Apply viridis palette with discrete scale
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






