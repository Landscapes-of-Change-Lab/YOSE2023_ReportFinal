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



