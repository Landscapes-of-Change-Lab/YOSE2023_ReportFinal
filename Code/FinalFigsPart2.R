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
    x = "Diameter class (cm)"
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




