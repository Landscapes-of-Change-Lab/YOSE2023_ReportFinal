### Inventory of White Pine Blister Rust, ###
## Fire, and Bark Beetles in Sugar Pine
# at Yosemite National Park #

## Authors: Michelle D. Mohr, Jennifer Cribbs, Andrew Latimer, and Joan Dudney

# Data collected in YOSE June - October 2023


## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ##

## Load libraries ##

library(tidyverse)
library(here)
library(librarian)

## Joan's code chunk part 1 ##
# setting a theme? #
theme_set(
  theme_bw(base_size = 17)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, readxl, viridis, patchwork)


## Michelle set working directory ##
setwd("/Users/michellemohr/Desktop/YOSE_report_2024")
files <- list.files()
outdir <- "/Users/michellemohr/Desktop/YOSE_report_2024"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#PILA data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## reading in PILA data
folders <- list.dirs(outdir)[-c(1,4)]

pila_list <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "PILAdata")
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>%
      dplyr::select(plotID, plot_type, treeNum, DBH_cm, slope, pitchTubes, exitHoles,
                    activeBranchCanker, inactiveBranchCanker,
                    activeBoleCanker, inactiveBoleCanker,
                    notes, plot_elevation_ft, trans_length, width,
                    percentLive, ribes_50m, ribes_100m, ribes_150m, ribes_200m)
                  # seedlings_50m, seedlings_100m, seedlings_150m, seedlings_200m)
    pila_list <- rbind(pila_list, xlsfile)
  }
}

### NOTE: picking up 1 NA row (will drop later in code - so only 3 plots have NAs as placeholders
### (NO PILA in these plots 7, 8, and 29)).
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#ASSOC data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## reading in ASSOC TREE data
folders <- list.dirs(outdir)[-c(1,4)]

assoc_list <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "YPE_Treedata")
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>%
      dplyr::select(plot, treeNum, DBH_cm, height_m, species, dOut_m, dSideR_m, dSideL_m, notes, percentLive)
    assoc_list <- rbind(assoc_list, xlsfile)
  }
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#VEG data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## reading in VEG data
folders <- list.dirs(outdir)[-c(1,4)]

veg_list <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "YPE_Understory")
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>%
      dplyr::select(plotID, species1, species2, species3, species4,
                    species5, species6, species7, species8, species9,
                    species10,
                    assoc1, assoc2, assoc3, assoc4, assoc5,
                    assoc6, assoc7, assoc8, assoc9, assoc10)
    veg_list <- rbind(veg_list, xlsfile)
  }
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#FIRE data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

firedata <- read_csv(here("FIREdata.csv"))

firedataCompare <-read_csv(here("fireCompare.csv"))

fire_severity <- firedataCompare %>%
  select(plotID, Fire_Severity_plotAverge)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#CLIMATE data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

climatedata <- read_csv(here("updatedPRISMdata.csv"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#Changing plot numbers to have 1-59
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# create plot_num column which will change the plots to numerical order#

pila_list <- pila_list %>%
  mutate(plotNum = dense_rank(plotID))

assoc_list <- assoc_list %>%
  mutate(plotNum = dense_rank(plot))

veg_list <- veg_list %>%
  mutate(plotNum = dense_rank(plotID))

climatedata <- climatedata %>%
  mutate(plotNum = dense_rank(PlotID))

fire_severity <- fire_severity %>%
  mutate(plotNum = dense_rank(plotID))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#DROPPING CERTAIN ROWS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Drop rows where plotID column has NA values
pila_list <- pila_list[!is.na(pila_list$plotID), ]

# Drop specific row for plotNum 48 treeNum 11
pila_list <- pila_list %>%
  filter(!(plotNum == 48 & treeNum == 11))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#ALIVE STATUS using pila_list
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#add new column to pila_list dataframe called "alive_status"
pila_list <- pila_list %>%
  mutate(alive_status = ifelse(percentLive == 0, "Dead", "Alive"))

#create new dataframe called 'pila_counts'
pila_counts <- pila_list %>%
  group_by(alive_status) %>%
  summarize(count = n())

# There are 1730 pila trees (+3 NAs)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#ALIVE STATUS using assoc_list
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#add new column to pila_list dataframe called "alive_status"
assoc_list <- assoc_list %>%
  mutate(alive_status = ifelse(percentLive == 0, "Dead", "Alive"))

#create new dataframe called 'assoc_counts'
assoc_counts <- assoc_list %>%
  group_by(alive_status) %>%
  summarize(count = n())

# There are 3310 assoc trees (+1 NA)


######## IMPORTANT STATS #############
# Looks like we have three plots without PILA 7, 8, and 25 (old YPE 29)
# Total trees says 1734 but really is probably 1730 ( 4 NAs are placeholders for plots 7, 8, and 25 without PILA data. 1 row is NAs being picked up)

# Figure out how many trees are alive vs dead for each plot
# Create new dataframe pilaplots_list with selected columns from pila_list
pilaplots_list <- pila_list[, c("plotNum", "treeNum", "alive_status")]

# Create new columns Alive and Dead based on alive_status
pilaplots_list <- pilaplots_list %>%
  mutate(Alive = ifelse(alive_status == "Alive", 1, 0),
         Dead = ifelse(alive_status == "Alive", 0, 1)) %>%
  select(-alive_status)  # Remove the original alive_status column if needed

# Create new column plot_alive with the sum of alive trees per plotNum
pilaplots_list <- pilaplots_list %>%
  group_by(plotNum) %>%
  mutate(plot_alive = sum(Alive))

# Create new column plot_dead with the sum of dead trees per plotNum
pilaplots_list <- pilaplots_list %>%
  group_by(plotNum) %>%
  mutate(plot_dead = sum(Dead))

####
pilaplots <- pilaplots_list %>%
  distinct(plotNum, plot_alive, plot_dead)


### IMPORTANT ########
# 3 plots no PILA
#(plotNum 7, 8, and 25)
# 3 plots with only DEAD PILA
####
#(plotNum 5: 1 dead tree, 32: 1 dead tree, and 44: 6 dead trees - with recruitment)


# Calculate the number of alive and dead trees per plot
plot_summary <- pilaplots_list %>%
  group_by(plotNum) %>%
  summarise(plot_alive = sum(Alive),
            plot_dead = sum(Dead)) %>%
  pivot_longer(cols = c(plot_alive, plot_dead), names_to = "status", values_to = "count")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
###########       WPBR PILA infection     #################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# Create a new dataframe with selected columns from pila_list
pilaWPBR <- pila_list[, c("plotNum", "treeNum", "plot_type", "DBH_cm", "activeBoleCanker", "activeBranchCanker", "inactiveBoleCanker", "inactiveBranchCanker", "alive_status")]

# Replace NA values in the specified columns with 0
pilaWPBR$activeBoleCanker[is.na(pilaWPBR$activeBoleCanker)] <- 0
pilaWPBR$activeBranchCanker[is.na(pilaWPBR$activeBranchCanker)] <- 0
pilaWPBR$inactiveBoleCanker[is.na(pilaWPBR$inactiveBoleCanker)] <- 0
pilaWPBR$inactiveBranchCanker[is.na(pilaWPBR$inactiveBranchCanker)] <- 0

# Create the new column WPBRinfected
pilaWPBR$WPBRinfected <- as.integer(rowSums(pilaWPBR[, c("activeBoleCanker", "activeBranchCanker", "inactiveBoleCanker", "inactiveBranchCanker")]) > 0)

# Group by plotNum and calculate the sum of WPBRinfected per plot
pilaWPBR$plotWPBRinfected <- ave(pilaWPBR$WPBRinfected, pilaWPBR$plotNum, FUN = sum)

# Create new column with the number of unique treeNum per plot
pilaWPBR <- pilaWPBR %>%
  group_by(plotNum) %>%
  mutate(trees_plot = n_distinct(treeNum))

# Create new column with the number of trees in the whole study
pilaWPBR <- pilaWPBR %>%
  mutate(total_trees = nrow(.))

#### work with new pilaWPBRclean
# Define the plots to exclude
plots_to_exclude <- c(7, 8, 25)

# Create a new dataframe excluding the specified plots
pilaWPBRclean <- pilaWPBR %>%
  filter(!plotNum %in% plots_to_exclude)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############# INCIDENCE ####### EXTENT ######### SEVERITY ############ INFECTION RATE ######################
#Incidence (Percent) =
  ### number of individuals in the plot with infections/number of trees in the plot * 100
#Extent =
  ### number of plots with at least one infection/total number of plots surveyed
#Severity =
  ### FORMULA ### cs = (25 - DBH) / 5
#Infection Rate =
  ### number of trees in plot with WPBR/number of PILA in the plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##### Incidence Percent ######

# Create new column called incidencepercent using the number of indiv. infected within a plot (plotWPBRinfected)
# divided by the total number of PILA in the plot (trees_plot)
# and multiply it by 100 to get percent

# Create new column incidencepercent
pilaWPBRclean <- pilaWPBRclean %>%
  mutate(incidencepercent = (plotWPBRinfected / trees_plot) * 100)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############# Extent BEGIN ###################
# ### number of plots with at least one infection/total number of plots surveyed
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Create the WPBRone column indicating if a tree has at least one infection
pilaWPBRclean <- pilaWPBRclean %>%
  mutate(WPBRone = ifelse(activeBoleCanker > 0 |
                            activeBranchCanker > 0 |
                            inactiveBoleCanker > 0 |
                            inactiveBranchCanker > 0,
                          1, 0))

# Now summarize to determine if any tree in the plot is infected
plotWPBRone <- pilaWPBRclean %>%
  group_by(plotNum) %>%
  summarize(plotWPBRone = as.integer(any(WPBRone == 1)), .groups = "drop")

# View the resulting summary
print(plotWPBRone)

# Sum the values in the plotWPBRone column
total_WPBRone <- sum(plotWPBRone$plotWPBRone, na.rm = TRUE)

# Print the result
print(total_WPBRone)

# Count the number of unique plots in the plotNum column
num_unique_plots <- n_distinct(plotWPBRone$plotNum)

# Print the result
print(num_unique_plots)


###### IMPORTANT EXTENT NOTES ########
# 27 plots have at least one infection.
# 26 plots do not have any infections.
# There are a total of 53 plots
# (original plot total of 59, but plots 5, 7, 8, 25, 32, and 44 either have no PILA or only dead PILA)
# (plot 44 is being included because it has 6 dead PILA with recruitment)
# EXTENT =
#### 27/53 plots with infection = 50.94%

# Step 1: Summarize by plotNum to calculate if each plot has any WPBR infection (1 = infected, 0 = not infected)
plot_extent_summary <- plotWPBRone %>%
  group_by(plotNum) %>%
  summarise(WPBR_infection = ifelse(sum(plotWPBRone) > 0, 1, 0))

# Step 2: Count the number of infected vs. non-infected plots
plot_extent_counts <- plot_extent_summary %>%
  group_by(WPBR_infection) %>%
  summarise(count = n())


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############## Extent END ####################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############## Severity BEGIN ###############
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# ### severity = cs = (50 - DBH) / 5
## cs =
# 0 -> 0 branch cankers
# 1 -> 1 -3 branch cankers
# 2 -> 4 - 9 branch cankers
# 3 -> 10 - 25 branch cankers
# 4 -> 25 or more branch cankers
# 5 -> bole canker

# NOTE:
# * if tree diameter is greater than 50 inches, then DBH = 50 *
# * unless cs = 0, then S = 0 *
### note: I am going to have to change the DBH column from cm -> inches

#### NEED TO DO THIS WITH ONLY LIVE TREES ####

###
pilaWPBRclean <- pilaWPBRclean %>%
  mutate(
    # Calculate total branch cankers
    total_branch_cankers = activeBranchCanker + inactiveBranchCanker,

    # Assign canker severity based on the new criteria
    cankerseverity = case_when(
      activeBoleCanker == TRUE | inactiveBoleCanker == TRUE ~ 5,  # Presence of bole canker takes priority
      total_branch_cankers == 1 ~ 1,  # 1 branch canker = severity 1
      total_branch_cankers == 2 ~ 2,  # 2 branch cankers = severity 2
      total_branch_cankers >= 3 & total_branch_cankers <= 4 ~ 3,  # 3-4 branch cankers = severity 3
      total_branch_cankers >= 5 & total_branch_cankers <= 7 ~ 4,  # 5-7 branch cankers = severity 4
      TRUE ~ 0  # No branch cankers or other cases = severity 0
    )
  )



### Clean DBH for severity formula
### note: I am going to have to change the DBH column from cm -> inches

pilaWPBRclean <- pilaWPBRclean %>%
  mutate(DBH_in = DBH_cm * 0.393701)

## note: I'm going to need to make a new DBH column that takes the original DBH_in column and makes anything that is above 50 = 50

# Create a new DBH column where any value greater than 50 is set to 50
pilaWPBRclean <- pilaWPBRclean %>%
  mutate(DBH_sevformula = ifelse(DBH_in > 50, 50, DBH_in))

#### Severity Formula
pilaWPBRclean <- pilaWPBRclean %>%
  mutate(
    severity = if_else(cankerseverity == 0, 0, (cankerseverity + (50 - DBH_sevformula)) / 5)
  )
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
################ Severity END ################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
########## Infection Rate BEGIN ##############
# ### number of trees in plot with WPBR/number of PILA in the plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### NEED TO DO THIS WITH ONLY LIVE TREES ####

pilaWPBRclean <- pilaWPBRclean %>%
  mutate(infectionrate = (plotWPBRinfected / trees_plot))

# Step 1: Filter for only alive trees
alive_trees_data <- pilaWPBRclean %>%
  filter(alive_status == "Alive")  # Assuming "alive_status" column is present

# Step 2: Group by plotNum and calculate the infection rate for each plot
alive_infection_rate <- alive_trees_data %>%
  group_by(plotNum) %>%
  summarise(
    total_alive_trees = n(),  # Count of alive trees in each plot
    infected_alive_trees = sum(WPBRinfected == 1, na.rm = TRUE),  # Count of infected alive trees
    infection_rate_alive = infected_alive_trees / total_alive_trees  # Infection rate
  )


# Calculate total number of infected live trees
total_infected_alive_trees <- sum(alive_infection_rate$infected_alive_trees, na.rm = TRUE)

# Print the result
total_infected_alive_trees

# Calculate the total number of alive trees
total_alive_trees <- pilaWPBRclean %>%
  filter(alive_status == "Alive") %>%   # Filter for alive trees
  nrow()                                # Count the number of rows

# Print the total number of alive trees
total_alive_trees


#### IMPORTANT STATS: 57/1400 alive trees are infected = 4.07% infection ##

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############ Infection Rate END #############
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

###### Let's separate random and high severity plots ##########
###### to see how infection rates change ########

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#$$$$$$$$$$$$$$$$$$$$$$#
      # EXTENT #
#$$$$$$$$$$$$$$$$$$$$$$#
# Step 1: Summarize the number of plots with WPBR infections by plot type
plottype_summary <- pilaWPBRclean %>%
  group_by(plotNum, plot_type) %>%
  summarize(wpbr_present = sum(WPBRinfected > 0)) %>%  # 1 if WPBR is present, 0 otherwise
  ungroup() %>%
  group_by(plot_type) %>%
  summarize(
    total_plots = n_distinct(plotNum),      # Count total plots
    plots_with_wpbr = sum(wpbr_present > 0) # Count plots with WPBR presence
  )

# View the summary table
print(plottype_summary)

#### IMPORTANT STATS:

# 53 total plots

# random -> 36 plots total (22 of the random plots have infections)
## random 22/36 = 61.1%

# high severity -> 17 plots total (5 of the high severity plots have infections)
## high severity 5/17 = 29.41%

### both random and high severity = 27/53 = 50.94%

#$$$$$$$$$$$$$$$$$$$$$$#
   # INFECTION RATE #
#$$$$$$$$$$$$$$$$$$$$$$#
# Step 1: Group by plotNum and plot_type, calculate infection rate for each plot
plottypeWPBRtreesummary <- pilaWPBRclean %>%
  group_by(plotNum, plot_type) %>%
  summarize(
    trees_plot = n(),                             # Total number of trees in the plot
    plotWPBRinfected = sum(WPBRinfected > 0)      # Number of infected trees in the plot
  ) %>%
  mutate(
    infectionrate = plotWPBRinfected / trees_plot  # Calculate infection rate
  ) %>%
  ungroup()

# View the result
print(plottypeWPBRtreesummary)

# OR #

# Step 2: Calculate average infection rate per plot type
infectionrate_summary <- pilaWPBRclean %>%
  group_by(plot_type) %>%
  summarize(
    total_plots = n_distinct(plotNum),            # Total number of plots in each plot type
    avg_infectionrate = mean(infectionrate, na.rm = TRUE)  # Average infection rate per plot type
  )

# View the infection rate summary
print(infectionrate_summary)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
###########         PLOT CHARACTERISTICS        ############
# objective: create a table that has the following
### DBH
### height
### presence/absence of Ribes
### number of standing dead trees
### number of trees with signs of DEPO/DEVA/weevils
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#RIBES pila_list
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### work with new pila_list_clean
# Define the plots to exclude
plots_to_exclude <- c(7, 8, 25)

# Create a new dataframe excluding the specified plots
pila_list_clean <- pila_list %>%
  filter(!plotNum %in% plots_to_exclude)

# create a ribes A/P (0/1) column #
pila_list_clean <- pila_list_clean %>%
  mutate(ribes = ifelse(
    !is.na(ribes_50m) & !(ribes_50m %in% c("N", "None", "NONE")) |
      !is.na(ribes_100m) & !(ribes_100m %in% c("N", "None", "NONE")) |
      !is.na(ribes_150m) & !(ribes_150m %in% c("N", "None", "NONE")) |
      !is.na(ribes_200m) & !(ribes_200m %in% c("N", "None", "NONE")),
    1, 0))


#create new dataframe called 'ribespila_counts'
ribespila_counts <- pila_list_clean %>%
  group_by(plotNum) %>%
  summarize(ribes_plot = ifelse(any(ribes == 1), 1, 0))

# Find the sum of ribes_plot
ribes_plot_sum <- sum(ribespila_counts$ribes_plot)

############# Important Stats ################
### 36 out of 53 plots have ribes = 67.92% ###

#### Create visuals #####
# Create a summary dataframe for plotting
ribes_plot_summary <- ribespila_counts %>%
  group_by(ribes_plot) %>%
  summarize(count = n(), .groups = "drop")  # Count the number of plots for each presence/absence category



###### VISUAL OF RIBES AND WPBR #######
# Assuming `pila_list_clean` includes a column for WPBR status (e.g., wpbr_present)
# Modify this based on your actual WPBR column name

# Create wpbr_present column in pila_list_clean
pila_list_clean <- pila_list_clean %>%
  mutate(wpbr_present = ifelse(
    activeBoleCanker > 0 |
      activeBranchCanker > 0 |
      inactiveBoleCanker > 0 |
      inactiveBranchCanker > 0,
    1, 0
  ))

# Create a new dataframe to combine Ribes and WPBR information
ribes_wpbr_combined <- pila_list_clean %>%
  mutate(ribes_presence = ifelse(ribes == 1, "Present", "Absent"),
         wpbr_presence = ifelse(wpbr_present == TRUE, "Present", "Absent")) %>%
  group_by(plotNum) %>%
  summarize(ribes_status = first(ribes_presence),
            wpbr_status = first(wpbr_presence), .groups = "drop")

# Create a summary table for plotting
plot_summary <- ribes_wpbr_combined %>%
  count(ribes_status, wpbr_status) %>%
  mutate(status = paste(ribes_status, wpbr_status))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#BEETLES pila_list
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# Step 1: Create `beetles_treespresent` at the tree level for DEPO presence
pila_list_clean <- pila_list_clean %>%
  mutate(
    beetles_treespresent = ifelse(
      (!is.na(pitchTubes) & pitchTubes %in% c("DEPO", "DEPO, DEVA", "DEVA, DEPO", "DEVA_DEPO")) |  # Check if pitchTubes contains specific DEPO patterns
        (!is.na(exitHoles) & exitHoles %in% c("DEPO", "DEPO, DEVA", "DEVA, DEPO", "DEVA_DEPO")),     # Check if exitHoles contains specific DEPO patterns
      1,  # Assign 1 if either column matches any of the DEPO patterns
      0   # Assign 0 otherwise
    )
  )




# Step 2: Create `beetles_present` at the plot level if any tree in the plot has `beetles_treespresent` == 1
pila_list_clean <- pila_list_clean %>%
  group_by(plotNum) %>%
  mutate(
    beetles_present = ifelse(
      any(beetles_treespresent == 1),  # Check if any tree in the plot has 'DEPO'
      1,  # Assign 1 if 'DEPO' is present in any tree within the plot
      0   # Assign 0 otherwise
    )
  ) %>%
  ungroup()  # Remove grouping after calculation

# View the updated dataframe
print(pila_list_clean)


## VISUALS ##
# Summarize data by plotNum and beetles_present
beetles_summary <- pila_list_clean %>%
  group_by(plotNum) %>%
  summarize(beetles_present_plot = ifelse(any(beetles_present == 1), 1, 0))  # 1 if beetles present in the plot

# Count how many plots have beetles and how many don't
beetles_plot_count <- beetles_summary %>%
  group_by(beetles_present_plot) %>%
  summarize(n = n())


#### SEPARATE ALIVE VS DEAD IN EACH PLOT #####

pila_list_clean <- pila_list_clean %>%
  mutate(
    alive_trees = ifelse(alive_status == "Alive", 1, 0),  # Assign 1 if tree is alive, otherwise 0
    dead_trees = ifelse(alive_status == "Dead", 1, 0)     # Assign 1 if tree is dead, otherwise 0
  )


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#FIRE pila_list
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

fire_severity <- fire_severity %>%
  mutate(fire_present = ifelse(Fire_Severity_plotAverge %in% 1:3, 1, 0))

fire_severity <- fire_severity %>%
  mutate(fire_sev = Fire_Severity_plotAverge)

# Convert fire_sev column to a categorical variable (factor)
fire_severity <- fire_severity %>%
  mutate(fire_sev = as.factor(fire_sev))

# Create a new column that assigns 1 if fire_sev is 2 or 3, otherwise 0
fire_severity <- fire_severity %>%
  mutate(high_sev = ifelse(fire_sev %in% c(3), 1, 0))


############## PILA PLOT CHARACTERISTICS DATAFRAME #######################
## Need to create a table
### ribes_plot (present in plot)
### wpbr_plot (present in plot)
### beetles_plot (present in plot)
### alive_plot (how many trees are alive in plot)
### dead_plot (how many trees are dead in plot)

pila_plotcharacteristics <- pila_list_clean %>%
  select(plotNum, treeNum, DBH_cm, ribes, wpbr_present, beetles_present, alive_trees, dead_trees)


#install.packages("gt")
library(gt)

# Create a table using gt
pila_plotcharacteristics %>%
  gt() %>%
  tab_header(
    title = "Plot Characteristics Table",
    subtitle = "Ribes, WPBR, Beetles, and Tree Status by Plot"
  ) %>%
  cols_label(
    plotNum = "Plot Number",
    ribes = "Ribes Present",
    wpbr_present = "WPBR Present",
    beetles_present = "Beetles Present",
    alive_trees = "Alive Trees",
    dead_trees = "Dead Trees"
  ) %>%
  fmt_number(columns = vars(alive_trees, dead_trees), decimals = 0) %>%
  tab_options(
    table.width = pct(80),
    table.font.size = 12
  )


##### VISUALIZE PLOT CHARACTERISTICS ######
# Create a summary dataframe to count the presence of Ribes, WPBR, and Beetles in each plot
plotcharacteristics_summary <- pila_plotcharacteristics %>%
  group_by(plotNum) %>%
  summarize(
    ribes_present = max(ribes, na.rm = TRUE),  # Assuming ribes is a binary column (0 or 1)
    wpbr_present = max(wpbr_present, na.rm = TRUE),  # Same for wpbr_present
    beetles_present = max(beetles_present, na.rm = TRUE)  # Same for beetles_present
  ) %>%
  pivot_longer(cols = c(ribes_present, wpbr_present, beetles_present),
               names_to = "Characteristic",
               values_to = "Present") %>%
  filter(Present == 1)  # Only keep rows where the characteristic is present


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# AREA CODE ############
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# Load required libraries
library(sf)
library(ggplot2)
library(sp)
library(readr)
library(dplyr)
library(tmap)  # For interactive mapping

# Step 1: Load Data and Ensure Valid Format
waypoints <- read_csv("pila_datum.csv") %>%
  mutate(
    plot_beg_UTM_E = as.numeric(plot_beg_UTM_E),
    plot_beg_UTM_N = as.numeric(plot_beg_UTM_N),
    zone = as.numeric(zone)
  ) %>%
  drop_na(plot_beg_UTM_E, plot_beg_UTM_N, zone)  # Remove rows with NA values

# Step 2: Convert UTM to Lat/Lon
convert_utm_to_latlon <- function(easting, northing, zone) {
  proj_string <- paste0("+proj=utm +zone=", zone, " +datum=WGS84")
  coords <- SpatialPoints(cbind(easting, northing), proj4string = CRS(proj_string))
  latlon <- spTransform(coords, CRS("+proj=longlat +datum=WGS84"))
  return(data.frame(lat = latlon@coords[, 2], lon = latlon@coords[, 1]))
}

latlon_coords <- do.call(rbind, mapply(function(easting, northing, zone) {
  tryCatch({
    convert_utm_to_latlon(easting, northing, zone)
  }, error = function(e) {
    return(data.frame(lat = NA, lon = NA))  # Handle conversion errors
  })
}, waypoints$plot_beg_UTM_E, waypoints$plot_beg_UTM_N, waypoints$zone, SIMPLIFY = FALSE))

waypoints <- cbind(waypoints, latlon_coords) %>%
  drop_na(lat, lon)  # Remove rows with invalid lat/lon

# Step 3: Create sf Object and Reproject CRS
waypoints_sf <- st_as_sf(waypoints, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 32611)  # Change CRS to your specific UTM zone

# Step 4: Calculate Convex Hull and Area for Each Plot
plot_areas <- waypoints_sf %>%
  group_by(plotID) %>%
  summarise(
    convex_hull = st_convex_hull(st_union(geometry)),
    area_sq_m = st_area(convex_hull)
  ) %>%
  mutate(area_sq_km = as.numeric(area_sq_m) / 1e6) %>%  # Convert to square kilometers
  st_set_geometry(NULL)  # Remove geometry column for merging

# Step 5: Join Areas Back to Original Data
waypoints_with_area <- waypoints %>%
  left_join(plot_areas, by = "plotID")

# Step 6: Create Interactive Map with tmap
waypoints_sf_with_area <- st_as_sf(waypoints_with_area, coords = c("lon", "lat"), crs = 4326)

tmap_mode("view")  # Set tmap to interactive mode

# Create a map showing all waypoints
tm_shape(waypoints_sf_with_area) +
  tm_dots(col = "plotID", palette = "Set1", title = "Plot ID") +
  tm_layout(legend.position = c("left", "bottom"))

# Step 7: Visualize Results with ggplot
ggplot() +
  geom_sf(data = waypoints_sf_with_area, aes(color = factor(plotID))) +
  geom_sf(data = st_as_sf(plot_areas, crs = 4326), aes(fill = area_sq_km), color = "blue", alpha = 0.2) +
  ggtitle("Waypoints with Convex Hull by PlotID") +
  theme(legend.position = "right")

# Optional: Save the Updated Data
write_csv(waypoints_with_area, "pila_datum_with_area.csv")

######### MORE ################
#issue with calculating areas and projections of plots in wrong spots

# Read the CSV file containing the waypoints data
waypoints <- read.csv("pila_datum.csv")

# Convert the UTM columns to numeric (this will introduce NAs for any non-numeric values)
waypoints$PILA_UTM_E <- as.numeric(waypoints$PILA_UTM_E)
waypoints$PILA_UTM_N <- as.numeric(waypoints$PILA_UTM_N)

# Remove rows with missing or invalid UTM coordinates
waypoints_clean <- waypoints %>%
  filter(!is.na(PILA_UTM_E) & !is.na(PILA_UTM_N))

# Convert the cleaned waypoints to an sf object (assuming UTM zone 11N, EPSG: 32611)
waypoints_sf <- st_as_sf(waypoints_clean, coords = c("PILA_UTM_E", "PILA_UTM_N"), crs = 32611)

# Group by plotID, calculate convex hull, and compute area in square meters
plot_areas <- waypoints_sf %>%
  group_by(plotID) %>%
  summarise(
    convex_hull = st_convex_hull(st_union(geometry)),  # Calculate the convex hull of the plot
    area_sq_m = st_area(convex_hull)  # Calculate the area of the convex hull in square meters
  )

# Print the area for each plot
print(plot_areas)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#LENGTH WIDTH CODE #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


########################################
# Load required libraries
library(sf)
library(dplyr)

# Ensure waypoints_clean is an sf object with the correct CRS
waypoints_clean_sf <- st_as_sf(waypoints_clean, coords = c("PILA_UTM_E", "PILA_UTM_N"), crs = 32611)

# Step 1: Define the starting waypoint (waypoint_beg) for each plot
waypoints_clean_sf <- waypoints_clean_sf %>%
  group_by(plotID) %>%
  mutate(
    waypoint_beg = st_sfc(st_point(c(first(plot_beg_UTM_E), first(plot_beg_UTM_N))), crs = 32611)
  ) %>%
  ungroup()

# Step 2: Function to calculate the maximum distance within a plotID
calculate_distances_within_plot <- function(geometry, waypoint_beg) {
  # Calculate distances from the starting waypoint to all other waypoints in the same plot
  dist_from_beg <- st_distance(waypoint_beg, geometry)  # Distance from the starting waypoint
  furthest_distance <- max(dist_from_beg)  # Maximum distance from waypoint_beg within the same plot
  return(furthest_distance)
}

# Step 3: Group by plotID and calculate length, width, and area for each plot
plot_areas <- waypoints_clean_sf %>%
  group_by(plotID) %>%
  summarise(
    waypoint_beg = first(waypoint_beg),  # Store the starting waypoint
    length_m = calculate_distances_within_plot(geometry, first(waypoint_beg)),  # Maximum distance for length within plot
    # Calculate the maximum width between the two furthest waypoints within the same plot
    width_m = max(as.numeric(st_distance(geometry)), na.rm = TRUE),  # Maximum distance between waypoints within plot
    area_sq_m = length_m * width_m,  # Compute area as length * width
    area_sq_km = area_sq_m / 1e6  # Convert to square kilometers
  ) %>%
  ungroup()  # Ungroup after summarization

# View results
print(plot_areas)


#### Manually change some widths based on what the notes say (dSide)

# plot 9 width should be 100
# plot 31 width should be 100
# plot 37 width (only one tree and the notes say it is too old to assess, did we eliminate this plot???)
# plot 68 width should be 105.2


# Convert width_m to numeric if it's not
plot_areas$width_m <- as.numeric(plot_areas$width_m)

# Then run the mutate
plot_areas <- plot_areas %>%
  mutate(width_m = case_when(
    plotID == 9 ~ 100,
    plotID == 31 ~ 100,
    plotID == 68 ~ 105.2,
    TRUE ~ width_m
  ))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# CUSTOM CODE FOR PLOT 48 USING ONLY TREE WAYPOINTS (EXCLUDING treeNum 1) #####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Filter tree waypoints for plotID 48, excluding treeNum 1
plot_48_tree_waypoints <- waypoints_clean_sf %>%
  filter(plotID == 48 & !is.na(PILA_waypoint) & treeNum != 1)  # Exclude treeNum 1

# Recalculate length and width for plotID 48 using only tree waypoints (excluding treeNum 1)
plot_48_custom <- plot_48_tree_waypoints %>%
  summarise(
    plotID = first(plotID),  # Keep plotID
    length_m = calculate_distances_within_plot(geometry, first(geometry)),  # Recalculate length using only tree waypoints
    width_m = max(as.numeric(st_distance(geometry)), na.rm = TRUE),  # Recalculate width using only tree waypoints
    area_sq_m = length_m * width_m,  # Compute new area as length * width
    area_sq_km = area_sq_m / 1e6  # Convert to square kilometers
  )


#####$$$$$$$$$$$$$$$$$$$$######################
# Now I need to bring this data back in with the rest of the pila data
# plotID is the old plotID so match them that way

# Join length_m and width_m from plot_areas to pila_list_clean based on plotID
pila_list_clean <- pila_list_clean %>%
  left_join(plot_areas %>% select(plotID, length_m, width_m), by = "plotID")




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
##### Climate Data Analysis #########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


pilalist_climate <- left_join(pila_list_clean, climatedata, by = c("plotID" = "PlotID"))

# Drop specific columns from the dataframe
pilalist_climate <- pilalist_climate %>%
  select(-plotNum.y)

pilalist_climate <- pilalist_climate %>%
  rename(plotNum = plotNum.x)

####
# Aggregate pilaWPBR data by plotNum, taking the mean of incidencepercent
pilaWPBR_aggregated <- pilaWPBRclean %>%
  group_by(plotNum) %>%
  summarise(incidencepercent = mean(incidencepercent, na.rm = TRUE))

# Perform left join
pilalist_climate <- left_join(pilalist_climate, pilaWPBR_aggregated, by = "plotNum")




### The combined graph is not showing
#install.packages("cowplot")
library(cowplot)


# Calculate linear regression model
lm_modelclimate <- lm(incidencepercent ~ vpdmax, data = pilalist_climate)


#install.packages("ggpubr")
library(ggpubr)
## Create VPD plot ##
VPDscatter_plot <- ggscatter(pilalist_climate, x = "vpdmax", y = "incidencepercent",
                             color = "black", shape = 20, size = 3,
                             add = "reg.line",
                             add.params = list(color = "blue", fill = "lightgray"),
                             conf.int = TRUE,
                             cor.coef = TRUE,
                             cor.coeff.args = list(method = "pearson", label.x = 18, label.sep = "\n")) +
  labs(x = "VPD Max", y = "WPBR Incidence (% infected)") +
  coord_cartesian(xlim = c(12, 20))

VPDscatter_plot

TEMPscatter_plot <- ggscatter(pilalist_climate, x = "tmean", y = "incidencepercent",
                              color = "black", shape = 20, size = 3,
                              add = "reg.line",
                              add.params = list(color = "blue", fill = "lightgray"),
                              conf.int = TRUE,
                              cor.coef = TRUE,
                              cor.coeff.args = list(method = "pearson", label.x = 13, label.sep = "\n")) +
  labs(x = "Mean Temp", y = "WPBR Incidence (% infected)") +
  coord_cartesian(xlim = c(5, 15))

TEMPscatter_plot

PRECIPscatter_plot <- ggscatter(pilalist_climate, x = "ppt", y = "incidencepercent",
                                color = "black", shape = 20, size = 3,
                                add = "reg.line",
                                add.params = list(color = "blue", fill = "lightgray"),
                                conf.int = TRUE,
                                cor.coef = TRUE,
                                cor.coeff.args = list(method = "pearson", label.x = 1300, label.sep = "\n")) +
  labs(x = "Precipitation", y = "WPBR Incidence (% infected)") +
  coord_cartesian(xlim = c(950, 1450))

PRECIPscatter_plot

ELEVscatter_plot <- ggscatter(pilalist_climate, x = "plot_elevation_ft", y = "incidencepercent",
                              color = "black", shape = 20, size = 3,
                              add = "reg.line",
                              add.params = list(color = "blue", fill = "lightgray"),
                              conf.int = TRUE,
                              cor.coef = TRUE,
                              cor.coeff.args = list(method = "pearson", label.x = 5500, label.sep = "\n")) +
  labs(x = "Elevation", y = "WPBR Incidence (% infected)") +
  coord_cartesian(xlim = c(3200, 8000))

ELEVscatter_plot

combinedscatteredclimategraphs <- VPDscatter_plot + TEMPscatter_plot + PRECIPscatter_plot + ELEVscatter_plot
combinedscatteredclimategraphs

###### Dataframe for the model #####
# Select only the columns you want from pilaWPBRclean
pilaWPBR_subset <- pilaWPBRclean %>%
  select(plotNum, treeNum, severity, infectionrate)

# Perform the join using both columns
pilalist_climate_wpbr <- pilalist_climate %>%
  left_join(pilaWPBR_subset, by = c("plotNum", "treeNum"))

### Bring in the rest of the pila data
### pilalist_climate_wpbr and plot_areas

pilalist_climate_wpbr <- pilalist_climate_wpbr %>%
  left_join(fire_severity %>% select(plotNum, fire_present, Fire_Severity_plotAverge, fire_sev, high_sev),
            by = "plotNum")

###################################################
######## DENSITY ANALYSIS ###################
#################################################

### PILA AREA

# Calculate area for standard plots using tans_length and width
# NA values should be 40

# Create the standard_area column with a condition for NA in width
pilalist_climate_wpbr$standard_area <- pilalist_climate_wpbr$trans_length *
  ifelse(is.na(pilalist_climate_wpbr$width), 40, pilalist_climate_wpbr$width)


# Create the balloon_area column by multiplying length_m and width_m
pilalist_climate_wpbr$balloon_area <- pilalist_climate_wpbr$length_m * pilalist_climate_wpbr$width_m

# Create the true_area column based on balloon_area if it exists, otherwise use standard_area
pilalist_climate_wpbr$pila_area <- ifelse(!is.na(pilalist_climate_wpbr$balloon_area),
                                          pilalist_climate_wpbr$balloon_area,
                                          pilalist_climate_wpbr$standard_area)

### PILA DENSITY
# Calculate the number of alive trees per plot and add it to the original dataframe
pilalist_climate_wpbr <- pilalist_climate_wpbr %>%
  group_by(plotNum) %>%  # Group by plot
  mutate(pilaalive_plot = sum(alive_trees, na.rm = TRUE)) %>%  # Sum alive trees per plot
  ungroup()  # Ungroup after the operation

#Calculate dead
pilalist_climate_wpbr <- pilalist_climate_wpbr %>%
  group_by(plotNum) %>%  # Group by plot
  mutate(piladead_plot = sum(dead_trees, na.rm = TRUE)) %>%  # Sum alive trees per plot
  ungroup()  # Ungroup after the operation

# Calculate the total number of trees (alive + dead) per plot
pilalist_climate_wpbr <- pilalist_climate_wpbr %>%
  mutate(total_pila = pilaalive_plot + piladead_plot)  # Sum alive and dead trees

# Calculate the density of alive trees per area
pilalist_climate_wpbr <- pilalist_climate_wpbr %>%
  mutate(pilaalive_density = pilaalive_plot / pila_area)  # Divide alive trees by area

# Calculate the density of alive trees per area
pilalist_climate_wpbr <- pilalist_climate_wpbr %>%
  mutate(pilatotal_density = total_pila / pila_area)  # Divide alive trees by area

##############################################
#$$$$$$$$$ ASSOC AREA $$$$$$$$$$$$$$$$$@@@@@@@@@@@

# Create a new column 'length' and assign the value 50 to all rows
assoc_list$length <- 50

# Convert dSideL_m to numeric if it's not already
assoc_list$dSideL_m <- as.numeric(as.character(assoc_list$dSideL_m))

# Create a new column dSideLp_m with positive values from dSideL_m
assoc_list$dSideLp_m <- abs(assoc_list$dSideL_m)

# First, ensure that dSideR_m and dSideLp_m are numeric
assoc_list$dSideR_m <- as.numeric(as.character(assoc_list$dSideR_m))
assoc_list$dSideLp_m <- as.numeric(as.character(assoc_list$dSideLp_m))

# Calculate max_side based on the largest number in each plot
assoc_list <- assoc_list %>%
  group_by(plotNum) %>%
  mutate(max_side = max(c(dSideR_m, dSideLp_m), na.rm = TRUE)) %>% # Find the max across dSideR_m and dSideLp_m for each plot
  mutate(max_side = case_when(
    max_side > 15 ~ 25,  # Assign 25 if max_side is greater than 15
    TRUE ~ max_side      # Otherwise, keep the original max_side value
  )) %>%
  mutate(width = case_when(
    max_side <= 5 ~ 5,       # Assign 5 if max_side is <= 5
    max_side <= 10 ~ 10,     # Assign 10 if max_side is > 5 and <= 10
    max_side <= 15 ~ 15,     # Assign 15 if max_side is > 10 and <= 15
    max_side == 25 ~ 25      # Assign 25 if max_side is 25
  )) %>%
  ungroup()  # Ungroup after the operation


# Create a new area column by multiplying length and width
assoc_list <- assoc_list %>%
  mutate(assoc_area = length * (width * 2))  # Multiply length and width to get area



# Aggregate assoc_area to get one value per plotNum
assoc_area_summary <- assoc_list %>%
  group_by(plotNum) %>%
  summarize(assoc_area = max(assoc_area, na.rm = TRUE))  # Change mean to sum, max, etc. if needed

# Join assoc_area to pilalist_climate_wpbr using plotNum
pilalist_climate_wpbr <- pilalist_climate_wpbr %>%
  left_join(assoc_area_summary, by = "plotNum")


pilalist_climate_wpbr <- pilalist_climate_wpbr %>%
  group_by(plotNum) %>%
  mutate(wpbr_presentplot = ifelse(any(wpbr_present == 1), 1, 0)) %>%
  ungroup()

num_wpbr_plots <- pilalist_climate_wpbr %>%
  group_by(plotNum) %>%
  summarize(wpbr_present_in_plot = max(wpbr_presentplot)) %>%
  filter(wpbr_present_in_plot == 1) %>%
  nrow()

num_wpbr_plots

# If your data is an sf object
library(sf)

# Convert POINT column to WKT
pilalist_climate_wpbr$waypoint_beg <- st_as_text(pilalist_climate_wpbr$waypoint_beg)

# Write to CSV
write.csv(pilalist_climate_wpbr, "pila_report_df.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")


