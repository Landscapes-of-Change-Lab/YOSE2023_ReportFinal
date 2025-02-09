pila_report <- read.csv("pila_report_df.csv")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# MODELING  #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

library("lme4")
#install.packages("lme4", type = "source")

librarian::shelf(lme4, tidyverse, terra, sjPlot, Matrix)



# Model wpbr_present in the full dataset
mod_wpbr_pilareport <- lmer(wpbr_present ~ vpdmax + ribes + beetles_present + pilaalive_density + factor(fire_sev) + DBH_cm + (1 | plotNum),
               data = pila_report)
tab_model(mod_wpbr_pilareport)


#######################################################
####### Models with only live trees ########
#######################################################

#######################################
# Create a new dataframe keeping only rows where high_sev is 0 (no high severity fire)
pilareport_live <- subset(pila_report, alive_status == "Alive")



# Replace NA values in wpbr_present with 0
pilareport_live$wpbr_present[is.na(pilareport_live$wpbr_present)] <- 0
# Model wpbr_present using fire_sev in the full dataset
mod_wpbr_pilareportlive <- lmer(wpbr_present ~ vpdmax + ribes + beetles_present + pilaalive_density + factor(fire_sev) + DBH_cm + (1 | plotNum),
             data = pilareport_live)
tab_model(mod_wpbr_pilareportlive)



# Beetle model using the complete dataset
modbeetles <- lmer(beetles_present ~ vpdmax + pilaalive_density + factor(fire_sev) + slope + DBH_cm + (1 | plotNum),
                   data = pilareport_live)
tab_model(modbeetles)



