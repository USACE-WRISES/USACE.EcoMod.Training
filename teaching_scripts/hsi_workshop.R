# Install packages; only do this if you have not done this before or if they need to be updated
#install.packages("ecorest")
#install.packages("tidyverse")
#install.packages("knitr")
#install.packages("knitr")
#install.packages("kableExtra")

# Load R packages; do this for each R Session
library(ecorest)
library(tidyverse)
library(knitr)
library(kableExtra)

# HSI modeling ####

## Blue Book HSI Model ####

# Load the HSI model data into the R session 
data(HSImodels)

# Check which HSI models are available
names(HSImodels)

# Load metadata associated with each HSI model data into the R session 
data(HSImetadata)

# Print a subset of columns and rows
HSImetadata %>%
  select(model, submodel, Eqtn) %>% # Select which columns to display
  slice(1,4:6) # Select which rows to display

# Subset the `HSImodels` object to just the marsh wren model
wren_hsi <- HSImodels$marshwren

# Print the wren_hsi model to the console
print(wren_hsi)

# Save an image of the HSI model in the output folder
# User picks name of image, but must keep the .jpg file type
HSIplotter(wren_hsi, "output/marshwren_hsi.jpg")

# Create some environmental variables to generate HSI scores for
# These must be in the order of the HSI model variables
env_vars <- data.frame(hydrophytes = "a",
                   herb_cover = 55,
                   water_depth = 10,
                   canopy_cover = 0)

# Create a vector representing the size of a restoration feature
area = 34

# Use the SI calc function to view the model suitability
si_vars <- SIcalc(wren_hsi, unlist(env_vars))
print(si_vars)

# Built-in equations to calculate overall HSI score
HSIarimean(si_vars)
HSIgeomean(si_vars)
HSImin(si_vars)
HSIwarimean(si_vars, w = c(.2, .2, .2, .4)) #weighted arithmetic mean; weights must sum to 1

# View the model-specific equation for calculating equation
filter(HSImetadata, model == "marshwren") %>% select(Eqtn) %>% pull()
filter(HSImetadata, model == "marshwren") %>% select(CCR) %>% pull()

# Calculate the total HSI score
# Model name must match an entry from HSImodels object
total_HSI <- HSIeqtn("marshwren", si_vars, HSImetadata)
print(total_HSI)

# Calculate habitat units
total_hab <- total_HSI*area
print(total_hab)

# Create dataframe of environmental variables for 4 alternatives
# Important that variables are in the same order as the model
new_env_vars <- data.frame(hydrophytes = rep("a", 4),
                       herb_cover = c(5, 8.5, 12, 16),
                       water_depth = c(94.5, 89.9, 85.3, 80.8),
                       woody_cover = c(0, 0, 0, 0))

n_alts <- nrow(new_env_vars) # Number of years/alternatives
n_vars <- ncol(new_env_vars) # Number of environmental variables

# Area associated with each alternative
new_area = c(68, 102, 136, 170)

# Create an empty matrix and vector to hold the results of the SI and HSI calculations, respectively
new_si_vars <- matrix(0, nrow=n_alts, ncol=n_vars)
new_total_HSI <- rep(NA, 4)

# Use a for-loop to calculate SI values and HSI scores for each year/scenario
for (i in 1:n_alts) {
  new_si_vars[i, ] <- SIcalc(wren_hsi, unlist(new_env_vars[i, ]))
  new_total_HSI[i] <- HSIeqtn("marshwren", new_si_vars[i, ], HSImetadata)
}

new_si_vars
new_total_HSI

new_names <- paste0("SI_", names(new_env_vars))

# Create a dataframe of the new SI values
new_si_df <- as.data.frame(new_si_vars) %>%
  set_names(c(new_names))

# Create an output table with all of the input and output data
output_wren <- new_env_vars %>%
  bind_cols(new_si_df) %>%
  mutate(output_HSI = round(new_total_HSI, 2),
         total_acres = new_area,
         HUs = output_HSI*total_acres)

write_csv(output_wren, "output/marsh_wren_output.csv")

# Print the table using the knitr package
knitr::kable(output_tab_wren, 
             digits = 2, 
             padding = 1,
             caption = "Table 1. Habitat Suitability Index (HSI) Variables and 
             Resulting Outputs for the Marsh Wren Model With-Project.") %>%
  kableExtra::kable_styling()

## Custom HSI Model ####

# Import custom marsh HSI model from San Francisco Bay Shoreline Study
marsh_model<-read.csv("data/SF-TM-HEI_InputData.csv")
names(marsh_model)
HSIplotter(marsh_model,"output/marsh_model.jpg")

# Import dataset of environmental conditions associated with project alternatives
marsh_alts <- read.csv("data/SF-TM-HEI_FieldData.csv")

print(marsh_alts)

# Create a dataframe of just environmental variables associated with each alternative
env_marsh <- select(marsh_alts, -alt)

# Number of alternatives
n_alts <- nrow(marsh_alts)

# Create empty matrix to store SI values for each alternative
si_marsh <- matrix(NA, nrow=n_alts, ncol= 4)

# Calculate the SI values for each alternative using a for-loop
for(i in 1:n_alts) {
  si_marsh[i, ] <- SIcalc(marsh_model, env_marsh[i, ])
}

# Convert matrix of SI values to a data frame and name the columns
si_marsh_df <- as.data.frame(si_marsh) %>%
  set_names(paste0("SI_", names(env_marsh)))

# Calculate overall HSI from individual SI values
# Uses a custom HSI equation (Quasi geometric mean)
si_total <- si_marsh_df %>%
  mutate(total_hsi = ((((SI_V4+SI_V3)/2)*(SI_V2*SI_V1))^(1/3)))

# Calculate habitat units by multiplying total HSI by project area (300 acres)
area_marsh <- 300

# Create new column with habitat units
si_total_hu <- si_total %>%
  mutate(HUs = area_marsh*total_hsi)

output_tab <- bind_cols(marsh_alts, si_total_hu) %>%
  set_names(c("Alternative", "PTR", "MA", "HTR", "E", "PTR-SI", "MA-SI", 
              "HTR-SI", "E-SI", "HSI-Total", "Marsh Units"))

knitr::kable(output_tab, caption="Table 2. SF Salt Marsh HSI Values and Marsh Units.", 
             align="c", 
             digits = 2) %>% 
  kableExtra::kable_styling()

# Same code using pipes
output_tab_2 <- as_tibble(si_marsh) %>%
  set_names(c(paste0("SI_", names(env_marsh)))) %>%
  mutate(total_hsi = ((((SI_V4+SI_V3)/2)*(SI_V2*SI_V1))^(1/3)),
         HUs = area_marsh*total_hsi) %>%
  bind_cols(marsh_alts,.) %>% # This code is slightly different so that marsh_alts data will come first; the "." symbol indicates everything that has come before this function
  set_names(c("Alternative", "PTR", "MA", "HTR", "E", "PTR-SI", "MA-SI", 
              "HTR-SI", "E-SI", "HSI-Total", "Marsh Units"))

all.equal(output_tab, output_tab_2)

# Decision support ####

## Annualizing benefits ####

# User-specified time intervals
ben_df <- data.frame(Year = c(0,2,20,50),
                     Benefits = c(0,40,35,30))

# Quick plot
ggplot(ben_df, aes(Year, Benefits))+
  geom_point(size = 2)+
  geom_line()

# Annualized benefits across 50 years
annualizer(ben_df$Year, ben_df$Benefits)

## CEICA ####

# Read in cost-benefit data
bi_cost_bens <- read_csv("data/beaver_island_cost_ben.csv") 

# Extract benefits and cost data
benefits <- bi_cost_bens$RestBen
costs <- bi_cost_bens$AnnCost

# Calculate cost effective plans
# Cost effective = Plans at lowest cost for a given benefit
restCE <- CEfinder(benefits, costs)

# Determine which options are `best buys`
# Best buys identified by minimizing incremental costs ($/unit)
BB_list <- BBfinder(benefits, costs, restCE)
BB_list

# Create a vector of the `best buy` status of each alternative
bb_vector <- BB_list[[1]][,4]

#Create a table of the results
ceica_table <- bi_cost_bens %>% 
  mutate(
    "Cost Effectiveness" = restCE,
    "Best Buy" = bb_vector) 

write_csv(ceica_table, "output/ceica_table.csv")

# Create a plot of CEICA results
CEICAplotter(bi_cost_bens$AltID, benefits, costs, restCE, bb_vector, "output/CEICAexample.jpeg")

