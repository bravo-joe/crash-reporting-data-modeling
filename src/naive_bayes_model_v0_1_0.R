# Beginning of naive_bayes_model_v0_1_0.R
#library(dplyr) # for functions
#library(data.table)
#library(janitor)
library(tidyverse)
library(tidymodels)
tidymodels_prefer()
library(themis)
library(doParallel) # All operating systems
library(discrim)
set.seed(525)  # Setting seed

# Unzip data file location and output directory
#zip_file <- 
#out_dir <- 
# Read the configuration file
config <- config::get()

# Check if subdirectory exists
if (file.exists(config$output_dir)){
  unzip(config$dataset, exdir = config$output_dir)
} else {
  dir.create(file.path(".", config$output_dir), showWarnings = TRUE)
  unzip(config$dataset, exdir = config$output_dir)
}

# Load data into variable
raw_data <- fread("data_folder/mont_cty_crash_reporting_incidents_data.csv")

# Preprocessed Dataframe
preproc_df <- raw_data %>%
  select(
    "Weather",
    "Light",
    "Vehicle Body Type",
    "Speed Limit",
    "Injury Severity"
  ) %>%
  filter(
    # Weather
    raw_data$"Weather" != "OTHER",
    raw_data$"Weather" != "N/A",
    raw_data$"Weather" != "UNKNOWN",
    # Light
    raw_data$"Light" != "N/A",
    raw_data$"Light" != "OTHER",
    raw_data$"Light" != "UNKNOWN",
    # Vehicle Body Type
    raw_data$"Vehicle Body Type" != "N/A",
    raw_data$"Vehicle Body Type" != "OTHER",
    raw_data$"Vehicle Body Type" != "UNKNOWN",
    raw_data$"Vehicle Body Type" != ""
  ) %>%
  # Converting all the character data types to factors
  mutate_if(is.character, as.factor) %>%
  # Convert the only integer, i.e. speed limit, to factor
  # Will treat speed limit as a categorical variable
  mutate_if(is.integer, as.factor) %>%
  # Change the column names to snake and lower case
  clean_names()

# Want a 2.5% subset of data for testing
num_records <- nrow(preproc_df)
sample_size <- ceiling(num_records * 0.025)
subset_df <- preproc_df[sample(1:num_records, sample_size, replace = FALSE),]

head(subset_df)

## Create splits, folds, performance metrics, and control object
# Splits
splits <- initial_split(subset_df, prop = 2/3, strata = injury_severity)
train_split <- training(splits)
test_split <- testing(splits)

## Parallel Processing Configuration
# Cluster
all_cores <- parallel::detectCores(logical = FALSE)
cluster <- makePSOCKcluster(all_cores)

# Recipe
nb_rec <-
  recipe(injury_severity  ~ ., data = train_split) %>%
  step_upsample(injury_severity, over_ratio = 1.0) %>%
  step_dummy(weather, light, vehicle_body_type)
nb_rec

# Naive Bayes Model Specifications
naive_bayes_model <- naive_Bayes(
  mode = "classification",
  smoothness = tune(),
  Laplace = tune(),
  engine = "naivebayes"
)
naive_bayes_model


# Cleanup
unlink("data_folder", recursive = TRUE)

# End of naive_bayes_model_v0_1_0.R