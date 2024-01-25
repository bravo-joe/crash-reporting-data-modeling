# Beginning of crash_reporting_nb_model_v0_1_0.R
library(tidyverse)
library(tidymodels)
tidymodels_prefer()
library(themis)
library(doParallel) # All operating systems
library(discrim)
library(data.table)
library(janitor)
set.seed(525)  # Setting seed
## Parallel Processing Configuration
# Cluster
all_cores <- parallel::detectCores(logical = FALSE)
cluster <- makePSOCKcluster(all_cores)

# Read the configuration file
config <- config::get()

# Check if necessary directory exists and unzip data
if (file.exists(config$output_dir)){
  unzip(config$dataset, exdir = config$output_dir)
} else {
  dir.create(file.path(".", config$output_dir), showWarnings = TRUE)
  unzip(config$dataset, exdir = config$output_dir)
}

# Load data into variable
raw_data <- fread(config$data_loc)

# Preprocessed dataframe
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

# 1. Create splits, folds, performance metrics, and control object
splits <- initial_split(subset_df, prop = 2/3, strata = injury_severity)
train_split <- training(splits)
test_split <- testing(splits)

# 2. Prep recipe
nb_rec <-
  recipe(injury_severity  ~ ., data = train_split) %>%
  step_upsample(injury_severity, over_ratio = 1.0) %>%
  step_dummy(weather, light, vehicle_body_type)
nb_rec

# 3. Specify model
# Naive Bayes Model Specifications
nb_model <- naive_Bayes(
  mode = "classification",
  smoothness = config$smoothness,
  Laplace = config$laplace,
  engine = "naivebayes"
)
nb_model

# 4. Set workflow (Preprocess & model)
nb_wflow <-
  workflow() %>%
  add_model(naive_bayes_model) %>%
  add_recipe(nb_rec)
nb_wflow

# 5. An alternative approach using prep()
crash_rec_trained <- prep(nb_rec)
crash_rec_trained

show_variables <-
  nb_rec %>%
  prep(log_changes = TRUE)

# 2b. Prep recipe
nb_rec <-
  train_split %>%
  recipe(injury_severity  ~ .) %>%
  step_upsample(injury_severity, over_ratio = 1.0) %>%
  step_dummy(weather, light, vehicle_body_type)
nb_rec

# 3b. Apply transformation to the data
crash_data_transformed <-
  prep(nb_rec) %>%
  bake(new_data = train_split)
crash_data_transformed

# 4b. Apply bake() function
crash_val_processed <- bake(crash_rec_trained, new_data = test_split)
crash_val_processed

#crash_validation <- test_split$splits %>% pluck(1) %>% assessment()
#crash_validation

p2 <-
  crash_val_processed %>%
  ggplot(aes(x = injury_severity)) +
  geom_bar() +
  ggtitle("Processed validation set data")
p2
  




# Cleanup
unlink("data_folder", recursive = TRUE)

# End of crash_reporting_nb_model_v0_1_0.R
