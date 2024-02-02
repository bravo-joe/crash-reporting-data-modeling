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

###################################################
# 2. Prep recipe
nb_recipe <-
  recipe(injury_severity  ~ ., data = train_split) %>%
  step_upsample(injury_severity, over_ratio = 0.75)
nb_recipe

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
  add_model(nb_model) %>%
  add_recipe(nb_recipe)
nb_wflow

# 5. Fit the training data
nb_fit <- fit(nb_wflow, train_split)
nb_fit

nb_fit %>% extract_fit_engine()

# 6. Predict from workflow object on new data
nb_pred <- predict(nb_fit, test_split, type = "class")
nb_pred

# 7. Create a new table with actual and predicted values
results_tbl <- bind_cols(actual = test_split$injury_severity, predicted = nb_pred$.pred_class)
results_tbl

# 8. Output result table to csv file
write.csv(results_tbl, config$results_tbl, row.names=FALSE)

# Cleanup
#unlink("data_folder", recursive = TRUE)

# End of crash_reporting_nb_model_v0_1_0.R
