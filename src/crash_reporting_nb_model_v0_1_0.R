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
  
# Apply model recipe to new data
test_data_transformed <-
  prep(nb_rec) %>%
  bake(new_data = test_split)

# Store predictions/results in its own variable
result <-
  predict(
    object = extract_fit_parsnip(
      nb_model
    ),
    new_data = test_data_transformed,
    type = "class"
  )

###################################################
# Will attempt different approach to modeling
# Create 2 custom functions
###################################################
# Create prediction tables
predict_table <- function(model, data, tidy_flag){
  if (tidy_flag == TRUE) {
    result <- model %>%
      predict(data) %>%
      rename(pred = .pred) %>%
      mutate(
        actual = data$total,
        pred_real = pred^3,
        actual_real = actual^3
      )
  } else {
    result <- model %>%
      predict(data) %>%
      as_tibble_col(column_name = "pred") %>%
      mutate(
        actual = data$total,
        pred_real = pred^3,
        actual_real = actual^3
      )
  }
  result
}

nb_model <- naive_Bayes(
  mode = "classification",
  smoothness = tune(),
  Laplace = tune(),
  engine = "naivebayes"
)

nb_wflow <-
  workflow() %>%
  add_model(naive_bayes_model) %>%
  add_recipe(nb_rec)

# Folds
severity_folds <- vfold_cv(train_split, v = 10, strata = injury_severity)
# Performance metrics - classification model
perf_metrics <- metric_set(roc_auc, recall, precision, f_meas)
# Control Object
ctrl_obj <- control_grid(verbose = FALSE, save_pred = TRUE)

nb_tune <- nb_wflow %>%
  tune_grid(
    resamples = severity_folds,
    metrics = perf_metrics,
    control = ctrl_obj
  )

# Fit with best parameters
nb_best <-
  finalize_workflow(nb_wflow, select_best(nb_tune)) %>%
  fit(train_split)




# Cleanup
unlink("data_folder", recursive = TRUE)

# End of crash_reporting_nb_model_v0_1_0.R
