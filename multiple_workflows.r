# Beginning of "multiple_workflows.r"
# Import necessary libraries
library(dplyr) # for functions
library(data.table)
#install.packages("tidymodels")
library(tidymodels)
tidymodels_prefer()
require(caTools)
library(parsnip)
set.seed(120)  # Setting seed

# Import the dataset
raw_data <- fread("/Volumes/my_passport/data_sets/WA_Crash_Summary.csv")
processed_df <- raw_data %>%
    select(
        "City",
        "Weather Condition",
        "Lighting Condition",
        "Injury Severity"
    ) %>%
    filter(
        "Weather Condition" != ""
    ) %>%
    filter(
        "Lighting Condition" != ""
    )

sample <- sample.split(processed_df$"Injury Severity", SplitRatio = 0.80)
train_df <- subset(processed_df, sample == TRUE)
test_df  <- subset(processed_df, sample == FALSE)

# Initiate the random forest model
rf_model_1 <-
    rand_forest(trees = 500) %>%
    set_engine("ranger") %>%
    set_mode("classification")
#rf_model_1

# Setup a workflow
rf_wflow_1 <-
    workflow() %>%
    add_model(rf_model_1)

# Now, add a formula as a preprocessor
rf_wflow_1 <-
    rf_wflow_1 %>%
    add_formula(
        "Injury Severity" ~
        "Weather Condition" +
        "Lighting Condition" +
        "City"
    )





# End of "multiple_workflows.r"
