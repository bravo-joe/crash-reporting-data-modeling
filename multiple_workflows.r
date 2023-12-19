# Beginning of "multiple_workflows.r"
# Import necessary libraries
library(dplyr) # for functions
library(data.table)
#install.packages("tidymodels")
library(tidyverse)
library(tidymodels)
tidymodels_prefer()
require(caTools)
library(parsnip)
#library(ranger)
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
# Change the column names
colnames(processed_df) <- c(
    "city",
    "weather_condition",
    "lighting_condition",
    "injury_severity"
    )
sample <- sample.split(processed_df$"injury_severity", SplitRatio = 0.80)
train_df <- subset(processed_df, sample == TRUE)
test_df  <- subset(processed_df, sample == FALSE)
# Turn injury_severity into a factor
test_df$injury_severity <- as.factor(test_df$injury_severity)
train_df$injury_severity <- as.factor(train_df$injury_severity)

# Initiate the random forest model
rf_model_1 <-
    rand_forest(trees = 1000) %>%
    #set_engine("ranger") %>%
    set_engine("randomForest") %>%
    set_mode("classification")
rf_model_1

# Setup a workflow
rf_wflow_1 <-
    workflow() %>%
    add_model(rf_model_1) %>%
    add_formula(
        injury_severity ~
        weather_condition +
        lighting_condition +
        city
    )

# Now, add a formula as a preprocessor
#rf_wflow_1 <-
#    rf_wflow_1 %>%
#    add_formula(
#        "Injury Severity" ~
#        "Weather Condition" +
#        "Lighting Condition" +
#        "City"
#    )

rf_fit <- fit(rf_wflow_1, data = train_df)
rf_fit


#Use preditc() of the fitted model
#prediction <- predict(rf_fit, test_df %>% slice(1:25) %>% print(n = 25))
prediction <- predict(rf_fit, test_df)

# Trying to get performance metrics
#metrics(prediction, truth, predicted)
prediction %>%
  accuracy(truth = prediction$injury_severity, estimate = .pred_class)

#final_rf_result <- last_fit(rf_wflow_1, sample)
#final_rf_result
#accuracy(prediction, .pred_class)

# Initiate the k Nearest Neightbor model
install.packages("kknn")
library(kknn)

knn_model <- nearest_neighbor(
    neighbors = integer(1),
    weight_func = character(1),
    dist_power = double(1)
) %>%
    set_engine("kknn") %>%
    set_mode("classification") %>%
    translate()
knn_model





# End of "multiple_workflows.r"