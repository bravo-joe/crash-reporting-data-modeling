# Beginning of "multiple_workflows.r"
# Import necessary libraries
library(dplyr) # for functions
library(data.table)
install.packages("tidymodels")
library(tidymodels)
tidymodels_prefer()
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











# End of "multiple_workflows.r"