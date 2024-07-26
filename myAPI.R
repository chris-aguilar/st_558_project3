library(readr)
library(dplyr)

# Grabbing response and predictors of simplest model
# Since the binary predictors are already indicator variables, we'll leave them as-is so everything's staying numeric
# We'll only convert the response to a binary factor variable
diabetes2015 <- read_csv("data/diabetes_binary_health_indicators_BRFSS2015.csv") |>
  select(Diabetes_binary, HighBP, HighChol, BMI, Stroke, HeartDiseaseorAttack, DiffWalk) |>
  mutate(Diabetes_binary = ifelse(Diabetes_binary == 1, "yes", "no") |> factor())

# training the model on all the rows and the relevant inputs/response
logreg_mod <- glm(Diabetes_binary ~ HighBP + HighChol + BMI + Stroke + HeartDiseaseorAttack + DiffWalk, 
                  data = diabetes2015,
                  family = "binomial")

# helper function to find the mean or the mode of a variable
summarizer <- function(x) {
  # considering numeric to have more than two unique values and be type numeric
  if(is.numeric(x) & length(unique(x)) > 2) {
    res <- mean(x)
  } else {
    res <- tail(names(sort(table(x))), 1) # grabs most prevalent value
  }
  res
}

# using helper to determine appropriate default input values
lapply(diabetes2015, summarizer) |> as.data.frame()

# Function to get predictions
getPreds <- function(model_fit, HighBP = 0, HighChol = 0, BMI = 28, Stroke = 0, HeartDiseaseorAttack = 0, DiffWalk = 0) {
  
  input_data <- data.frame(
    HighBP = HighBP, 
    HighChol = HighChol, 
    BMI = BMI, 
    Stroke = Stroke, 
    HeartDiseaseorAttack = HeartDiseaseorAttack, 
    DiffWalk = DiffWalk)
  res <- predict(model_fit, newdata = input_data, type = "response")
  res
}
