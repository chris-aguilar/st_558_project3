library(readr)
library(dplyr)


# Read in data and train model --------------------------------------------


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


# Helper functions --------------------------------------------------------


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


# Endpoints and parameters ------------------------------------------------

# Provide assignment info
#* @get /info
function(){
  "Student: Chris Aguilar; Assignment Github page: https://chris-aguilar.github.io/st_558_project3/EDA.html"
}

#http://localhost:8000/info


# Obtain prediction based on HighBP, HighChol, BMI, Stroke, HeartDiseaseorAttack, DiffWalk inputs
#* @param HighBP High blood pressure by respondent; 1 if yes, 0 if no.
#* @param HighChol High cholesterol by respondent; 1 if yes, 0 if no.
#* @param BMI Body mass index value of respondent, some number.
#* @param Stroke Stroke occurrence by respondent; 1 if yes, 0 if no.
#* @param HeartDiseaseorAttack Heart disease or heart attack occurrence; 1 if yes, 0 if no.
#* @param DiffWalk Experienced difficulty walking by respondent; 1 if yes, 0 if no.
#* @get /pred
function(HighBP = 0, HighChol = 0, BMI = 28, Stroke = 0, HeartDiseaseorAttack = 0, DiffWalk = 0){
  getPreds(
    logreg_mod, 
    HighBP = as.numeric(HighBP), 
    HighChol = as.numeric(HighChol), 
    BMI = as.numeric(BMI), 
    Stroke = as.numeric(Stroke), 
    HeartDiseaseorAttack = as.numeric(HeartDiseaseorAttack), 
    DiffWalk = as.numeric(DiffWalk))
}

#http://localhost:8000/pred
#http://localhost:8000/pred?BMI=40
#http://localhost:8000/pred?HighBP=1&HighChol=1&BMI=40&Stroke=1&HeartDiseaseorAttack=1&DiffWalk=1