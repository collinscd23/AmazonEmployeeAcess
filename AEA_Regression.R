library(tidymodels)
library(embed) 
library(vroom)
library(reshape2)


train <- vroom("/Users/carsoncollins/Desktop/Stats348/AmazonEmployeeAcess/train.csv")
test <- vroom("/Users/carsoncollins/Desktop/Stats348/AmazonEmployeeAcess/test.csv")

train$ACTION <- as.factor(train$ACTION)

my_recipe <- recipe(ACTION ~ ., data=train) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>% # turn all numeric features into factors
  step_other(all_nominal_predictors(), threshold = .001) %>% # combines categorical values that occur
  step_dummy(all_nominal_predictors())  # dummy variable encoding


# apply the recipe to your data
prep <- prep(my_recipe)
baked <- bake(prep, new_data = train)

logRegModel <- logistic_reg() %>% #Type of model
  set_engine("glm")

## Put into a workflow here
logReg_workflow <- workflow() %>%
  add_model(logRegModel) %>%
  add_recipe(my_recipe) %>%
  fit(train)


## Make predictions
amazon_predictions <- predict(logReg_workflow,
                              new_data = test,
                              type = "prob") # "class" or "prob" (see doc)

# Extract the probability of the positive class (ACTION = 1)
amazon_predictions <- amazon_predictions$.pred_1

# Prepare the submission file
submission <- test %>%
  dplyr::select(id) %>%  # Ensure 'id' is the correct column name
  mutate(ACTION = amazon_predictions) # Add predicted probabilities

# Save the submission file
vroom_write(submission, 
            path = "/Users/carsoncollins/Desktop/Stats348/AmazonEmployeeAcess/AEA_Regression.csv", 
            delim = ",")
