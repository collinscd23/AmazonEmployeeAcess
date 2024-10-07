library(tidymodels)
library(embed) 
library(vroom)
library(reshape2)


train <- vroom("/Users/carsoncollins/Desktop/Stats348/AmazonEmployeeAcess/train.csv")
test <- vroom("/Users/carsoncollins/Desktop/Stats348/AmazonEmployeeAcess/test.csv")

ggplot(train, aes(x = as.factor(ACTION))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Approved vs. Not Approved Actions",
       x = "Action (1 = Approved, 0 = Not Approved)",
       y = "Count") +
  theme_minimal()

train_cor <- cor(train[,sapply(train, is.numeric)])
melted_cor <- melt(train_cor)

ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1), space="Lab", 
                       name="Correlation") + 
  theme_minimal() + 
  labs(title = "Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#------------------------------------------------------------------------------#

my_recipe <- recipe(ACTION ~ ., data=train) %>%
step_mutate_at(all_numeric_predictors(), fn = factor) %>% # turn all numeric features into factors
  step_other(all_nominal_predictors(), threshold = .001) %>% # combines categorical values that occur
  step_dummy(all_nominal_predictors())  # dummy variable encoding
  

# apply the recipe to your data
prep <- prep(my_recipe)
baked <- bake(prep, new_data = train)
baked
ncol(baked)

