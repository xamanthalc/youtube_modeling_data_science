library(readr)
library(ggfortify)
library(arrow)
library(skimr)
library(tidymodels)
library(lubridate)
library(patchwork)
library(stringr)
load("data/youtube_pred.rda")
set.seed(123)

#Data set manipulation ----

#subsetting youtube_pred data set
#to make the data set more efficient for modeling
#reasoning explained in the Rmd
youtube_modeling <- youtube_pred %>%
  #getting rid of variables 
  select(-c(title, publishedAt, 
            channelId, tags, 
            thumbnail_link, description, 
            id, channelTitle, 
            released_date, trending_date)) %>%
  #making some variables factors for better dummy conversion later
  mutate(has_thumbnail = factor(has_thumbnail), 
         ratings_disabled = factor(ratings_disabled), 
         comments_disabled = factor(comments_disabled), 
         categoryId = factor(categoryId), 
         day_week_released = factor(day_week_released))

#showing first results
head(youtube_modeling, 10)

#Allocation of data ----

##Splitting data ----
#split training-test
youtube_split <- youtube_modeling %>% 
  #80% for training set and 20% for test set
  #stratify by outcome variable 
  initial_split(prop = 0.8, strata = target, breaks = 4)

#getting training set
youtube_training <- youtube_split %>% training()

#getting test set 
youtube_test <- youtube_split %>% testing()

##Resamples----
#making resamples
#5 folds with 3 repeats
resamples <- vfold_cv(youtube_training, v = 5, repeats = 3, strata = target)
resamples

#Model selection ----

##Models ----

###linear simple----
lm_model <- 
  linear_reg() %>%
  set_engine("lm")

###ridge----
ridge_model <- 
  #mixture 0 to indicate that it is ridge
  linear_reg(penalty = 0.001, mixture = 0) %>%
  set_engine("glmnet")

###lasso----
lasso_model <- 
  #mixture 1 to indicate that it is lasso
  linear_reg(penalty = 0.001, mixture = 1) %>%
  set_engine("glmnet")

###random forest----
rand_model <- 
  #using default values
  rand_forest(mode = "regression") %>%
  set_engine("ranger")

###random forest customized----
rand_model_customized <- 
  #making 1000 trees
  rand_forest(mode = "regression", trees = 1000) %>%
  set_engine("ranger")

##Recipes----
####no interaction----
recipe_for_without_inter <- 
  #outcome: target, predictor: all other variable
  recipe(target ~.,data = youtube_training) %>%
  #get rid of variables below
  step_rm(video_id, view_count, likes) %>%
  #turning the following variables into log based 10
  step_log(comment_count, base = 10, signed = TRUE) %>% 
  step_log(duration_seconds, base = 10, signed = TRUE) %>%
  step_log(dislikes, base = 10, signed = TRUE) %>%
  #turning nominal predictors into dummy variables 
  step_dummy(all_nominal_predictors()) %>%
  #normalize numeric predictors: center and scale 
  step_normalize(all_numeric_predictors())

####interaction----
recipe_for_with_inter <- 
  #outcome: target, predictor: all other variable
  recipe(target ~.,data = youtube_training) %>%
  #get rid of variables below
  step_rm(video_id, view_count, likes) %>%
  #turning the following variables into log based 10
  step_log(comment_count, base = 10, signed = TRUE) %>% 
  step_log(duration_seconds, base = 10, signed = TRUE) %>%
  step_log(dislikes, base = 10, signed = TRUE) %>%
  #turning nominal predictors into dummy variables
  step_dummy(all_nominal_predictors()) %>%
  #normalize numeric predictors: center and scale 
  step_normalize(all_numeric_predictors()) %>%
  #add interactions 
  step_interact(~all_predictors()*all_predictors())


##Workflow sets----
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

preproc_lm <- 
  list(rec_no_inter = recipe_for_without_inter, 
       rec_inter = recipe_for_with_inter
  )

###linear----
lm_models <- workflow_set(preproc_lm, list(lm = lm_model), cross = FALSE)
lm_models <- 
  lm_models %>% 
  #workflow map to apply the resamples to each model 
  workflow_map("fit_resamples", 
               # Options to `workflow_map()`: 
               #seed for reproducibility and verbose to print progress
               seed = 123, verbose = TRUE,
               resamples = resamples, control = keep_pred)

###lasso----
lasso_models <- workflow_set(preproc_lm, list(lm = lasso_model), cross = FALSE)
lasso_models <- 
  lasso_models %>% 
  #workflow map to apply the resamples to each model 
  workflow_map("fit_resamples", 
               # Options to `workflow_map()`: 
               #seed for reproducibility and verbose to print progress
               seed = 123, verbose = TRUE,
               resamples = resamples, control = keep_pred)

###ridge----
ridge_models <- workflow_set(preproc_lm, list(lm = ridge_model), cross = FALSE)
ridge_models <- 
  ridge_models %>% 
  #workflow map to apply the resamples to each model 
  workflow_map("fit_resamples", 
               # Options to `workflow_map()`: 
               #seed for reproducibility and verbose to print progress
               seed = 123, verbose = TRUE,
               resamples = resamples, control = keep_pred)

##Individual workflows ----

###default----
rf_workflow_default <- 
  #empty workflow
  workflow() %>% 
  #adding recipe without interaction
  add_recipe(recipe_for_without_inter) %>% 
  #adding default random model
  add_model(rand_model)

rf_default_models <- 
  #fitting default random forest workflow into resamples
  rf_workflow_default %>% 
  #keeping predictions 
  fit_resamples(resamples = resamples, control = keep_pred)

#save fitted resamples 
save(rf_default_models, file = "data/rf_default_models.rda") #to load later

###customized----
rf_workflow_cust <- 
  #empty workflow
  workflow() %>% 
  #adding recipe without interaction
  add_recipe(recipe_for_without_inter) %>% 
  #adding customized random model
  add_model(rand_model_customized)

rf_customized_models <- 
  #fitting customized random forest workflow into resamples
  rf_workflow_cust %>% 
  #keeping predictions 
  fit_resamples(resamples = resamples, control = keep_pred) #you need to run this 

#save fitted resamples 
save(rf_customized_models, file = "data/rf_customized_models.rda")

##Merging into one workflow set ----

#change id names to yes if the recipe had interaction 
#and no if the recipe did not have interaction 
#saving the workflows 
lm_models$wflow_id <- c("lm_no", "lm_yes")
save(lm_models, file = "data/lm_models.rda")
lasso_models$wflow_id <- c("lasso_no", "lasso_yes")
save(lasso_models, file = "data/lasso_models.rda")
ridge_models$wflow_id <- c("ridge_no", "ridge_yes")
save(ridge_models, file = "data/ridge_models.rda")

#joining linear models in a workflow set
linear_models <-lm_models %>%
  bind_rows(lasso_models) %>%
  bind_rows(ridge_models)

#adding the default random forest to the workflow set
almost_all_models <- 
  #adding work flow
  as_workflow_set(random_forest = rf_default_models) %>% 
  #to the previous set
  bind_rows(linear_models)

#adding the customized random forest to the workflow set
b <- as_workflow_set(random_forest_cust = rf_customized_models)
all_models <- 
  #adding work flow
  b %>% 
  #to the previous set
  bind_rows(almost_all_models)

#save all models 
save(all_models, file = "data/all_models.rda")

##Assessing models ----

###plots ----
#plot models in order to which ranks the best 
autoplot(all_models) +
  #personalizing colors and theme 
  scale_color_manual(values = c("black", "#cc0000")) +
  theme_bw()

###metrics ----
#getting the mean r-squared of all models 
collect_metrics(all_models) %>% filter(.metric == "rsq")
#getting the mean rmse of all models 
collect_metrics(all_models) %>% filter(.metric == "rmse")

###chosen model visualization ----
#the customized random forest was selected as the best model 
#visualizing selected model

#collecting_predictions
assess_res <- collect_predictions(rf_customized_models)

#making plot 
assess_res %>% 
  #map x to target variable (actual values)
  #map y to target variable (predicted values)
  ggplot(aes(x = target, y = .pred)) + 
  #make a scatterplot with alpha to account for overplotting
  geom_point(alpha = .15) +
  #add an ab line 
  geom_abline(color = "red") + 
  #adjusts axis for square grid 
  coord_obs_pred() + 
  #personalizing labels
  theme_bw() +
  labs(y = "predictions in the training set - resamples", 
       x = "actual values in the training set")

#Applying to the test set ----

##fitting ----

#workflow to fit
rf_workflow_cust <- 
  workflow() %>% 
  add_recipe(recipe_for_without_inter) %>% 
  add_model(rand_model_customized)

#fit on training set
final_fit <- fit(rf_workflow_cust, youtube_training)
save(final_fit, file = "data/final_fit.rda")

##actual vs predicted----

#actual vs predicted values
truth_against_pred <- youtube_test %>%
  select(target) %>%
  bind_cols(predict(final_fit, youtube_test))

#tibble actual vs. predicted
truth_against_pred

#plot 
truth_against_pred %>%
  #mapping x to actual values and y to predicted values 
  ggplot(aes(x = target, 
             y = .pred)) +
  #make scatterplot with alpha to account for overplotting 
  geom_point(alpha = .15) +
  #add an ab line
  geom_abline(color = "red") + 
  #adjusts axis for square grid 
  coord_obs_pred() +
  #personalize theme and labels 
  theme_bw() +
  labs(y = "predictions in the test set", 
       x = "actual values in the test set")

##metrics ----
#r-squared
rsq(truth_against_pred, truth = target, estimate = .pred)

#rmse
rmse(truth_against_pred, truth = target, estimate = .pred)

#written comments in Rmd

#FINISH SECTION 
