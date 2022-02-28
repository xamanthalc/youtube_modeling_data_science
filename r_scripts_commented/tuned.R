
#Recipes ----

#interaction
#making recipe that will be used with tune linear penalized models 
recipe_for_with_inter #from modeling.R 

#splines
#making recipe that will be used with lm model 
recipe_for_with_inter_sp <- 
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
  step_interact(~all_predictors()*all_predictors()) %>%
  step_ns(comment_count, deg_free = tune("comment_count df")) %>% #tuning parameter
  step_ns(hour_released, deg_free = tune("hour_release df")) #tuning parameter

#Models ----
#linear models 
lasso_model_tune <- 
  #mixture 1 to indicate that it is lasso
  linear_reg(penalty = tune(), #tuning parameter
             mixture = 1) %>%
  set_engine("glmnet")

ridge_model_tune <- 
  #mixture 0 to indicate that it is ridge
  linear_reg(penalty = tune(),  #tuning parameter
             mixture = 0) %>%
  set_engine("glmnet")

lm_model_tune <- linear_reg() %>%
  set_engine("lm") #the tuning parameter is in the recipe 

#Parameters ----
#lm
#lm parameters are executed after the workflow

#lasso
lasso_params <- parameters(lasso_model_tune) #using default
lasso_grid <- grid_regular(lasso_params, levels = 8)

#ridge
ridge_params <- parameters(ridge_model_tune) #using default
ridge_grid <- grid_regular(ridge_params, levels = 8)

#Grids ----
#lm_grid is executed after the workflow 
lasso_grid
ridge_grid

#Workflows ----
#lm
lm_tune_workflow <- workflow() %>% #first
  add_model(lm_model_tune) %>% 
  add_recipe(recipe_for_with_inter_sp)

lm_params <- parameters(lm_tune_workflow)#parameters for lm
lm_grid <- grid_regular(lm_params, levels = 4)#grid 4x4

lm_grid #lm grid result 

#lasso
lasso_tune_workflow <- workflow() %>% 
  add_model(lasso_model_tune) %>% 
  add_recipe(recipe_for_with_inter)

#ridge
ridge_tune_workflow <- workflow() %>% 
  add_model(ridge_model_tune) %>% 
  add_recipe(recipe_for_with_inter)

#tune grids
#lm
lm_tuned <- lm_tune_workflow %>%
  tune_grid(resamples, grid = lm_grid)
#save(lm_tuned, file = "data/lm_tuned.rda") save object 
autoplot(lm_tuned, metric = "rmse") #autoplot to visualize performance
select_best(lm_tuned, metric = "rmse") #best tuning 

#lasso
lasso_tuned <- lasso_tune_workflow %>%
  tune_grid(resamples, grid = lasso_grid)
#save(lasso_tuned, file = "data/lasso_tuned.rda") save object 
autoplot(lasso_tuned, metric = "rmse") #autoplot to visualize performance
select_best(lasso_tuned, metric = "rmse") #best tuning 

#ridge
ridge_tuned <- ridge_tune_workflow %>%
  tune_grid(resamples, grid = ridge_grid)
#save(ridge_tuned, file = "data/ridge_tuned.rda") save object 
autoplot(ridge_tuned) #autoplot to visualize performance
select_best(ridge_tuned, metric = "rmse") #best tuning

#Final parameters ----
##lm ----
recipe_for_with_inter_sp_final <- 
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
  step_interact(~all_predictors()*all_predictors()) %>%
  step_ns(comment_count, deg_free = 15) %>%
  step_ns(hour_released, deg_free = 15)

#workflow
final_lm_tune_workflow <- workflow() %>% #first
  add_model(lm_model_tune) %>%
  add_recipe(recipe_for_with_inter_sp_final)

#fitting
final_lm_tune_fit <- 
  final_lm_tune_workflow %>% 
  fit_resamples(resamples)
#save(final_lm_tune_fit, file = "data/final_lm_tune_fit.rda") save object 

#collect metrics
collect_metrics(final_lm_tune_fit)


##lasso ----
#param
lasso_param <- 
  tibble(penalty = 0.0000000001) #selected parameter after tuning 

#workflow
final_lasso_tune_workflow <- 
  lasso_tune_workflow %>% 
  finalize_workflow(lasso_param) #finalizing wflow with the selected parameter

#fitting
final_lasso_tune_fit <- #fitting on resamples 
  final_lasso_tune_workflow %>% 
  fit_resamples(resamples)
#save(final_lasso_tune_fit, file = "data/final_lasso_tune_fit.rda") save object 

#collect metrics 
collect_metrics(final_lasso_tune_fit)

##ridge ----
#param
ridge_param <- 
  tibble(penalty = 0.0000000001) #selected parameter after tuning 

#workflow
final_ridge_tune_workflow <- 
  ridge_tune_workflow %>% 
  finalize_workflow(ridge_param) #finalizing wflow with the selected parameter

#fitting
final_ridge_tune_fit <- #fitting on resamples 
  final_ridge_tune_workflow %>% 
  fit_resamples(resamples)
#save(final_ridge_tune_fit, file = "data/final_ridge_tune_fit.rda") save object

#collect metrics 
collect_metrics(final_ridge_tune_fit) 


#Summary parameters ----
#making tibble
summary_tuned <- data.frame("linear_splines" = c(0.507, 0.0283), 
                            "lasso" = c(0.444, 0.0301), 
                            "ridge" = c(0.440, 0.0302))
#row names for the tibble 
row.names(summary_tuned) <- c("mean rsqr", "mean rmse")
summary_tuned

#FINISH SECTION
