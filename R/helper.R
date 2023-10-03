# get default search space
default_space = function(learner_list) {
  sp = ParamSet$new()
  # set params
  for (learner in learner_list) {
    for (param in learner_params[[learner]]) {
      sp$add(param)
    }
  }
  
  # set tune tokens
  for (learner in learner_list) {
    for (token_name in names(learner_tokens[[learner]])) {
      sp$set_values(.values = learner_tokens[[learner]][token_name])
    }
  }
  
  # add dependencies for branch selection
  sp = add_branch_selection_dependencies(learner_list, sp)
  
  return(sp)
}


learner_params = list(
  'classif.glmnet' = list(
    'classif.glmnet.s' = ParamDbl$new(id = 'classif.glmnet.s', lower = 0, upper = Inf, default = 0.01),
    'classif.glmnet.alpha' = ParamDbl$new(id = 'classif.glmnet.alpha', lower = 0, upper = 1, default = 1)
  ),
  'classif.kknn' = list(
    'classif.kknn.k' = ParamInt$new(id = 'classif.kknn.k', lower = 1, upper = Inf, default = 7),
    'classif.kknn.distance' = ParamDbl$new(id = 'classif.kknn.distance', lower = 1, upper = Inf, default = 2),
    'classif.kknn.kernel' = ParamFct$new(id = 'classif.kknn.kernel', levels = c("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian",
                                                                                "rank", "optimal"), default = "optimal")
  ),
  'classif.ranger' = list(
    'classif.ranger.mtry.ratio' = ParamDbl$new(id = 'classif.ranger.mtry.ratio', lower = 0, upper = 1),
    'classif.ranger.replace' = ParamLgl$new(id = 'classif.ranger.replace', default = TRUE),
    'classif.ranger.sample.fraction' = ParamDbl$new(id = 'classif.ranger.sample.fraction', lower = 0, upper = 1),
    'classif.ranger.num.trees' = ParamInt$new(id = 'classif.ranger.num.trees', lower = 1, upper = Inf, default = 500)
  ),
  'classif.rpart' = list(
    'classif.rpart.minsplit' = ParamInt$new(id = 'classif.rpart.minsplit', lower = 1, upper = Inf, default = 20),
    'classif.rpart.minbucket' = ParamInt$new(id = 'classif.rpart.minbucket', lower = 1, upper = Inf),
    'classif.rpart.cp' = ParamDbl$new(id = 'classif.rpart.cp', lower = 0, upper = 1, default = 0.01)
  ),
  'classif.xgboost' = list(
    'classif.xgboost.eta' = ParamDbl$new(id = 'classif.xgboost.eta', lower = 0, upper = 1, default = 0.3),
    'classif.xgboost.nrounds' = ParamInt$new(id = 'classif.xgboost.nrounds', lower = 1, upper = Inf),
    'classif.xgboost.max_depth' = ParamInt$new(id = 'classif.xgboost.max_depth', lower = 0, upper = Inf, default = 6),
    'classif.xgboost.colsample_bytree' = ParamDbl$new(id = 'classif.xgboost.colsample_bytree', lower = 0, upper = 1, default = 1),
    'classif.xgboost.colsample_bylevel' = ParamDbl$new(id = 'classif.xgboost.colsample_bylevel', lower = 0, upper = 1, default = 1),
    'classif.xgboost.lambda' = ParamDbl$new(id = 'classif.xgboost.lambda', lower = 0, upper = Inf, default = 1),
    'classif.xgboost.alpha' = ParamDbl$new(id = 'classif.xgboost.alpha', lower = 0, upper = Inf, default = 0),
    'classif.xgboost.subsample' = ParamDbl$new(id = 'classif.xgboost.subsample', lower = 0, upper = 1, default = 1)
  )
)

learner_tokens = list(
  'classif.glmnet' = list(
    'classif.glmnet.s' = to_tune(1e-04, 10000, logscale = TRUE),
    'classif.glmnet.alpha' = to_tune(0, 1)
  ),
  'classif.kknn' = list(
    'classif.kknn.k' = to_tune(1, 50, logscale = TRUE),
    'classif.kknn.distance' = to_tune(1, 5),
    'classif.kknn.kernel' = to_tune(levels = c("rectangular", "optimal", "epanechnikov", "biweight", 
                                               "triweight", "cos", "inv", "gaussian", "rank"))
  ),
  'classif.ranger' = list(
    'classif.ranger.mtry.ratio' = to_tune(lower = 0, upper = 1),
    'classif.ranger.replace' = to_tune(levels = c(TRUE, FALSE)),
    'classif.ranger.sample.fraction' = to_tune(0.1, 1),
    'classif.ranger.num.trees' = to_tune(1, 2000)
  ),
  'classif.rpart' = list(
    'classif.rpart.minsplit' = to_tune(2, 128, logscale = TRUE),
    'classif.rpart.minbucket' = to_tune(1, 64, logscale = TRUE),
    'classif.rpart.cp' = to_tune(1e-04, 0.1, logscale = TRUE)
  ),
  'classif.xgboost' = list(
    'classif.xgboost.eta' = to_tune(1e-04, 1, logscale = TRUE),
    'classif.xgboost.nrounds' = to_tune(1, 5000),
    'classif.xgboost.max_depth' = to_tune(1, 20),
    'classif.xgboost.colsample_bytree' = to_tune(1e-01, 1),
    'classif.xgboost.colsample_bylevel' = to_tune(1e-01, 1),
    'classif.xgboost.lambda' = to_tune(1e-03, 1000, logscale = TRUE),
    'classif.xgboost.alpha' = to_tune(1e-03, 1000, logscale = TRUE),
    'classif.xgboost.subsample' = to_tune(1e-01, 1)
  )
)


# add dependencies for branches
add_branch_selection_dependencies = function(learner_list, param_set) {
  param_set$add(ParamFct$new("branch.selection", learner_list))
  param_set$set_values(.values = list(branch.selection = to_tune(learner_list)))
  for (learner in learner_list) {
    for (param in names(param_set$values)[grepl(learner, names(param_set$values))]) {
      param_set$add_dep(param, "branch.selection",
                        CondEqual$new(learner))
    }
  }
  return(param_set)
}

learners_default = list(
  'classif' = c("classif.glmnet", "classif.kknn", "classif.ranger", "classif.rpart", "classif.xgboost")
)
