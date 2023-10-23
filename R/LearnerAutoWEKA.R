#' @title Auto-WEKA Learner
#'
#' @include LearnerAutoWEKA.R
#'
#' @description
#' Class for Auto-WEKA like learner in mlr3automl.
#' The original Auto-WEKA system performed model based optimization using random forest learner
#' as basis for a surrogate model.
#'
#' @param task ([`Task`][mlr3::Task]) \cr
#' [`Task`][mlr3::Task] for learning.
#' 
#' @examples
#' \dontrun{
#' library(mlr3)
#' library(mlr3automl)
#'
#' classif_model = LearnerAutoWEKA(tsk("iris"))
#' classif_model$train()
#' 
#' regr_model = LearnerAutoWEKA(tsk("mtcars"))
#' regr_model$train()
#' }
#' @importFrom R6 R6Class
#' @export
LearnerAutoWEKA = R6Class("LearnerAutoWEKA",
  public = list(
   task = NULL,
   graph_learner = NULL,
   at = NULL,
   measure = NULL,
   #' @description
   #' Creates a new instance of this class.
   initialize = function(task, metric) {
     assert_task(task)
     self$task = task
     task_type = task$task_type
     
     learner_names = paste(task_type, learners_default, sep = ".")
     learners = lrns(learner_names)
     
     graph = ppl("branch", lapply(learners, po))
     graph = ppl("robustify", task = task, factors_to_numeric = TRUE) %>>% graph
     self$graph_learner = as_learner(graph)
     self$graph_learner$id = "graph_learner"
     search_space = default_space(learners_default, task_type)
     
     resampling = rsmp("cv", folds = 3)
     self$measure = msr(metric)
     terminator = trm("evals", n_evals = 5)
     
     instance = ti(
       task = self$task,
       learner = self$graph_learner,
       resampling = resampling,
       measure = self$measure,
       terminator = terminator,
       search_space = search_space
     )
     surrogate = default_surrogate(instance)
     acq_function = default_acqfunction(instance)
     acq_optimizer = default_acqoptimizer(acq_function)
     
     tuner = tnr("mbo",
                 loop_function = bayesopt_ego,
                 surrogate = surrogate,
                 acq_function = acq_function,
                 acq_optimizer = acq_optimizer)
     
     self$at = auto_tuner(
       tuner = tuner,
       learner = self$graph_learner,
       resampling = resampling,
       measure = self$measure,
       terminator = terminator,
       search_space = search_space
     )
   },
   
   train = function(row_ids = NULL) {
     self$at$train(self$task, row_ids)
   },
   
   predict = function(data = NULL, row_ids = NULL) {
     if (is.null(data)) {
       return(self$at$predict(self$task, row_ids))
     } else {
       return(self$at$predict_newdata(data))
     }
   }
  )
)