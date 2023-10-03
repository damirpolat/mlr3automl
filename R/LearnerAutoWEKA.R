#' @title Auto-WEKA Learner
#'
#' @include LearnerAutoWEKA.R
#'
#' @description
#' Auto-WEKA learner.
#'
#' @export
LearnerAutoWEKA = R6Class("LearnerAutoWEKA",
  public = list(
   task = NULL,
   graph_learner = NULL,
   at = NULL,
   #' @description
   #' Creates a new instance of this [R6][R6::R6Class] class.
   initialize = function(task) {
     assert_task(task)
     self$task = task
     
     learner_names = learners_default[[task$task_type]]
     learners = lrns(learner_names)
     
     graph = ppl("branch", lapply(learners, po))
     graph = ppl("robustify", task = task, factors_to_numeric = TRUE) %>>% graph
     self$graph_learner = as_learner(graph)
     self$graph_learner$id = "graph_learner"
     search_space = default_space(learner_names)
     
     resampling = rsmp("cv", folds = 3)
     measure = msr("classif.ce")
     terminator = trm("evals", n_evals = 5)
     
     instance = ti(
       task = self$task,
       learner = self$graph_learner,
       resampling = resampling,
       measure = measure,
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
       measure = measure,
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