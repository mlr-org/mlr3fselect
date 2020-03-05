#' @title Titanic Classification Task
#'
#' @name mlr_tasks_titanic
#' @importFrom mlr3 as_data_backend
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("titanic")
#' tsk("titanic")
#' ```
#'
#' @description
#' Titanic data set from the opendatasoft repository
#' (\url{https://public.opendatasoft.com/explore/dataset/titanic-passengers}).
NULL

load_task_titanic = function(id = "spam") {
  b = as_data_backend(readRDS(system.file("extdata", "titanic.rds", package = "mlr3fselect")))
  task = TaskClassif$new(id, b, target = "Survived", positive = "Yes")
  b$hash = task$man = "mlr3::mlr_tasks_titanic"
  task
}
