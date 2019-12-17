get_model_exe <- function(javastics_path, model_name=character(0)){
  #' @title Getting Stics model executable name
  #'
  #' @description Extracting the current used model executable name from preferences file
  #' @description or corresponding one to the given model_name
  #'
  #' @param javastics_path JavaStics installation root folder
  #' @param model_name Model name in model name list
  #'
  #' @examples
  #'\dontrun{
  #' Getting the current model executable
  #' model_exe <- get_model_exe("/home/plecharpent/Work/JavaSTICS-v131-stics-v841")
  #' Getting model executable correponding to the given "model_name"
  #' model_exe <- get_model_exe("/home/plecharpent/Work/JavaSTICS-v131-stics-v841","model_name")
  #'}
  #'
  #' @return Stics model executable name (current, matching given model name)
  #'
  #' @keywords internal
  #'
  #@export

  if(length(model_name)==0) model_name=get_java_model(javastics_path)

  java_models=get_java_models(javastics_path)

  model_id=java_models$tags==model_name

  if (!any(model_id)) stop("The given model name doesn't exist: ",model_name)

  return(java_models$exe[model_id])


}
