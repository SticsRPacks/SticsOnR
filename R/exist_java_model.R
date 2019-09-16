exist_java_model <- function(javastics_path,java_model_tag){
  #' @title Getting existing status of a Stics model name
  #'
  #' @description Finding in JavaStics preference.xml file in the given JavaStics path,
  #' if the given model name exists
  #'
  #' @param javastics_path JavaStics installation root folder
  #' @param java_model_tag Model name in JavaStics configuration
  #'
  #' @examples
  #' exist_java_model("/home/plecharpent/Work/JavaSTICS-v131-stics-v841/example","stics_name")
  #'
  #' @return Existing status, logical
  #'
  #' @export
  #'
  # ----------------------------------------------------------------------
  #  MODIFICATIONS (last commit)
  #  $Date: 2019-06-03 11:56:53 +0200 (lun. 03 juin 2019) $
  #  $Author: plecharpent $
  #  $Revision: 1429 $
  # ----------------------------------------------------------------------

  # if no preference have been set yet
  if (!exists_java_pref(javastics_path)) set_java_pref(javastics_path)

  #
  models = get_java_models(javastics_path)
  exist_model=TRUE
  if (!is.element(java_model_tag,models$tags)) {
    exist_model = FALSE
  }
  return(exist_model)
}
