remove_java_model <- function(javastics_path,java_model_tag){
  #' @title Removing a Stics model version to use from JavaStics
  #'
  #' @description Removing a model entry in preferences file (model exe name, model tag name)
  #'
  #' @details The executable file in the current JavaStics bin folder will not be deleted
  #' @details It will not be possible to run it through JavaStics command line interface
  #'
  #' @param javastics_path JavaStics installation root folder
  #' @param java_model_tag Model name (not executable file name)
  #'
  #' @examples
  #' \dontrun{
  #' remove_java_model("/home/plecharpent/Work/JavaSTICS-v131-stics-v841","model_name")
  #'}
  #'
  #' @keywords internal
  #'
  #@export


  # checking javastics path
  check_java_path(javastics_path)

  if (!exist_java_model(javastics_path,java_model_tag)) {
    warning("The model doesn't exists or his name is miss spelled : ",java_model_tag,".\n Call get_java_models(javastics_path)$tags to list existing names.")
    return()
  }

  # exe_name=basename(java_model_exe)
  # java_exe_path=file.path(javastics_path,"bin",exe_name)
  #
  # if (!file.exists(java_model_exe) & !file.exists(java_exe_path)){
  #   stop("The model executable file doesn't exist : ",java_model_exe)
  # }


  xml_path=file.path(javastics_path,"config","preferences.xml")
  xml_path_ori=file.path(javastics_path,"config","preferences_ori.xml")
  xml_path_prev=file.path(javastics_path,"config","preferences_prev.xml")

  # saving original file
  if (!file.exists(xml_path_ori)) {
    file.copy(xml_path,xml_path_ori)
  }

  # saving a previous version
  file.copy(xml_path,xml_path_prev)

  java_models=get_java_models(javastics_path)


  # getting new list for models
  # id of the model to remove
  modelidx=is.element(get_java_models(javastics_path)$tags,java_model_tag)
  java_models$tags=java_models$tags[!modelidx]
  java_models$exe=java_models$exe[!modelidx]
  nb_models=length(java_models$tags)

  # writing models list string
  fmt="{%s\t%s},"
  java_models_string=""
  for (i in 1:(nb_models)){
    java_models_string=paste0(java_models_string,sprintf(fmt,java_models$tags[i],java_models$exe[i]))
  }

  xml_pref= SticsRFiles ::: xmldocument(xml_path)

  # removing model from last if needed
  if (get_java_model(javastics_path) == java_model_tag){
    SticsRFiles ::: setValues(xml_pref,'//entry[@key="model.last"]',"")
  }

  SticsRFiles ::: setValues(xml_pref,'//entry[@key="model.list"]',java_models_string)

  # writing file
  SticsRFiles ::: saveXmlDoc(xml_pref,xml_path)

}
