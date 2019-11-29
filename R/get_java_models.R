get_java_models <- function(javastics_path){
  #' @title Getting models list from a JavaStics configuration
  #'
  #' @description Extracting Stics model names and executables file names from preferences file
  #'
  #' @param javastics_path JavaStics installation root folder
  #'
  #' @examples
  #' \dontrun{
  #' models <- get_java_models("/home/plecharpent/Work/JavaSTICS-v131-stics-v841")
  #'}
  #'
  #' @return A list with model name list ($tag), and model executables names ($exe)
  #'
  #@export

  # checking javastics path
  check_java_path(javastics_path)

  # if no preference have been set yet
  if (!exists_java_pref(javastics_path)) set_java_pref(javastics_path)

  xml_pref= SticsRFiles ::: xmldocument(file.path(javastics_path,"config","preferences.xml"))
  models_string= SticsRFiles ::: getValues(xml_pref,'//entry[@key="model.list"]')
  mod=strsplit(models_string,"\t")
  mod=unlist(lapply(mod, function(x) strsplit(x,",")))
  mod_tags=mod[seq(1,length(mod),2)]
  mod_exe=mod[seq(2,length(mod),2)]
  mod_tags=unlist(lapply(mod_tags,function(x) substr(x,2,nchar(x))))
  mod_exe=unlist(lapply(mod_exe,function(x) substr(x,1,nchar(x)-1)))

  models=list()
  models$tags=mod_tags
  models$exe=mod_exe
  return(models)
}
