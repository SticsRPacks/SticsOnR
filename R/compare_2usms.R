compare_2usms <- function(first_dir,second_dir,in_usms_list=c()){
  #' @title Comparing Stics daily outputs of 2 directories
  #' @description For common usm, or a given usm list, and for common variables correlations are calculated
  #' @param first_dir A first output directory path
  #' @param second_dir A second output directory path
  #' @param in_usms_list Usms selection list inside usms from usms.xml file (optional)
  #' @return A list with usms names, usms correlations for variables, and bad NULL and NA status !!!
  #' @examples
  #' out <- compare_2usms("out_dir1","out_dir2")
  #' out <- compare_2usms("out_dir1","out_dir2",c("ble","mais"))
  #' @export

  # getting common usms
  first_dir_path=file.path(first_dir,"usms.xml")
  second_dir_path=file.path(second_dir,"usms.xml")
  usms_list=base :: intersect(get_usms_list(first_dir_path),get_usms_list(second_dir_path))
  # getting common usms with input list
  if (length(in_usms_list)>0) usms_list=base :: intersect(usms_list,in_usms_list)

  usms_nb=length(usms_list)

  plants_nb=get_plants_nb(first_dir_path)
  plants_nb_2=get_plants_nb(second_dir_path)

  if (!all(plants_nb==plants_nb_2)) stop("Usms plants numbers are not consistent in: ",first_dir, " and ",second_dir)
  # if (length(plants_nb)!=usms_nb) stop("Plants number vector length differs from the usms number !")

  plants_sum=sum(plants_nb)
  cor_bad_status=rep(0,plants_sum)
  cor_NA_status=rep(0,plants_sum)
  cor_NULL_status=rep(0,plants_sum)
  usm_cor=vector("list",plants_sum)
  usm_names=vector("character",plants_sum)


  for (usm in 1:usms_nb){
    name=usms_list[usm]
    nplants=plants_nb[usm]

    print(name)
    print(usm)

    asso=""
    if (nplants==2) asso="p"
    for (plant in 1:nplants){
      pname=paste0(asso,name)
      first_path=file.path(first_dir,paste0("mod_s",pname,".sti"))
      if (!file.exists(first_path)) next
      second_path=file.path(second_dir,paste0("mod_s",pname,".sti"))
      if (!file.exists(second_path)) next
      first_tbl <- try(read.table(first_path,header = TRUE,sep = ";",stringsAsFactors = FALSE))
      if (is(first_tbl,"try-error")) next
      second_tbl <- try(read.table(second_path,header = TRUE,sep = ";",stringsAsFactors = FALSE))
      if (is(second_tbl,"try-error")) next
      names_first=names(first_tbl)
      names_second=names(second_tbl)
      common_names=base::intersect(names_first[-(1:5)],names_second[-(1:5)])
      cor_var=rep(NaN,length(common_names))
      for (var in 1:length(common_names)){
        varname=common_names[var]
        val_first=first_tbl[,names(first_tbl)==varname]
        val_second=second_tbl[,names(second_tbl)==varname]
        cor_val=try(cor(val_first,val_second))
        if (is(cor_val,"try-error")) {
          print(varname)
        } else {
          cor_var[var]=cor_val
        }
        #plot(val_first,val_second,main=varname)
      }
      id_plant=usm+plant-1
      cor_bad_status[id_plant]=any(cor_var[!is.na(cor_var)] < 0.999)
      cor_NA_status[id_plant]=any(is.na(cor_var))
      cor_NULL_status[id_plant]=any(is.null(cor_var))

      usm_cor[[id_plant]]=list(cor_var,common_names)
      usm_names[id_plant]=pname
      asso="a"
    }
  }
  # output
  out=list()
  out$cor_bad_status=cor_bad_status
  out$cor_NA_status=cor_NA_status
  out$cor_NULL_status=cor_NULL_status
  out$usm_cor=usm_cor
  out$names=usm_names
  return(out)
}
