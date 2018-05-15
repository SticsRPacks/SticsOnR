#' Read STICS input parameters
#'
#' @description Read an input parameter from a pre-existing STICS input
#'              file. Generally used after calling \code{\link{set_usm}}.
#'
#' @param filepath File path
#' @param param    Parameter name. Optional, if not provided, the function
#'                 return an object with all parameters
#'
#' @seealso \code{\link{set_usm}}.
#'
#' @examples
#'\dontrun{
#' # Replace the interrow distance parameter to 0.01:
#'
#' library(sticRs)
#' filepath= '1-Simulations/IC_Wheat_Wheat/fictec1.txt'
#' Param= read_param(filepath = filepath)
#'
#'}
#'
#' @export
#'
read_param= function(filepath,param=NULL){
  # Make this function read all parameters from STICS automatically
  # 1. Detect how much plants there are
  # 2. Read tec+plant files
  # 3. Return the output as a list
  parameters= read_tec(filepath)
  return(parameters[grep(param,parameters)])
}


#' Read STICS technical parameters
#'
#' @description Read the technical input parameter from a pre-existing STICS input
#'              file. Generally not used, prefer using \code{\link{read_param}}.
#'
#' @param filepath     File path
#' @param several_fert Is there several fertilization in the USM ?
#' @param several_thin Is there several thinning in the USM ?
#' @param is_pasture   Is the plant a pasture ?
#'
#' @seealso \code{\link{read_param}}.
#'
#' @examples
#'\dontrun{
#' # Replace the interrow distance parameter to 0.01:
#'
#' library(sticRs)
#' filepath= '1-Simulations/IC_Wheat_Wheat/fictec1.txt'
#' Param= read_param(filepath = filepath)
#'
#'}
#'
#' @export
#'
read_tec= function(filepath,several_fert=T,several_thin=T,is_pasture=F){

  params= readLines(filepath)
  itk= vector(mode='list', length = 0)
  values= params[!seq_along(params)%%2]

  index= 1
  val= function(){
    index<<- index+1
    return(values[index-1])
  }

  itk$P_nbjres= as.numeric(val())

  if(itk$P_nbjres > 0){
    for(i in 1:itk$P_nbjres){
      vec= strsplit(x = val(),split = " ")[[1]]
      itk$P_julres= c(itk$P_julres, vec[1])
      itk$P_coderes= c(itk$P_coderes,vec[2])
      itk$P_qres= c(itk$P_qres,vec[3])
      itk$P_Crespc= c(itk$P_Crespc,vec[4])
      itk$P_CsurNres= c(itk$P_CsurNres,vec[5])
      itk$P_Nminres= c(itk$P_Nminres,vec[6])
      itk$P_eaures= c(itk$P_eaures,vec[7])
    }
  }
  itk$P_nbjtrav= as.numeric(val())
  if(itk$P_nbjtrav > 0){
    for(i in 1:itk$P_nbjtrav){
      vec= strsplit(x = val(),split = " ")[[1]]
      itk$P_jultrav= c(itk$P_jultrav, vec[1])
      itk$P_profres= c(itk$P_profres, vec[2])
      itk$P_proftrav= c(itk$P_proftrav, vec[3])
    }
  }

  itk$P_iplt0= val()
  itk$P_profsem= val()
  itk$P_densitesem= val()
  itk$P_variete= val()
  itk$P_codetradtec= val()
  itk$P_interrang= val()
  itk$P_orientrang= val()
  itk$P_codedecisemis= val()
  itk$P_nbjmaxapressemis= val()
  itk$P_nbjseuiltempref= val()
  itk$P_codestade= val()
  itk$P_ilev= val()
  itk$P_iamf= val()
  itk$P_ilax= val()
  itk$P_isen= val()
  itk$P_ilan= val()
  itk$P_iflo= val()
  itk$P_idrp= val()
  itk$P_imat= val()
  itk$P_irec= val()
  itk$P_irecbutoir= val()
  itk$P_effirr= val()
  itk$P_codecalirrig= val()
  itk$P_ratiol= val()
  itk$P_dosimx= val()
  itk$P_doseirrigmin= val()
  itk$P_codedateappH2O= as.numeric(val())
  itk$nap= as.numeric(val())

  if(itk$nap > 0){
    for(i in 1:itk$nap){
      if(itk$P_codedateappH2O != 1) {
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$P_julapI= c(itk$P_julapI,vec[1])
        itk$P_doseI= c(itk$P_doseI,vec[2])
      }else{
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$P_upvttapI= c(itk$P_upvttapI,vec[1])
        itk$P_doseI= c(itk$P_doseI,vec[2])
      }
    }
  }

  itk$P_codlocirrig= val()
  itk$P_locirrig= val()
  itk$P_profmes= val()

  if(!several_fert){
    itk$P_engrais= val()
  }else{
    # val()
  }

  itk$P_concirr= val()
  itk$P_codedateappN= as.numeric(val())
  itk$P_codefracappN= as.numeric(val())
  itk$P_Qtot_N= val()
  itk$napN= as.numeric(val())

  if(itk$napN > 0){
    for(i in 1:itk$napN){
      if(itk$P_codedateappN != 1) {
        if(itk$P_codefracappN == 1) {
          if(several_fert){
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_julapN= c(itk$P_julapN, vec[1])
            itk$P_doseN= c(itk$P_doseN, vec[2])
            itk$P_engrais= c(itk$P_engrais, vec[3])
          }else{
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_julapN= c(itk$P_julapN, vec[1])
            itk$P_doseN= c(itk$P_doseN, vec[2])
          }
        }else{
          if(several_fert){
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_julapN= c(itk$P_julapN, vec[1])
            itk$P_fracN= c(itk$P_fracN, vec[2])
            itk$P_engrais= c(itk$P_engrais, vec[3])
          }else{
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_julapN= c(itk$P_julapN, vec[1])
            itk$P_fracN= c(itk$P_fracN, vec[2])
          }
        }
      }else{
        if (itk$P_codefracappN == 1) {
          if(several_fert){
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_upvttapN= c(itk$P_upvttapN, vec[1])
            itk$P_doseN= c(itk$P_doseN, vec[2])
            itk$P_engrais= c(itk$P_engrais, vec[3])
          }else{
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_upvttapN= c(itk$P_upvttapN, vec[1])
            itk$P_doseN= c(itk$P_doseN, vec[2])
          }
        }else{
          if(several_fert){
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_upvttapN= c(itk$P_upvttapN, vec[1])
            itk$P_fracN= c(itk$P_fracN, vec[2])
            itk$P_engrais= c(itk$P_engrais, vec[3])
          }else{
            vec= strsplit(x = val(),split = " ")[[1]]
            itk$P_upvttapN= c(itk$P_upvttapN, vec[1])
            itk$P_fracN= c(itk$P_fracN, vec[2])
          }
        }
      }
    }
  }

  itk$P_codlocferti= val()
  itk$P_locferti= val()
  itk$P_ressuite= val()
  itk$P_codcueille= val()
  itk$P_nbcueille= val()
  itk$P_cadencerec= val()
  itk$P_codrecolte= val()
  itk$P_codeaumin= val()
  itk$P_h2ograinmin= val()
  itk$P_h2ograinmax= val()
  itk$P_sucrerec= val()
  itk$P_CNgrainrec= val()
  itk$P_huilerec= val()
  itk$P_coderecolteassoc= val()
  itk$P_codedecirecolte= val()
  itk$P_nbjmaxapresrecolte= val()
  itk$P_codefauche= val()
  itk$P_mscoupemini= val()
  itk$P_codemodfauche= as.numeric(val())

  if(itk$P_codemodfauche == 1) {
    itk$lecfauche= FALSE
  }else{
    itk$lecfauche= TRUE
  }

  itk$P_hautcoupedefaut= val()
  itk$P_stadecoupedf= val()
  nbcoupe2= as.numeric(val())

  if (itk$P_codemodfauche == 2) {
    for(i in 1:nbcoupe2){
      if(is_pasture){
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$P_julfauche= c(itk$P_julfauche, vec[1])
        itk$P_hautcoupe= c(itk$P_hautcoupe, vec[2])
        itk$P_lairesiduel= c(itk$P_lairesiduel, vec[3])
        itk$P_msresiduel= c(itk$P_msresiduel, vec[4])
        itk$P_anitcoupe= c(itk$P_anitcoupe, vec[5])
        itk$P_restit= c(itk$P_restit, vec[6])
        itk$P_mscoupemini= c(itk$P_mscoupemini, vec[7])
      }else{
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$P_julfauche= c(itk$P_julfauche, vec[1])
        itk$P_hautcoupe= c(itk$P_hautcoupe, vec[2])
        itk$P_lairesiduel= c(itk$P_lairesiduel, vec[3])
        itk$P_msresiduel= c(itk$P_msresiduel, vec[4])
        itk$P_anitcoupe= c(itk$P_anitcoupe, vec[5])
      }
    }
    itk$nbcoupe = nbcoupe2
  }else{
    for(i in 1:nbcoupe2){
      # val()
    }
  }

  nbcoupe3= as.numeric(val())

  if(itk$P_codemodfauche == 3) {
    for(i in 1:nbcoupe3){
      if(is_pasture){
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$P_tempfauche= c(itk$P_tempfauche, vec[1])
        itk$P_hautcoupe= c(itk$P_hautcoupe, vec[2])
        itk$P_lairesiduel= c(itk$P_lairesiduel, vec[3])
        itk$P_msresiduel= c(itk$P_msresiduel, vec[4])
        itk$P_anitcoupe= c(itk$P_anitcoupe, vec[5])
        itk$P_restit= c(itk$P_restit, vec[6])
        itk$P_mscoupemini= c(itk$P_mscoupemini, vec[7])
      }else{
        vec= strsplit(x = val(),split = " ")[[1]]
        itk$P_tempfauche= c(itk$P_tempfauche, vec[1])
        itk$P_hautcoupe= c(itk$P_hautcoupe, vec[2])
        itk$P_lairesiduel= c(itk$P_lairesiduel, vec[3])
        itk$P_msresiduel= c(itk$P_msresiduel, vec[4])
        itk$P_anitcoupe= c(itk$P_anitcoupe, vec[5])
      }
    }
    itk$nbcoupe= nbcoupe3
  }else{
    for(i in 1:nbcoupe3){
      # val()
    }
  }

  itk$P_codepaillage= val()
  itk$P_couvermulchplastique= val()
  itk$P_albedomulchplastique= val()
  itk$P_codrognage= val()
  itk$P_largrogne= val()
  itk$P_hautrogne= val()
  itk$P_biorognem= val()
  itk$P_codcalrogne= val()
  itk$P_julrogne= val()
  itk$P_margerogne= val()
  itk$P_codeclaircie= val()

  if(several_thin){
    itk$P_nb_eclair= as.numeric(val())
    for(i in 1:itk$P_nb_eclair){
      vec= strsplit(x = val(),split = " ")[[1]]
      itk$P_juleclair=  c(itk$P_juleclair, vec[1])
      itk$P_nbinfloecl=  c(itk$P_nbinfloecl, vec[2])
    }
  }else{
    # vec= strsplit(x = val(),split = " ")[[1]]
    itk$P_nb_eclair= 1
    itk$P_juleclair=  val()
    itk$P_nbinfloecl=  val()
  }

  itk$P_codeffeuil= val()
  itk$P_codhauteff= val()
  itk$P_codcaleffeuil= val()
  itk$P_laidebeff= val()
  itk$P_effeuil= val()
  itk$P_juleffeuil= val()
  itk$P_laieffeuil= val()
  itk$P_codetaille= val()
  itk$P_jultaille= val()
  itk$P_codepalissage= val()
  itk$P_hautmaxtec= val()
  itk$P_largtec= val()
  itk$P_codabri= val()
  itk$P_transplastic= val()
  itk$P_surfouvre1= val()
  itk$P_julouvre2= val()
  itk$P_surfouvre2= val()
  itk$P_julouvre3= val()
  itk$P_surfouvre3= val()
  itk$P_codeDST= val()
  itk$P_dachisel= val()
  itk$P_dalabour= val()
  itk$P_rugochisel= val()
  itk$P_rugolabour= val()
  itk$P_codeDSTtass= val()
  itk$P_profhumsemoir= val()
  itk$P_dasemis= val()
  itk$P_profhumrecolteuse= val()
  itk$P_darecolte= val()
  itk$P_codeDSTnbcouche= val()

  # Transform into numeric:
  itk_out= suppressWarnings(lapply(itk, as.numeric))
  # Two parameters are not numeric, resetting them
  # to their original value:
  itk_out$P_stadecoupedf= itk$P_stadecoupedf
  itk_out$P_ressuite= itk$P_ressuite

  invisible(itk_out)
}
