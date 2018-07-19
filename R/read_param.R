#' Read STICS input parameters
#'
#' @description Read STICS model input parameters from a pre-existing STICS input
#'              file. Generally used after calling \code{\link{set_usm}}.
#'
#' @param dirpath      Directory path
#' @param filepath     File path
#' @param param        Parameter name. Optional, if not provided, the function
#'                     return an object with all parameters
#' @param several_fert Is there several fertilization in the USM ?
#' @param several_thin Is there several thinning in the USM ?
#' @param is_pasture   Is the plant a pasture ?
#' @param max_variety  Maximum number of variety authorized (this is only for STICS
#'                     compatibility)
#' @param ...          Helper to pass arguments from \code{\link{read_param}} to the
#'                     other functions
#'
#' @note Users generally only use \code{read_param} that identify parameters for
#'       other functions and call them.
#'
#' @return A list of all parameters:
#'         \item{ini}{Initialization parameters}
#'         \item{general}{General parameters}
#'         \item{tec}{Technical parameters}
#'         \item{plant}{Plant parameters}
#'         \item{soil}{Soil parameters}
#'         \item{station}{Station parameters}
#'         \code{output}{The output variables the user require from STICS}
#' The function can return several sub-lists for each \code{plant} and \code{tec}
#'  if mixed crops, numbered by usage
#'
#' @seealso \code{\link{set_param}}.
#'
#' @importFrom stats setNames
#'
#' @examples
#'\dontrun{
#' # Read the interrow distance parameter:
#'
#' library(sticRs)
#' read_param(param='interrang')
#'
#'}
#'
#' @export
#'
read_param= function(dirpath=getwd(),param=NULL,...){
  # Make this function read all parameters from STICS automatically
  # 1. Detect how much plants there are
  # 2. Read tec+plant files
  # 3. Return the output as a list
  dot_args= list(...)
  if(isTRUE(dot_args$several_fert)){
    several_fert= dot_args$several_fert
  }else{
    several_fert= F
  }
  if(isTRUE(dot_args$several_thin)){
    several_thin= dot_args$several_thin
  }else{
    several_thin= F
  }
  if(isTRUE(dot_args$is_pasture)){
    is_pasture= dot_args$is_pasture
  }else{
    is_pasture= F
  }
  if(isTRUE(dot_args$max_variety)){
    max_variety= dot_args$max_variety
  }else{
    max_variety= 30
  }

  ini= read_ini(file.path(dirpath,"ficini.txt"))
  general= read_general(file.path(dirpath,"tempopar.sti"))
  soil= read_soil(file.path(dirpath,"param.sol"))
  station= read_station(file.path(dirpath,"station.txt"))
  usm= read_usm(file.path(dirpath,"new_travail.usm"))
  output= read_out_var(file.path(dirpath,"var.mod"))
  tmp= read_tmp(file.path(dirpath,"tempoparV6.sti"))
  tec= plant= setNames(vector(mode = "list", length = ini$nbplantes),
                        paste0("plant",1:ini$nbplantes))
  for(i in seq_len(ini$nbplantes)){
    tec[paste0("plant",i)]=
      list(read_tec(file.path(dirpath,paste0("fictec",i,".txt")),
               several_fert = several_fert, several_thin = several_thin,
               is_pasture = is_pasture))
    plant[paste0("plant",i)]=
      list(read_plant(file.path(dirpath,paste0("ficplt",i,".txt")),
                      max_variety = max_variety))
  }

  parameters= list(usm= usm, ini= ini, general= general, tec= tec,
                   plant= plant, soil= soil, station= station,
                   output= output,tmp=tmp)

  if(!is.null(param)){
    parameters= unlist(parameters)
    parameters= parameters[grep(param,names(parameters))]
  }
  return(parameters)
}

#' @rdname read_param
#' @export
read_ini= function(filepath="ficini.txt"){
  params= readLines(filepath)
  ini= vector(mode='list', length = 0)
  values= params[!seq_along(params)%%2]

  index= 1
  val= function(){
    index<<- index+1
    return(values[index-1])
  }

  ini$nbplantes= as.numeric(val())
  return(ini)
}

#' @rdname read_param
#' @export
read_general= function(filepath="tempopar.sti"){

  params= readLines(filepath)
  pg= vector(mode='list', length = 0)
  values= params[!seq_along(params)%%2]

  index= 1
  val= function(){
    index<<- index+1
    return(values[index-1])
  }

  pg$nbresidus= 21 # added at import, not in the file

  pg$P_codeinnact= val()
  pg$P_codeh2oact= val()
  pg$P_codeminopt= val()
  pg$P_iniprofil= val()
  pg$P_codeprofmes= val()
  pg$P_codeinitprec= val()
  pg$P_codemsfinal= val()
  pg$P_codeactimulch= val()
  pg$P_codefrmur= val()
  pg$P_codemicheur= val()
  pg$P_codeoutscient= val()
  pg$P_codeseprapport= val()
  pg$P_separateurrapport= val()
  pg$P_codesensibilite= val()
  pg$P_flagEcriture= val()
  pg$P_parsurrg= val()
  pg$P_coefb= val()
  pg$P_proprac= val()
  pg$P_y0msrac= val()
  pg$P_khaut= val()
  pg$P_dacohes= val()
  pg$P_daseuilbas= val()
  pg$P_daseuilhaut= val()
  pg$P_beta= val()
  pg$P_lvopt= val()
  pg$P_rayon= val()
  pg$P_difN= val()
  pg$P_concrr= val()
  pg$P_plNmin= val()
  pg$P_irrlev= val()
  pg$P_QNpltminINN= val()
  pg$P_codesymbiose= val()
  pg$P_codefxn= val()
  pg$P_FTEMh= val()
  pg$P_FTEMha= val()
  pg$P_TREFh= val()
  pg$P_FTEMr= val()
  pg$P_FTEMra= val()
  pg$P_trefr= val()
  pg$P_finert= val()
  pg$P_fmin1= val()
  pg$P_fmin2= val()
  pg$P_fmin3= val()
  pg$P_Wh= val()
  pg$P_pHminvol= val()
  pg$P_pHmaxvol= val()
  pg$P_Vabs2= val()
  pg$P_Xorgmax= val()
  pg$P_hminm= val()
  pg$P_hoptm= val()
  pg$P_alphapH= val()
  pg$P_dpHvolmax= val()
  pg$P_pHvols= val()
  pg$P_fhminsat= val()
  pg$P_fredkN= val()
  pg$P_fredlN= val()
  pg$P_fNCBiomin= val()
  pg$P_fredNsup= val()
  pg$P_Primingmax= val()
  pg$P_hminn= val()
  pg$P_hoptn= val()
  pg$P_pHminnit= val()
  pg$P_pHmaxnit= val()
  pg$P_nh4_min= val()
  pg$P_pHminden= val()
  pg$P_pHmaxden= val()
  pg$P_wfpsc= val()
  pg$P_tdenitopt_gauss= val()
  pg$P_scale_tdenitopt= val()
  pg$P_Kd= val()
  pg$P_kdesat= val()
  pg$P_code_vnit= val()
  pg$P_fnx= val()
  pg$P_vnitmax= val()
  pg$P_Kamm= val()
  pg$P_code_tnit= val()
  pg$P_tnitmin= val()
  pg$P_tnitopt= val()
  pg$P_tnitopt2= val()
  pg$P_tnitmax= val()
  pg$P_tnitopt_gauss= val()
  pg$P_scale_tnitopt= val()
  pg$P_code_rationit= val()
  pg$P_rationit= val()
  pg$P_code_hourly_wfps_nit= val()
  pg$P_code_pdenit= val()
  pg$P_cmin_pdenit= val()
  pg$P_cmax_pdenit= val()
  pg$P_min_pdenit= val()
  pg$P_max_pdenit= val()
  pg$P_code_ratiodenit= val()
  pg$P_ratiodenit= val()
  pg$P_code_hourly_wfps_denit= val()
  pg$P_pminruis= val()
  pg$P_diftherm= val()
  pg$P_Bformnappe= val()
  pg$P_rdrain= val()
  pg$P_psihumin= val()
  pg$P_psihucc= val()
  pg$P_prophumtasssem= val()
  pg$P_prophumtassrec= val()
  pg$P_codhnappe= val()
  pg$P_distdrain= val()
  pg$P_proflabour= val()
  pg$P_proftravmin= val()
  pg$P_codetycailloux= val()

  for(i in 1:10){
    pg$P_masvolcx= c(pg$P_masvolcx,val())
    pg$P_hcccx= c(pg$P_hcccx,val())
  }

  pg$P_codetypeng= val()

  for(i in 1:8){
    pg$P_engamm= c(pg$P_engamm,val())
    pg$P_orgeng= c(pg$P_orgeng,val())
    pg$P_deneng= c(pg$P_deneng,val())
    pg$P_voleng= c(pg$P_voleng,val())
  }

  pg$P_codetypres= val()

  for(i in 1:pg$nbresidus){
    pg$P_CroCo= c(pg$P_CroCo,val())
    pg$P_akres= c(pg$P_akres,val())
    pg$P_bkres= c(pg$P_bkres,val())
    pg$P_awb= c(pg$P_awb,val())
    pg$P_bwb= c(pg$P_bwb,val())
    pg$P_cwb= c(pg$P_cwb,val())
    pg$P_ahres= c(pg$P_ahres,val())
    pg$P_bhres= c(pg$P_bhres,val())
    pg$P_kbio= c(pg$P_kbio,val())
    pg$P_yres= c(pg$P_yres,val())
    pg$P_CNresmin= c(pg$P_CNresmin,val())
    pg$P_cnresmax= c(pg$P_cnresmax,val())
    pg$P_qmulchruis0= c(pg$P_qmulchruis0,val())
    pg$P_mouillabilmulch= c(pg$P_mouillabilmulch,val())
    pg$P_kcouvmlch= c(pg$P_kcouvmlch,val())
    pg$P_albedomulchresidus= c(pg$P_albedomulchresidus,val())
    pg$P_Qmulchdec= c(pg$P_Qmulchdec,val())
  }

  # Transform into numeric:
  pg_out= suppressWarnings(lapply(pg, as.numeric))
  pg_out$P_separateurrapport= pg$P_separateurrapport

  return(pg_out)
}

#' @rdname read_param
#' @export
read_tmp= function(filepath="tempoparv6.sti"){
  params= readLines(filepath)
  p_tmp= vector(mode='list', length = 0)
  p_tmp= as.list(params[seq_along(params)%%2==0])
  names(p_tmp)= params[seq_along(params)%%2==1]

  return(p_tmp)
}

#' @rdname read_param
#' @export
read_plant= function(filepath="ficplt1.txt",max_variety=30){

  params= readLines(filepath)
  plant= vector(mode='list', length = 0)
  values= params[!seq_along(params)%%2]

  index= 1
  val= function(){
    index<<- index+1
    return(values[index-1])
  }

  plant$P_codeplante= val()
  plant$P_codemonocot= val()
  plant$P_alphaco2= val()
  plant$P_tdmin= val()
  plant$P_tdmax= val()
  plant$P_codetemp= val()
  plant$P_codegdh= val()
  plant$P_coeflevamf= val()
  plant$P_coefamflax= val()
  plant$P_coeflaxsen= val()
  plant$P_coefsenlan= val()
  plant$P_coeflevdrp= val()
  plant$P_coefdrpmat= val()
  plant$P_coefflodrp= val()
  plant$P_codephot= val()
  plant$P_phobase= val()
  plant$P_phosat= val()
  plant$P_coderetflo= val()
  plant$P_stressdev= val()
  plant$P_codebfroid= val()
  plant$P_jvcmini= val()
  plant$P_julvernal= val()
  plant$P_tfroid= val()
  plant$P_ampfroid= val()
  plant$P_stdordebour= val()
  plant$P_tdmindeb= val()
  plant$P_tdmaxdeb= val()
  plant$P_codedormance= val()
  plant$P_ifindorm= val()
  plant$P_q10= val()
  plant$P_idebdorm= val()
  plant$P_codegdhdeb= val()
  plant$P_codeperenne= val()
  plant$P_codegermin= val()
  plant$P_tgmin= val()
  plant$P_stpltger= val()
  plant$P_potgermi= val()
  plant$P_nbjgerlim= val()
  plant$P_propjgermin= val()
  plant$P_codehypo= val()
  plant$P_belong= val()
  plant$P_celong= val()
  plant$P_elmax= val()
  plant$P_nlevlim1= val()
  plant$P_nlevlim2= val()
  plant$P_vigueurbat= val()
  plant$P_laiplantule= val()
  plant$P_nbfeuilplant= val()
  plant$P_masecplantule= val()
  plant$P_zracplantule= val()
  plant$P_phyllotherme= val()
  plant$P_bdens= val()
  plant$P_laicomp= val()
  plant$P_hautbase= val()
  plant$P_hautmax= val()
  plant$P_tcxstop= val()
  plant$P_codelaitr= val()
  plant$P_vlaimax= val()
  plant$P_pentlaimax= val()
  plant$P_udlaimax= val()
  plant$P_ratiodurvieI= val()
  plant$P_tcmin= val()
  plant$P_tcmax= val()
  plant$P_ratiosen= val()
  plant$P_abscission= val()
  plant$P_parazofmorte= val()
  plant$P_innturgmin= val()
  plant$P_dlaimin= val()
  plant$P_codlainet= val()
  plant$P_dlaimax= val()
  plant$P_tustressmin= val()
  plant$P_dlaimaxbrut= val()
  plant$P_durviesupmax= val()
  plant$P_innsen= val()
  plant$P_rapsenturg= val()
  plant$P_codestrphot= val()
  plant$P_phobasesen= val()
  plant$P_dltamsmaxsen= val()
  plant$P_dltamsminsen= val()
  plant$P_alphaphot= val()
  plant$P_tauxrecouvmax= val()
  plant$P_tauxrecouvkmax= val()
  plant$P_pentrecouv= val()
  plant$P_infrecouv= val()
  plant$P_codetransrad= val()
  plant$P_extin= val()
  plant$P_ktrou= val()
  plant$P_forme= val()
  plant$P_rapforme= val()
  plant$P_adfol= val()
  plant$P_dfolbas= val()
  plant$P_dfolhaut= val()
  plant$P_temin= val()
  plant$P_temax= val()
  plant$P_teopt= val()
  plant$P_teoptbis= val()
  plant$P_efcroijuv= val()
  plant$P_efcroiveg= val()
  plant$P_efcroirepro= val()
  plant$P_remobres= val()
  plant$P_coefmshaut= val()
  plant$P_slamax= val()
  plant$P_slamin= val()
  plant$P_tigefeuil= val()
  plant$P_envfruit= val()
  plant$P_sea= val()
  plant$P_codeindetermin= val()
  plant$P_nbjgrain= val()
  plant$P_cgrain= val()
  plant$P_cgrainv0= val()
  plant$P_nbgrmin= val()
  plant$P_codeir= val()
  plant$P_vitircarb= val()
  plant$P_irmax= val()
  plant$P_vitircarbT= val()
  plant$P_nboite= val()
  plant$P_allocfrmax= val()
  plant$P_afpf= val()
  plant$P_bfpf= val()
  plant$P_cfpf= val()
  plant$P_dfpf= val()
  plant$P_stdrpnou= val()
  plant$P_spfrmin= val()
  plant$P_spfrmax= val()
  plant$P_splaimin= val()
  plant$P_splaimax= val()
  plant$P_codcalinflo= val()
  plant$P_nbinflo= val()
  plant$P_inflomax= val()
  plant$P_pentinflores= val()
  plant$P_codetremp= val()
  plant$P_tminremp= val()
  plant$P_tmaxremp= val()
  plant$P_vitpropsucre= val()
  plant$P_vitprophuile= val()
  plant$P_vitirazo= val()
  plant$P_sensanox= val()
  plant$P_stoprac= val()
  plant$P_sensrsec= val()
  plant$P_contrdamax= val()
  plant$P_codetemprac= val()
  plant$P_coderacine= val()
  plant$P_zlabour= val()
  plant$P_zpente= val()
  plant$P_zprlim= val()
  plant$P_draclong= val()
  plant$P_debsenrac= val()
  plant$P_lvfront= val()
  plant$P_longsperac= val()
  plant$P_codazorac= val()
  plant$P_minefnra= val()
  plant$P_minazorac= val()
  plant$P_maxazorac= val()
  plant$P_codtrophrac= val()
  plant$P_repracpermax= val()
  plant$P_repracpermin= val()
  plant$P_krepracperm= val()
  plant$P_repracseumax= val()
  plant$P_repracseumin= val()
  plant$P_krepracseu= val()
  plant$P_tletale= val()
  plant$P_tdebgel= val()
  plant$P_codgellev= val()
  plant$P_nbfgellev= val()
  plant$P_tgellev10= val()
  plant$P_tgellev90= val()
  plant$P_codgeljuv= val()
  plant$P_tgeljuv10= val()
  plant$P_tgeljuv90= val()
  plant$P_codgelveg= val()
  plant$P_tgelveg10= val()
  plant$P_tgelveg90= val()
  plant$P_codgelflo= val()
  plant$P_tgelflo10= val()
  plant$P_tgelflo90= val()
  plant$P_psisto= val()
  plant$P_psiturg= val()
  plant$P_h2ofeuilverte= val()
  plant$P_h2ofeuiljaune= val()
  plant$P_h2otigestruc= val()
  plant$P_h2oreserve= val()
  plant$P_h2ofrvert= val()
  plant$P_deshydbase= val()
  plant$P_tempdeshyd= val()
  plant$P_codebeso= val()
  plant$P_kmax= val()
  plant$P_rsmin= val()
  plant$P_codeintercept= val()
  plant$P_mouillabil= val()
  plant$P_stemflowmax= val()
  plant$P_kstemflow= val()
  plant$P_Vmax1= val()
  plant$P_Kmabs1= val()
  plant$P_Vmax2= val()
  plant$P_Kmabs2= val()
  plant$P_adil= val()
  plant$P_bdil= val()
  plant$P_masecNmax= val()
  plant$P_INNmin= val()
  plant$P_INNimin= val()
  plant$P_inngrain1= val()
  plant$P_inngrain2= val()
  plant$P_codeplisoleN= val()
  plant$P_adilmax= val()
  plant$P_bdilmax= val()
  plant$P_Nmeta= val()
  plant$P_masecmeta= val()
  plant$P_Nreserve= val()
  plant$P_codeINN= val()
  plant$P_codelegume= val()
  plant$P_stlevdno= val()
  plant$P_stdnofno= val()
  plant$P_stfnofvino= val()
  plant$P_vitno= val()
  plant$P_profnod= val()
  plant$P_concNnodseuil= val()
  plant$P_concNrac0= val()
  plant$P_concNrac100= val()
  plant$P_tempnod1= val()
  plant$P_tempnod2= val()
  plant$P_tempnod3= val()
  plant$P_tempnod4= val()
  plant$P_codefixpot= val()
  plant$P_fixmax= val()
  plant$P_fixmaxveg= val()
  plant$P_fixmaxgr= val()
  plant$P_codazofruit= val()
  plant$P_stadebbchplt= val()
  plant$P_stadebbchger= val()
  plant$P_stadebbchlev= val()
  plant$P_stadebbchamf= val()
  plant$P_stadebbchlax= val()
  plant$P_stadebbchsen= val()
  plant$P_stadebbchflo= val()
  plant$P_stadebbchdrp= val()
  plant$P_stadebbchnou= val()
  plant$P_stadebbchdebdes= val()
  plant$P_stadebbchmat= val()
  plant$P_stadebbchrec= val()
  plant$P_stadebbchfindorm= val()

  read_variety= function(p_list){

    p_list$P_codevar= c(p_list$P_codevar,val())
    p_list$P_stlevamf= c(p_list$P_stlevamf,val())
    p_list$P_stamflax= c(p_list$P_stamflax,val())
    p_list$P_stlevdrp= c(p_list$P_stlevdrp,val())
    p_list$P_stflodrp= c(p_list$P_stflodrp,val())
    p_list$P_stdrpdes= c(p_list$P_stdrpdes,val())
    p_list$P_pgrainmaxi= c(p_list$P_pgrainmaxi,val())
    p_list$P_adens= c(p_list$P_adens,val())
    p_list$P_croirac= c(p_list$P_croirac,val())
    p_list$P_durvieF= c(p_list$P_durvieF,val())
    p_list$P_jvc= c(p_list$P_jvc,val())
    p_list$P_sensiphot= c(p_list$P_sensiphot,val())
    p_list$P_stlaxsen= c(p_list$P_stlaxsen,val())
    p_list$P_stsenlan= c(p_list$P_stsenlan,val())
    p_list$P_nbgrmax= c(p_list$P_nbgrmax,val())
    p_list$P_stdrpmat= c(p_list$P_stdrpmat,val())
    p_list$P_afruitpot= c(p_list$P_afruitpot,val())
    p_list$P_dureefruit= c(p_list$P_dureefruit,val())
    return(p_list)
  }
  tmp= vector(mode='list', length = 0)

  # could use while but STICS uses a for loop
  for(i in 1:max_variety){
    err= read_variety(tmp)
    if(!any(is.na(err$P_codevar))){tmp= err}else{break}
  }

  plant= c(plant,tmp)
  if(i==max_variety){
    plant$nbVariete = i
  }else{
    plant$nbVariete = i-1
  }


  # Transform into numeric:
  plant_out= suppressWarnings(lapply(plant, as.numeric))
  # Two parameters are not numeric, resetting them
  # to their original value:
  plant_out$P_codevar= plant$P_codevar
  plant_out$P_stoprac= plant$P_stoprac
  plant_out$P_codeplante= plant$P_codeplante

  return(plant_out)
}


#' @rdname read_param
#' @export
read_tec= function(filepath="fictec1.txt",several_fert=T,several_thin=T,is_pasture=F){

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

  return(itk_out)
}


#' @rdname read_param
#' @export
read_soil= function(filepath= "param.sol"){

  params= readLines(filepath,warn=F)
  soil= vector(mode='list', length = 0)

  index= 1
  val= function(){
    index<<- index+1
    vec= strsplit(x = params[index-1],split = " ")[[1]]
    vec= vec[vec!=""]
    return(vec)
  }

  soil$nbcouchessol_max= 1000

  soil[c("P_numsol","P_typsol","P_argi","P_Norg","P_profhum","P_calc","P_pH",
         "P_concseuil","P_albedo","P_q0","P_ruisolnu","P_obstarac","P_pluiebat",
         "P_mulchbat","P_zesx","P_cfes","P_z0solnu","P_CsurNsol", "P_penterui")]= val()

  soil[c("P_numsol","P_codecailloux","P_codemacropor","P_codefente",
         "P_codrainage","P_coderemontcap","P_codenitrif","P_codedenit")]= val()

  soil[c("P_numsol","P_profimper","P_ecartdrain","P_ksol","P_profdrain",
         "P_capiljour","P_humcapil","P_profdenit","P_vpotdenit")]= val()

  vec= matrix(data = NA,nrow = 9, ncol = 5)
  for(i in 1:5){
    vec[,i]= val()
  }
  vec= apply(vec,MARGIN = 1,FUN = list)

  soil[c("P_numsol","P_epc","P_hccf","P_hminf","P_DAF",
         "P_cailloux","P_typecailloux","P_infil","P_epd")]=
    lapply(vec, unlist)

  # Transform into numeric:
  soil_out= suppressWarnings(lapply(soil, as.numeric))
  soil_out$P_typsol= soil$P_typsol

  return(soil_out)
}

#' @rdname read_param
#' @export
read_station= function(filepath="station.txt"){

  params= readLines(filepath)
  sta= vector(mode='list', length = 0)
  values= params[!seq_along(params)%%2]

  index= 1
  val= function(){
    index<<- index+1
    return(values[index-1])
  }

  sta$P_zr= val()
  sta$P_NH3ref= val()
  sta$P_latitude= val()
  sta$P_patm= val()
  sta$P_aclim= val()
  sta$P_codeetp= val()
  sta$P_alphapt= val()
  sta$P_codeclichange= val()
  sta$P_codaltitude= val()
  sta$P_altistation= val()
  sta$P_altisimul= val()
  sta$P_gradtn= val()
  sta$P_gradtx= val()
  sta$P_altinversion= val()
  sta$P_gradtninv= val()
  sta$P_cielclair= val()
  sta$P_codadret= val()
  sta$P_ombragetx= val()
  sta$P_ra= val()
  sta$P_albveg= val()
  sta$P_aangst= val()
  sta$P_bangst= val()
  sta$P_corecTrosee= val()
  sta$P_codecaltemp= val()
  sta$P_codernet= val()
  sta$P_coefdevil= val()
  sta$P_aks= val()
  sta$P_bks= val()
  sta$P_cvent= val()
  sta$P_phiv0= val()
  sta$P_coefrnet= val()

  # Transform into numeric:
  sta_out= suppressWarnings(lapply(sta, as.numeric))
  # sta_out$P_separateurrapport= sta$P_separateurrapport
  return(sta_out)
}


#' @rdname read_param
#' @export
read_usm= function(filepath="new_travail.usm"){
  params= readLines(filepath)
  usm= vector(mode='list', length = 0)
  values= params[!seq_along(params)%%2]

  index= 1
  val= function(){
    index<<- index+1
    return(values[index-1])
  }

  usm$P_codesimul= val()
  usm$codoptim= val()
  usm$P_codesuite= val()
  usm$P_nbplantes= val()
  usm$P_nomSimulation= val()
  usm$P_iwater= val()
  usm$P_ifwater= val()
  usm$P_ficInit= val()
  usm$P_ichsl= val()
  usm$P_nomsol= val()
  usm$P_ficStation= val()
  usm$P_wdata1= val()
  usm$P_wdata2= val()
  usm$nbans= val()
  usm$P_culturean= val()

  for(i in 1:usm$P_nbplantes){
    usm$P_fplt[i]= val()
    usm$P_ftec[i]= val()
    usm$P_flai[i]= val()
  }
  return(usm)
}


#' @rdname read_param
#' @export
read_out_var= function(filepath="var.mod"){
  Vars= readLines(filepath)
  return(Vars)
}
