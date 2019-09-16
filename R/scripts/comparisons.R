library(dplyr)
library(Classes)
library(SticsOnR)

iniwd=getwd()

wd="/home/plecharpent/Work/projet_tests_modulostics/Data"

javas="/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v140-stics-v850"

#### ATTENTION: PLUS DE COPIE DES EXE, LIEN SYMBOLIQUE FAIT !!!
#### A VOIR POUR WINDOWS: faire copie !!

###############################################
model_src="/home/plecharpent/Work/projet_modulostics/stics/Debug/Stics"
model="Stics_trunk"
#model_dest=file.path(javas,'bin',model)
#file.copy(model_src,model_dest,overwrite = TRUE)
data_rep1="example_trunk"
ws1=file.path(wd,data_rep1)
set_java_model(javas,model)
usms_out1 <- run_java(javas,ws1)

##############################################
model_src="/home/plecharpent/Work/projet_LegoStics/legostics/stics/Debug/Stics"
model="Stics_Lego"
#model_dest=file.path(javas,'bin',model)
#file.copy(model_src,model_dest,overwrite = TRUE)
data_rep2="example_Lego"
ws2=file.path(wd,data_rep2)
set_java_model(javas,model)
usms_out2 <- run_java(javas,ws2)

#################################################
model_src="/home/plecharpent/Work/projet_modulostics_tagv841/stics/Debug/Stics"
model="Stics_tagv841"
#model_dest=file.path(javas,'bin',model)
#file.copy(model_src,model_dest,overwrite = TRUE)
data_rep3="example_tagv841"
set_java_model(javas,model)
ws3=file.path(wd,data_rep3)
usms_out3 <- run_java(javas,ws3)

#####################################################
model_src="/home/plecharpent/Work/projet_modulostics_microclimate/stics/Debug/Stics"
model="Stics_microc"
#model_dest=file.path(javas,'bin',model)
#file.copy(model_src,model_dest,overwrite = TRUE)
data_rep4="example_microc"
set_java_model(javas,model)
ws4=file.path(wd,data_rep4)
usms_out4 <- run_java(javas,ws4)

#####################################################
model_src="/home/plecharpent/Work/projet_modulostics_radiative_transfert/stics/Debug/Stics"
model="Stics_rad_trans"
#model_dest=file.path(javas,'bin',model)
#file.copy(model_src,model_dest,overwrite = TRUE)
data_rep5="example_radiative_transfert"
set_java_model(javas,model)
ws5=file.path(wd,data_rep5)
usms_out5 <- run_java(javas,ws5)


######################################################
model_src="/home/plecharpent/Work/projet_modulostics_radiative_transfert/stics/Debug/Stics"
model="Stics_interc"
# #model_dest=file.path(javas,'bin',model)
# #file.copy(model_src,model_dest,overwrite = TRUE)
data_rep6="example_interc"
set_java_model(javas,model)
ws6=file.path(wd,data_rep6)
usms_out6 <- run_java(javas,ws6)


######################################################
model="Stics_v90_bis"
data_rep7="example_v90_bis"
add_java_model(javas,model,model)
set_java_model(javas,model)
ws7=file.path(wd,data_rep7)
usms_out7 <- run_java(javas,ws7)

######################################################
model="Stics_snow"
data_rep8="example_snow"
add_java_model(javas,model,model)
set_java_model(javas,model)
ws8=file.path(wd,data_rep8)
usms_out8 <- run_java(javas,ws8)


######################################################
model="Stics_v90"
data_rep9="example_v90"
add_java_model(javas,"Stics_v90","Stics_v90")
set_java_model(javas,model)
ws9=file.path(wd,data_rep9)
usms_out9 <- run_java(javas,ws9)

######################################################
model="Stics_trunk"
data_rep10="example_trunk"
add_java_model(javas,model,model)
set_java_model(javas,model)
ws10=file.path(wd,data_rep10)
usms_out10 <- run_java(javas,ws10)


# trunk vs tagv841
usms_to_comp=base::intersect(usms_out2$names,usms_out3$names)
trunk_vs_841 = compare_2usms(ws2,ws3,usms_to_comp)
ggplot(mat_long[1:660,], aes(x = Var2, y = Var1)) + geom_raster(aes(fill=value)) +
  labs(x="VARIABLES", y="USMs", title="Correlation of daily variables trunk vs v841, plot 1") +
  scale_fill_gradient(low="red", high="grey90")
ggplot(mat_long[661:1320,], aes(x = Var2, y = Var1)) + geom_raster(aes(fill=value)) +
  labs(x="VARIABLES", y="USMs", title="Correlation of daily variables trunk vs v841, plot 2") +
  scale_fill_gradient(low="red", high="grey90")

# trunk vs lego
usms_to_comp=base::intersect(usms_out1$names,usms_out2$names)
trunk_vs_lego = compare_2usms(ws1,ws2,usms_to_comp)

# trunk vs microc
usms_to_comp=base::intersect(usms_out1$names,usms_out4$names)
trunk_vs_microc = compare_2usms(ws1,ws4,usms_to_comp)

# trunk vs rad trans
usms_to_comp=base::intersect(usms_out1$names,usms_out5$names)
trunk_vs_radtrans = compare_2usms(ws1,ws5,usms_to_comp)

# v90 vs v90_bis
usms_to_comp=base::intersect(usms_out7$names,usms_out9$names)
v90_vs_90bis = compare_2usms(ws7,ws9,usms_to_comp)

# snow vs v90_bis
usms_to_comp=base::intersect(usms_out8$names,usms_out9$names)
snow_vs_90bis = compare_2usms(ws8,ws9,usms_to_comp)

# v90 vs trunk
usms_to_comp=base::intersect(usms_out10$names,usms_out9$names)
v90_vs_90bis = compare_2usms(ws10,ws9,usms_to_comp)


setwd(iniwd)
