# script for comparing execution time (benchmarking)
library("microbenchmark")
library("SticsOnR")
library("Classes")

iniwd=getwd()

wd="/home/plecharpent/Work/projet_tests_modulostics/Data"

javas="/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v131-stics-v841"
times = 10
###############################################
model_src="/home/plecharpent/Work/projet_modulostics_tagv841/stics/Debug/Stics"
model="Stics1_tagv841"
model_dest=file.path(javas,'bin',model)
file.copy(model_src,model_dest,overwrite = TRUE)
data_rep1="example_tagv841"
set_java_model(javas,model)
ws1=file.path(wd,data_rep1)
microbench1 <- microbenchmark(run_usm(javas,ws1,"ble"),times=times)


###############################################
model_src="/home/plecharpent/Work/projet_modulostics_snow/stics/Debug/Stics"
model="Stics2_snow"
model_dest=file.path(javas,'bin',model)
file.copy(model_src,model_dest,overwrite = TRUE)
data_rep2="example_snow_station_ini"
# add_java_model(javas,java_model_tag = model,java_model_exe = model)
set_java_model(javas,model)
ws2=file.path(wd,data_rep2)
microbench2 <- microbenchmark(run_usm(javas,ws2,c("ble")),times=times)



#########################################
# plotting results as boxplot, autoplot
par(mfrow=(c(1,2)))
ymin=min(c(microbench1$time,microbench2$time)/1000000000)
ymax=max(c(microbench1$time,microbench2$time)/1000000000)
ylim=c(ymin,ymax)

boxplot(microbench1,xlab ="Stics_tagv841",log=FALSE,unit="s",ylim = ylim)
boxplot(microbench2,xlab="Stics_snow",log=FALSE,unit="s",ylim = ylim)

##################################
# defining a function to compare stats
# over 2 microbenchmark results
#
mean_rel_diff=(mean(microbench2$time) - mean(microbench1$time))*100/1000000000
max_rel_diff=(max(microbench2$time) - max(microbench1$time))*100/1000000000
min_rel_diff=(min(microbench2$time) - min(microbench1$time))*100/1000000000

# fix a tolerance for degradation !

##########################################

setwd(iniwd)

