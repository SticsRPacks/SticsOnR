# script for comparing execution time (benchmarking)
library("SticsOnR")
library("microbenchmark")

iniwd=getwd()

wd="/home/plecharpent/Work/projet_tests_modulostics/Data"

times = 1000
###############################################
model_src="/home/plecharpent/Work/projet_modulostics_tagv841/stics/Debug/Stics"
data_rep1="example_tagv841"
microbench1 <- microbenchmark(run_system(model_src,wd,data_rep1),times=times)

###############################################
#model_src2="/home/plecharpent/Work/projet_modulostics_v90_test_sticsfiles/stics/Debug/Stics"
#data_rep2="example_v90"
#microbench2 <- microbenchmark(run_system(model_src2,wd,data_rep2),times=times)


model_src3="/home/plecharpent/Work/projet_modulostics_v90/stics/Debug/Stics"
data_rep3="example_v90"
microbench3 <- microbenchmark(run_system(model_src3,wd,data_rep3),times=times)


#########################################
# plotting results as boxplot, autoplot
par(mfrow=(c(1,2)))
ymin=min(c(microbench1$time,microbench3$time)/1000000000)
ymax=max(c(microbench1$time,microbench3$time)/1000000000)
ylim=c(ymin,ymax)

boxplot(microbench1,xlab ="Stics_tagv841",log=FALSE,unit="s",ylim = ylim)
#boxplot(microbench2,xlab="Stics_v90_test_sticsfiles",log=FALSE,unit="s",ylim = ylim)

boxplot(microbench3,xlab="Stics_v90",log=FALSE,unit="s",ylim = ylim)

##################################
# defining a function to compare stats
# over 2 microbenchmark results
#
mean_rel_diff=(mean(microbench2$time) - mean(microbench3$time))*100/1000000000
max_rel_diff=(max(microbench2$time) - max(microbench3$time))*100/1000000000
min_rel_diff=(min(microbench2$time) - min(microbench3$time))*100/1000000000

# fix a tolerance for degradation !

##########################################

setwd(iniwd)

