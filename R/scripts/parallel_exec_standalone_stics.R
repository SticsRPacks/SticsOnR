#! /usr/bin/Rscript
library("SticsOnR")
# arguments
# 1 : model executable path
#model_src="/home/plecharpent/Work/projet_modulostics/stics/Debug/Stics"
# 2 : working directory or root of working directories
#wd="/home/plecharpent/Work/projet_tests_modulostics/Data/workspace_parallel_exec_standalone/test_simulations_1"
# 3 : path of a file containing the names of working directories inside the root path (given as #2 argument)
#usms_dirs_file="/home/plecharpent/Work/workspace/Example_Stics_Tests/R/SticsOnR/R/scripts/data/usms_dirs_relative.txt"

args = commandArgs(trailingOnly = TRUE)

print(args)
nargs = length(args)


# exiting after arg test
if (nargs <2) {
  stop("Missing mandatory arguments : #1 model executable path, #2 usms data directory root")
}

model_src=args[1]
wd=args[2]

# ajout d'arguments : stics dir, usm file path list
# verif
if (nargs < 3) {
  data_dirs=list.dirs(wd,recursive = FALSE,full.names=FALSE)
  # print(data_dirs)
  #stop()
} else {
  usms_dirs_file=args[3]
  # getting dir list
  # print(args[3])
  f=file(usms_dirs_file)
  data_dirs=readLines(f)
  close(f)
}





# script for comparing execution time (benchmarking)
library("doParallel")

iniwd=getwd()



# simple_ptime <- system.time({
# # simple run
# for (d in data_dirs){
#   run_system(model_src,wd,d)
# }
# })

# parallel run
cores_number=4
cl <- parallel::makeCluster(cores_number)
registerDoParallel(cl)
nb_dirs=length(data_dirs)
parallel_ptime <- system.time({
foreach(d=1:nb_dirs) %dopar% SticsOnR::run_system(model_src,wd,data_dirs[d])
})
stopCluster(cl)

setwd(iniwd)

# writing perf stats to a file
sts=file(file.path(wd,"system_time_stats.txt"),"w")
writeLines("simple",con = sts)
writeLines(paste(names(simple_ptime),collapse="\t"),con = sts)
writeLines(paste(as.character(simple_ptime),collapse="\t\t"),con = sts)
writeLines(paste0("parallel (",as.character(cores_number)," cores)"),con = sts)
writeLines(paste(names(parallel_ptime),collapse="\t"),con = sts)
writeLines(paste(as.character(parallel_ptime),collapse="\t\t"),con = sts)
close(sts)
