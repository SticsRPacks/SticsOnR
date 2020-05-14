library(SticsOnR)
library(SticsRFiles)
library(microbenchmark)
library(ggplot2)
library(SticsOptimizR)

# Using stics_wrapper

# path to root directory containing subfolders (one for each situation)
data_dir <- "/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example_test"
# path to the Stics executable (according to the OS)
# here for linux, and the official standard version
javastics_path <- "/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0"

# getting obs data
obs <- read_obs_to_list(data_dir)

# generating stics_options list with time display and no parallel calculation
stics_options_no_par <- stics_wrapper_options(javastics_path = javastics_path,
                                              data_dir = data_dir,
                                              time_display = TRUE)

# generating stics_options list with time display and parallel calculation
# (without specifying cores number)
stics_options_par <- stics_wrapper_options(javastics_path = javastics_path,
                                           data_dir = data_dir,
                                           time_display = TRUE,
                                           parallel = TRUE)


# running stics_wrapper
# With selecting outputs against observations (i.e. situation/var/dates list)
# sequential
res <- stics_wrapper(model_options =  stics_options_no_par, site_var_dates_mask = obs)

# with parallel
res_par <- stics_wrapper(model_options =  stics_options_par, site_var_dates_mask = obs)



# comparing results between // and sequential runs
# benchmarking
mb_with_obs <- microbenchmark::microbenchmark(stics_wrapper(model_options =  stics_options_par, site_var_dates_mask = obs),
                               stics_wrapper(model_options =  stics_options_no_par, site_var_dates_mask = obs),
                               times = 10L)

# mb
# Unit: seconds
#                        expr                                              min        lq      mean
# stics_wrapper(model_options = stics_options_par, site_var_dates_mask = obs)  7.015765  7.080052  7.334173
# stics_wrapper(model_options = stics_options_no_par, site_var_dates_mask = obs) 11.012740 11.130114 11.241341
# median        uq       max neval
# 7.384921  7.546614  7.580808    10
# 11.204739 11.329043 11.596302    10


# plotting results
p_with_obs <- ggplot2::autoplot(mb_with_obs)

p_with_obs



# running stics_wrapper
# Without selecting outputs against observations (i.e. situation/var/dates list)
# sequential
res <- stics_wrapper(model_options =  stics_options_no_par)

# parallel
res_par <- stics_wrapper(model_options =  stics_options_par)


# comparing results between // and sequential runs
# benchmarking
mb_no_obs <- microbenchmark::microbenchmark(stics_wrapper(model_options =  stics_options_par),
                                     stics_wrapper(model_options =  stics_options_no_par),
                                     times = 10L)

# mb_no_obs
# Unit: seconds
# expr                                                 min       lq     mean   median       uq
# stics_wrapper(model_options = stics_options_par)    10.41928 10.74495 10.89209 10.95465 11.07877
# stics_wrapper(model_options = stics_options_no_par) 18.80119 19.09048 19.51086 19.54257 19.81991
# max        neval
# 11.33421    10
# 20.32123    10


# plotting results
p_no_obs <- ggplot2::autoplot(mb_no_obs)

p_no_obs



# Case with using prior_information data
sg=list(p1=list(sit_list=list(c("sit1","sit2","sit3"),c("sit4","sit5","sit6"))),
        p2=list(sit_list=list(c("sit1","sit2","sit3","sit4","sit5","sit6"))))

vec=c(1,2,3)
names(vec)=c("p2","p1","p1")

get_params_per_sit(sg,"sit2",vec)


