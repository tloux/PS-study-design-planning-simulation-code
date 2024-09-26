
# load packages, data ====

load('run-weight-sims-parallel.RData')

dim(sim_list[[1]])
head(sim_list[[1]])

sapply(sim_list, function(x) dim(x))
