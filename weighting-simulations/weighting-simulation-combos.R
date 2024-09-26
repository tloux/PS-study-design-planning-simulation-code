
# population parameters

prevs = c(5, 10, 20, 30)
smds = c(20, 50, 80)

# weighting parameters

stabilize = c(FALSE, TRUE)

# combinations

parameter_combos = expand.grid(stabilize, smds, prevs)
names(parameter_combos) = c('stabilize', 'smd', 'prev')

# save

save(file='weighting-simulation-combos.RData', parameter_combos)
