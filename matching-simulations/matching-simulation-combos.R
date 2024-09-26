
# population parameters

prevs = c(5, 10, 20, 30)
smds = c(20, 50, 80)

# matching parameters

ratios = c(1, 2, 3, 0) # ratio = 0 -> full matching
calipers = c(10, 25, 40, 0) # caliper = 0 -> no caliper

# combinations

parameter_combos = expand.grid(calipers, ratios, smds, prevs)
names(parameter_combos) = c('caliper', 'ratio', 'smd', 'prev')

# save

save(file='matching-simulation-combos.RData', parameter_combos)
