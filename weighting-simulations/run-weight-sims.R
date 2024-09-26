
# load packages, data ====

source('../sim-functions/sim-functions.R')

load('weighting-simulation-combos.RData')




# simulations ====

sim_list = lapply(1:nrow(parameter_combos), function(sim){
  
  
  ## simulation parameters ====
  
  nsims = 10
  
  samp_n = 2000
  
  
  popfile = paste0('../populations/simpop-ptr', parameter_combos$prev[sim], 
                   '-smd', parameter_combos$smd[sim], '.RData')
  
  load(popfile)
  
  stabilize_weights = parameter_combos$stabilize[sim]
  
  
  
  
  ## start simulation ====
  
  sim_res = lapply(1:nsims, function(i){
    
    
    ### sample from population ====
    
    my_sample = take_sample(pop = simpop, samp_size = samp_n)
    
    
    ### pre-weighting t-test ====
    
    pre_t = do_t_test_pre(dat = my_sample)
    names(pre_t) = c('pre_d', 'pre_ci_lo', 'pre_ci_hi')
    
    
    ### weighting and summary stats ====
    
    my_weighting = get_weighting(dat = my_sample, 
                                 stab = stabilize_weights)
    
    weighting_summaries = get_weighting_summaries(weighting = my_weighting)
    
    
    ### post-weighting t-test ====
    
    post_t = do_weighted_t_test(dat = my_sample, 
                                my_weights = my_weighting$weights)
    
    
    ### return simulation results ====
    
    sim_return = c(pre_t, weighting_summaries, post_t)
    return(sim_return)
  })
  
  
  ## format results ====
  
  simulation_results = data.frame(do.call(rbind, sim_res))
})





# save full simulation results ====

save(file='run-weight-sims.RData', sim_list)
