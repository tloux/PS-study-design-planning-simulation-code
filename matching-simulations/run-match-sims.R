
# load packages, data ====

source('../sim-functions/sim-functions.R')

load('matching-simulation-combos.RData')




# simulations ====

sim_list = lapply(1:nrow(parameter_combos), function(sim){
  
  
  ## simulation parameters ====
  
  nsims = 10
  
  samp_n = 2000
  
  
  popfile = paste0('../populations/simpop-ptr', parameter_combos$prev[sim], 
                   '-smd', parameter_combos$smd[sim], '.RData')
  
  load(popfile)
  
  match_method = ifelse(parameter_combos$ratio[sim] %in% 1:3, 'nearest', 'full')
  
  match_ratio = parameter_combos$ratio[sim]
  
  if(parameter_combos$caliper[sim] == 0){
    match_caliper = NULL
  }else{
    match_caliper = parameter_combos$caliper[sim] / 100
  }
  
  
  
  
  ## start simulation ====
  
  sim_res = lapply(1:nsims, function(i){
    
    
    ### sample from population ====
    
    my_sample = take_sample(pop = simpop, samp_size = samp_n)
    
    
    ### pre-matching t-test ====
    
    pre_t = do_t_test_pre(dat = my_sample)
    names(pre_t) = c('pre_d', 'pre_ci_lo', 'pre_ci_hi')
    
    
    ### match and summary stats ====
    
    my_matches = get_matches(dat = my_sample, 
                             meth = match_method, 
                             rat = match_ratio, 
                             cal = match_caliper)
    
    match_summaries = get_match_summaries(matches = my_matches)
    
    
    ### post-matching t-test ====
    
    if(match_method == 'full'){
      post_t = do_t_test_post_full(dat = match.data(my_matches))
    }else{
      post_t = do_t_test_post(dat = match.data(my_matches))
    }
    names(post_t) = c('post_d', 'post_ci_lo', 'post_ci_hi')
    
    
    ### return simulation results ====
    
    sim_return = c(pre_t, match_summaries, post_t)
    return(sim_return)
  })
  
  
  ## format results ====
  
  simulation_results = data.frame(do.call(rbind, sim_res))
  
  return(simulation_results)
})




# save full simulation results ====

save(file='run-match-sims.RData', sim_list)
