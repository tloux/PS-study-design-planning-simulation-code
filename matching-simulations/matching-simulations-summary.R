
# load packages, data ====

load("matching-simulation-combos.RData")
load("run-match-sims-parallel.RData")




# function for statistics per simulation set ====

calculate_statistics = function(dataset) { 
  
  
  # means and variances
  
  specified_vars <- c("pre_smd_ps", "pre_smd1", "pre_smd2", "pre_smd3", 
                      "pre_smd4", "pre_smd5", "pre_d", 
                      "post_smd_ps", "post_smd1", "post_smd2", "post_smd3", 
                      "post_smd4", "post_smd5", "post_d", 
                      "pre_n1", "pre_n0", "post_n1", "post_n0")
  
  means <- sapply(specified_vars, function(var) mean(dataset[[var]], na.rm = TRUE))
  
  vars <- sapply(specified_vars, function(var) var(dataset[[var]], na.rm = TRUE))
  
  
  # unbalanced variables
  
  post_unbalanced = mean(rowSums(abs(subset(dataset, 
                                            select=c(post_smd1, post_smd2, 
                                                     post_smd3, post_smd4, 
                                                     post_smd5))) > 0.10))
  
  
  ## get ATT
  ATT = ifelse(mean(dataset$pre_smd1) < 0.35, 2.11, 
               ifelse(mean(dataset$pre_smd1) > 0.65, 2.3, 2.205))
  ##
  
  # confidence interval coverage
  
  pre_covered = (dataset$pre_ci_lo <= ATT & ATT <= dataset$pre_ci_hi)
  pre_coverage = mean(pre_covered)
  
  post_covered = (dataset$post_ci_lo <= ATT & ATT <= dataset$post_ci_hi)
  post_coverage = mean(post_covered)
  
  
  # additional calculations 
  
  n1_post_n1_pre <- mean(dataset$post_n1 / dataset$pre_n1)
  
  n0_post_n0_pre <- mean(dataset$post_n0 / dataset$pre_n0)
  
  n0_post_n1_post <- mean(dataset$post_n0 / dataset$post_n1)
  
  pre_bias = means[7] - ATT
  
  pre_rmse = sqrt(pre_bias^2 + vars[7])
  
  post_bias = means[14] - ATT
  post_rmse = sqrt(post_bias^2 + vars[14])
  
  
  # compiling results
  
  results <- c(ATT, means, vars, post_unbalanced, pre_coverage, post_coverage, 
               n1_post_n1_pre, n0_post_n0_pre, n0_post_n1_post, 
               pre_bias, pre_rmse, post_bias, post_rmse) 
  
  names(results) <- c('ATT', paste0("m", specified_vars), paste0("v", specified_vars), 
                      'post_unbalanced', 'pre_coverage', 'post_coverage', 
                      "n1_post_n1_pre", "n0_post_n0_pre", "n0_post_n1_post", 
                      'pre_bias', 'pre_rmse', 'post_bias', 'post_rmse') 

  return(results)
}




# apply function to all simulations ====

matching_results0 = lapply(sim_list, function(x) calculate_statistics(x))
matching_results1 = do.call(rbind, matching_results0)


matching_results = cbind(parameter_combos, matching_results1)




# save results ====

save(file='matching-simulations-summary.RData', matching_results)
