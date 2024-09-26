
# load packages ====

library(MatchIt)
library(weights)
library(WeightIt)
library(cobalt)
library(survey)




# sample from population ====

take_sample = function(pop, samp_size){
  
  sample_index = sample(x=nrow(pop), size=samp_size, replace=FALSE)
  
  mysample = pop[sample_index, ]
  
  return(mysample)
}




# perform t test ====

do_t_test_pre = function(dat){

  test_result = t.test(y ~ tr, data = dat)

  d_est = test_result$estimate[2] - test_result$estimate[1]

  ci_lo = -test_result$conf.int[2]
  ci_hi = -test_result$conf.int[1]

  ret = c(d_est, ci_lo, ci_hi)

  return(ret)
}

do_t_test_post = function(dat){
  
  myx = dat$y[dat$tr == 1]
  myy = dat$y[dat$tr == 0]
  
  mywtx = dat$weights[dat$tr == 1]
  mywty = dat$weights[dat$tr == 0]
  
  test_res = wtd.t.test(x = myx, y = myy, 
                        weight = mywtx, weighty = mywty, 
                        samedata = FALSE)
  
  d_est = test_res$additional['Difference']
  
  tcrit = qt(0.975, df = test_res$coefficients['df'])
  
  ci_lo = d_est - tcrit * test_res$additional['Std. Err']
  ci_hi = d_est + tcrit * test_res$additional['Std. Err']
  
  ret = unname(c(d_est, ci_lo, ci_hi))
  
  return(ret)
}

do_t_test_post_full = function(dat){
  
  matched_survey = svydesign(ids=~dat$subclass, data=dat, 
                             weights=dat$weights)
  
  tmp_model = svyglm(y ~ tr, design=matched_survey)
  ret1 = coef(tmp_model)['tr']
  ret2 = confint(tmp_model)['tr', ]
  
  ret = c(ret1, ret2)
  
  return(ret)
}




# perform matching ====

get_matches = function(dat, meth, rat, cal){
  
  matches = matchit(tr ~ x1 + x2 + x3 + x4 + x5,
                    data = dat,
                    distance = "glm", 
                    estimand = 'ATT', 
                    method = meth,
                    ratio =rat,
                    caliper = cal)
  
  return(matches)
}




# get matching summary stats ====

get_match_summaries = function(matches){
  
  match_sum = summary(matches)
  
  # pre-matched SMDs
  std_mean_diff_all = match_sum$sum.all[, "Std. Mean Diff."]
  
  # post-matched SMDs
  std_mean_diff_matched = match_sum$sum.matched[, "Std. Mean Diff."]
  
  # pre- and post-matched sample sizes
  samp_sizes = match_sum$nn[c("All", "Matched"), ]
  
  ret = c(std_mean_diff_all, std_mean_diff_matched, samp_sizes)
  
  names(ret) = c('pre_smd_ps', 'pre_smd1', 'pre_smd2', 
                 'pre_smd3', 'pre_smd4', 'pre_smd5', 
                 'post_smd_ps', 'post_smd1', 'post_smd2', 
                 'post_smd3', 'post_smd4', 'post_smd5', 
                 'pre_n0', 'post_n0', 'pre_n1', 'post_n1')
  
  return(ret)
}




# perform weighting ====

get_weighting = function(dat, stab=FALSE){
  
  weighting = weightit(tr ~ x1 + x2 + x3 + x4 + x5,
                       data = dat, 
                       method = 'glm', 
                       estimand = 'ATT', 
                       stabilize = stab)
  
  return(weighting)
}




# get weighting summary stats ====

get_weighting_summaries = function(weighting){
  
  after_weights = bal.tab(weighting, 
                          un = TRUE, 
                          s.d.denom = 'treated')
  
  # pre- and post-weighted SMDs
  st_mean_diff_unadj = after_weights$Balance[, "Diff.Un"]
  st_mean_diff_adj = after_weights$Balance[, "Diff.Adj"]
  
  # pre- and post-weighted sample sizes
  samp_sizes_unadj = unlist(after_weights$Observations["Unadjusted",])
  samp_sizes_adj = unlist(after_weights$Observations["Adjusted",])
  
  sum_wts0 = sum(weighting$weights[weighting$treat == 0])
  
  # extreme weights
  wts_above_20 = sum(weighting$weights[weighting$treat == 0] > 20)
  
  ret = c(st_mean_diff_unadj, st_mean_diff_adj, 
          samp_sizes_unadj, samp_sizes_adj, sum_wts0, wts_above_20)
  
  names(ret) = c('pre_smd_ps', 'pre_smd1', 'pre_smd2', 'pre_smd3', 
                 'pre_smd4', 'pre_smd5', 
                 'post_smd_ps', 'post_smd1', 'post_smd2', 'post_smd3', 
                 'post_smd4', 'post_smd5', 
                 'pre_n0', 'pre_n1', 'post_n0', 'post_n1', 
                 'sum_wts0', 'wts_above_20')
  
  return(ret)
}




# perform weighted t test ====

do_weighted_t_test = function(dat, my_weights){
  
  ps_wt_svy = svydesign(ids=~0, 
                        data = dat, 
                        weights = my_weights)
  
  test_result = svyttest(y ~ tr, design = ps_wt_svy)
  
  d_est = test_result$estimate
  
  ci_lo = test_result$conf.int[1]
  ci_hi = test_result$conf.int[2]
  
  ret = c(d_est, ci_lo, ci_hi)
  names(ret) = c('post_d', 'post_ci_lo', 'post_ci_hi')
  
  return(ret)
}
