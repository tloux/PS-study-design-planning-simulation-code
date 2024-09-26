
# load packages, data ====

library(dplyr)

load('../matching-simulations/matching-simulations-summary.RData')
load('../weighting-simulations/weighting-simulations-summary.RData')




# combining results ====

all_results = bind_rows(matching_results, weighting_results)




# example table for one population ====

tmp = subset(all_results, 
             subset = smd==80 & prev==30, 
             select = c(ratio, caliper, stabilize, 
                        mpre_d, mpost_d, 
                        n1_post_n1_pre, n0_post_n1_post, 
                        post_bias, vpost_d, 
                        post_rmse, post_coverage))

tmp$simulation = ifelse(is.na(tmp$ratio), 
                        paste('Weighting,', ifelse(tmp$stabilize, 'stabilized', 'unstabilized')), 
                        paste('Matching,', 
                              ifelse(tmp$ratio==0, 'full,', paste(tmp$ratio, ':1 ratio,')), 
                              ifelse(tmp$caliper==0, 'no', tmp$caliper/100), 'caliper'))
                        