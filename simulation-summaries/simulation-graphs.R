
# load packages, data ====

library(ggplot2)
library(ggpubr)

load('../matching-simulations/matching-simulations-summary.RData')
load('../weighting-simulations/weighting-simulations-summary.RData')




# matching results cleaning ====

matching_results$prev = as.factor(matching_results$prev)

prev_labs = paste('Prev:', c('5%', '10%', '20%', '30%'))
names(prev_labs) = c('5', '10', '20', '30')


matching_results$smd_num = matching_results$smd / 100

matching_results$smd = as.factor(matching_results$smd)

smd_labs = paste('SMD:', c('0.20', '0.50', '0.80'))
names(smd_labs) = c('20', '50', '80')


matching_results$ratio = factor(matching_results$ratio, levels=0:3,
                                labels=c('Full', '1', '2', '3'))


matching_results$caliper = factor(matching_results$caliper, 
                                  levels=c(10, 25, 40, 0), 
                                  labels=c('0.10', '0.25', '0.40', 'None'))

tmp = rowMeans(subset(matching_results, 
                      select=c('mpost_smd1', 'mpost_smd2', 
                               'mpost_smd3', 'mpost_smd4', 
                               'mpost_smd5')))
matching_results$avg_post_smd = tmp

tmp = rowSums(subset(matching_results, 
                     select=c('mpost_smd1', 'mpost_smd2', 
                              'mpost_smd3', 'mpost_smd4', 
                              'mpost_smd5')) > 0.1)

matching_results$post_unbal = tmp




# weighting results cleaning ====

weighting_results$prev = as.factor(weighting_results$prev)

prev_labs0 = c('5%', '10%', '20%', '30%')
names(prev_labs0) = c('5', '10', '20', '30')

prev_labs = paste('Prev:', prev_labs0)
names(prev_labs) = c('5', '10', '20', '30')


weighting_results$smd_num = weighting_results$smd / 100

weighting_results$smd = as.factor(weighting_results$smd)

smd_labs = paste('SMD:', c('0.20', '0.50', '0.80'))
names(smd_labs) = c('20', '50', '80')


stab_labs = c('No', 'Yes')
names(stab_labs) = c('FALSE', 'TRUE')


tmp = rowMeans(subset(weighting_results, 
                      select=c('mpost_smd1', 'mpost_smd2', 
                               'mpost_smd3', 'mpost_smd4', 
                               'mpost_smd5')))
weighting_results$avg_post_smd = tmp


tmp = rowSums(subset(weighting_results, 
                     select=c('mpost_smd1', 'mpost_smd2', 
                              'mpost_smd3', 'mpost_smd4', 
                              'mpost_smd5')) > 0.1)

weighting_results$post_unbal = tmp




# matching and weighting ====

## covariate balance SMD ----

balance_m = ggplot(matching_results, aes(x=ratio, y=avg_post_smd, 
                                         color=caliper, 
                                         shape=caliper)) +
  geom_point(position=position_dodge(width=0.5), size=2) + 
  geom_segment(aes(y=smd_num, yend=smd_num, x=0, xend=5), 
               col='black', linetype=2) + 
  geom_hline(yintercept=0.1, col='grey', linetype=3) + 
  scale_y_continuous(minor_breaks=NULL) + 
  scale_color_manual(name='Caliper', 
                     values=c('#1b9e77','#d95f02','#7570b3','#e7298a')) + 
  scale_shape_discrete(name='Caliper') + 
  labs(x = "Matching ratio", 
       y = "Average covariate SMD") + 
  facet_grid(rows=vars(smd), cols=vars(prev), 
             labeller=labeller(prev=prev_labs, smd=smd_labs)) + 
  theme_bw() + 
  theme(legend.position='bottom')


balance_w = ggplot(weighting_results, aes(x=prev, y=avg_post_smd, 
                                          color=stabilize)) +
  geom_point(position=position_dodge(width=0.5), size=2) + 
  geom_segment(aes(y=smd_num, yend=smd_num, x=0, xend=5), 
               col='black', linetype=2) + 
  geom_hline(yintercept=0.1, col='grey', linetype=3) + 
  scale_color_discrete(name='Stabilized', labels=stab_labs) + 
  scale_x_discrete(labels=prev_labs0) + 
  scale_y_continuous(minor_breaks=NULL) + 
  labs(x = "Exposure prevalence", 
       y = "") + 
  facet_grid(rows=vars(smd), 
             labeller=labeller(smd=smd_labs)) + 
  theme_bw() + 
  theme(legend.position='bottom')


balance_plot = ggarrange(balance_m, balance_w, 
                         ncol=2, nrow=1, widths=c(3,1), 
                         labels='AUTO')


## covariate balance unbalanced ----

unbalanced_m = ggplot(matching_results, aes(x=ratio, y=post_unbalanced, 
                                            fill=caliper)) +
  geom_col(position=position_dodge()) +
  scale_y_continuous(limits=c(0,5), minor_breaks=NULL) +
  scale_fill_manual(name='Caliper', 
                     values=c('#1b9e77','#d95f02','#7570b3','#e7298a')) + 
  labs(x = "Matching ratio",
       y = "Average number of unbalanced covariates") +
  facet_grid(rows=vars(smd), cols=vars(prev),
             labeller=labeller(prev=prev_labs, smd=smd_labs)) +
  theme_bw() + 
  theme(legend.position='bottom')


unbalanced_w = ggplot(weighting_results, aes(x=prev, y=post_unbalanced, 
                                             fill=stabilize)) +
  geom_col(position=position_dodge()) +
  scale_fill_discrete(name='Stabilized', labels=stab_labs) +
  scale_x_discrete(labels=prev_labs0) +
  scale_y_continuous(limits=c(0,5), minor_breaks=NULL) +
  labs(x = "Exposure prevalence",
       y = "") +
  facet_grid(rows=vars(smd),
             labeller=labeller(smd=smd_labs)) +
  theme_bw() + 
  theme(legend.position='bottom')


unbalanced_plot = ggarrange(unbalanced_m, unbalanced_w,
          ncol=2, nrow=1, widths=c(3,1),
          labels='AUTO')


## bias ----

bias_m = ggplot(matching_results, aes(x=ratio, y=post_bias, 
                                      color=caliper, shape=caliper)) +
  geom_point(position=position_dodge(width=0.5), size=2) + 
  scale_y_continuous(limits=c(-0.05,3.25), minor_breaks=NULL) + 
  scale_color_manual(name='Caliper', 
                     values=c('#1b9e77','#d95f02','#7570b3','#e7298a')) + 
  scale_shape_discrete(name='Caliper') + 
  labs(x = "Matching ratio", 
       y = "Effect estimate bias") + 
  facet_grid(rows=vars(smd), cols=vars(prev), 
             labeller=labeller(prev=prev_labs, smd=smd_labs)) + 
  theme_bw() + 
  theme(legend.position='bottom')


bias_w = ggplot(weighting_results, aes(x=prev, y=post_bias, 
                                       color=stabilize)) +
  geom_point(position=position_dodge(width=0.5), size=2) + 
  scale_color_discrete(name='Stabilized', labels=stab_labs) + 
  scale_x_discrete(labels=prev_labs0) + 
  scale_y_continuous(limits=c(-0.05,3.25), minor_breaks=NULL) + 
  labs(x = "Exposure prevalence", 
       y = "") + 
  facet_grid(rows=vars(smd), 
             labeller=labeller(smd=smd_labs)) + 
  theme_bw() + 
  theme(legend.position='bottom')

bias_plot = ggarrange(bias_m, bias_w, 
                      ncol=2, nrow=1, widths=c(3,1), 
                      labels='AUTO')


## standard error ----

sterr_m = ggplot(matching_results, aes(x=ratio, y=sqrt(vpost_d), 
                                      color=caliper, shape=caliper)) +
  geom_point(position=position_dodge(width=0.5), size=2) + 
  scale_y_continuous(limits=c(0,0.8), minor_breaks=NULL) + 
  scale_color_manual(name='Caliper', 
                     values=c('#1b9e77','#d95f02','#7570b3','#e7298a')) + 
  scale_shape_discrete(name='Caliper') + 
  labs(x = "Matching ratio", 
       y = "Effect estimate standard error") + 
  facet_grid(rows=vars(smd), cols=vars(prev), 
             labeller=labeller(prev=prev_labs, smd=smd_labs)) + 
  theme_bw() + 
  theme(legend.position='bottom')


sterr_w = ggplot(weighting_results, aes(x=prev, y=sqrt(vpost_d), 
                                        color=stabilize)) +
  geom_point(position=position_dodge(width=0.5), size=2) + 
  scale_color_discrete(name='Stabilized', labels=stab_labs) + 
  scale_x_discrete(labels=prev_labs0) + 
  scale_y_continuous(limits=c(0,0.8), minor_breaks=NULL) + 
  labs(x = "Exposure prevalence", 
       y = "") + 
  facet_grid(rows=vars(smd), 
             labeller=labeller(smd=smd_labs)) + 
  theme_bw() + 
  theme(legend.position='bottom')

sterr_plot = ggarrange(sterr_m, sterr_w, 
                       ncol=2, nrow=1, widths=c(3,1), 
                       labels='AUTO')


## root-MSE ----

mse_m = ggplot(matching_results, aes(x=ratio, y=post_rmse, 
                                     color=caliper, shape=caliper)) +
  geom_point(position=position_dodge(width=0.5), size=2) + 
  scale_y_continuous(limits=c(0,3.5), minor_breaks=NULL) + 
  scale_color_manual(name='Caliper', 
                     values=c('#1b9e77','#d95f02','#7570b3','#e7298a')) + 
  scale_shape_discrete(name='Caliper') + 
  labs(x = "Matching ratio", 
       y = "Root-MSE") + 
  facet_grid(rows=vars(smd), cols=vars(prev), 
             labeller=labeller(prev=prev_labs, smd=smd_labs)) + 
  theme_bw() + 
  theme(legend.position='bottom')


mse_w = ggplot(weighting_results, aes(x=prev, y=post_rmse, 
                                      color=stabilize)) +
  geom_point(position=position_dodge(width=0.5), size=2) + 
  scale_color_discrete(name='Stabilized', labels=stab_labs) + 
  scale_x_discrete(labels=prev_labs0) + 
  scale_y_continuous(limits=c(0,3.5), minor_breaks=NULL) + 
  labs(x = "Exposure prevalence", 
       y = "") + 
  facet_grid(rows=vars(smd), 
             labeller=labeller(smd=smd_labs)) + 
  theme_bw() + 
  theme(legend.position='bottom')

mse_plot = ggarrange(mse_m, mse_w, 
                     ncol=2, nrow=1, widths=c(3,1), 
                     labels='AUTO')


## confidence interval coverage ----

cov_m = ggplot(matching_results, aes(x=ratio, y=post_coverage, 
                                     color=caliper, shape=caliper)) +
  geom_point(position=position_dodge(width=0.5), size=2) + 
  geom_hline(yintercept=1) + 
  geom_hline(yintercept=0.95, col='grey', linetype=2) + 
  scale_y_continuous(limits=c(0,1), breaks=c(0,0.5,0.8,0.9,1), 
                     minor_breaks=NULL,
                     trans=scales::exp_trans(base=10)) + 
  scale_color_manual(name='Caliper', 
                     values=c('#1b9e77','#d95f02','#7570b3','#e7298a')) + 
  scale_shape_discrete(name='Caliper') + 
  labs(x = "Matching ratio", 
       y = "Confidence interval coverage\nbase-10 exponential scale") + 
  facet_grid(rows=vars(smd), cols=vars(prev), 
             labeller=labeller(prev=prev_labs, smd=smd_labs)) + 
  theme_bw() + 
  theme(legend.position='bottom')


cov_w = ggplot(weighting_results, aes(x=prev, y=post_coverage, 
                                      color=stabilize)) +
  geom_point(position=position_dodge(width=0.5), size=2) + 
  geom_hline(yintercept=1) + 
  geom_hline(yintercept=0.95, col='grey', linetype=2) + 
  scale_color_discrete(name='Stabilized', labels=stab_labs) + 
  scale_x_discrete(labels=prev_labs0) + 
  scale_y_continuous(limits=c(0,1), breaks=c(0,0.5,0.8,0.9,1), 
                     minor_breaks=NULL,
                     trans=scales::exp_trans(base=10)) + 
  labs(x = "Exposure prevalence", 
       y = "") + 
  facet_grid(rows=vars(smd), 
             labeller=labeller(smd=smd_labs)) + 
  theme_bw() + 
  theme(legend.position='bottom')


coverage_plot = ggarrange(cov_m, cov_w, 
                          ncol=2, nrow=1, widths=c(3,1), 
                          labels='AUTO')




# matching only ====

## n1post / n1pre ----

selection_plot = ggplot(matching_results, aes(x=ratio, y=n1_post_n1_pre, 
                                              color=caliper, shape=caliper)) +
  geom_point(position=position_dodge(width=0.5), size=2) + 
  geom_hline(yintercept=1) + 
  scale_y_continuous(minor_breaks=NULL) + 
  scale_color_manual(name='Caliper', values=c('#1b9e77','#d95f02','#7570b3','#e7298a')) + 
  scale_shape_discrete(name='Caliper') + 
  labs(x = "Matching ratio", 
       y = "Post-matching n1 / pre-matching n1") + 
  facet_grid(rows=vars(smd), cols=vars(prev), 
             labeller=labeller(prev=prev_labs, smd=smd_labs)) + 
  theme_bw() + 
  theme(legend.position='bottom')




# weighting only ====

## extreme weights ----

weights_plot = ggplot(weighting_results, aes(x=prev, y=m_wts_above_20, 
                                             fill=stabilize)) +
  geom_col(position=position_dodge()) + 
  scale_fill_discrete(name='Stabilized', labels=stab_labs) + 
  scale_x_discrete(labels=prev_labs) + 
  scale_y_continuous(minor_breaks=NULL) + 
  labs(x = "Exposure prevalence", 
       y = "Mean count of extreme weights") + 
  facet_grid(rows=vars(smd), 
             labeller=labeller(smd=smd_labs)) + 
  theme_bw() + 
  theme(legend.position='bottom')




# save figures ====

ggsave(filename = 'balance-plot.png', balance_plot, 
       height=6, width=9, units='in')

ggsave(filename = 'unbalanced-plot.png', unbalanced_plot, 
       height=6, width=9, units='in')

ggsave(filename = 'bias-plot.png', bias_plot, 
       height=6, width=9, units='in')

ggsave(filename = 'sterr-plot.png', sterr_plot, 
       height=6, width=9, units='in')

ggsave(filename = 'mse-plot.png', mse_plot, 
       height=6, width=9, units='in')

ggsave(filename = 'coverage-plot.png', coverage_plot, 
       height=6, width=9, units='in')

ggsave(filename = 'selection-plot.png', selection_plot, 
       height=6, width=9, units='in')

ggsave(filename = 'weights-plot.png', weights_plot, 
       height=6, width=9, units='in')


save(file='simulation-graphs.RData', 
     balance_plot, unbalanced_plot, 
     bias_plot, sterr_plot, mse_plot, coverage_plot, 
     selection_plot, 
     weights_plot)
