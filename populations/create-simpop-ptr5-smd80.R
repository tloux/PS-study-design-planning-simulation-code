
# population parameters ====

N = 1000000

pop_name = 'simpop-ptr5-smd80'

p_tr = 0.05
cont_smd = 0.80
bin_pdif = 0.30

res_sd = 4 # set so full model (line 51-52) R-sq 0.2-0.25
           # as p_tr and cont_smd and bin_pdif increase, R-sq will increase too


# simulate treatment ====

tr = rbinom(n=N, size=1, prob=p_tr)


# simulate confounders ====

x1 = rnorm(n=N, mean=cont_smd*tr, sd=1)
x2 = rnorm(n=N, mean=cont_smd*tr, sd=1)
x3 = rnorm(n=N, mean=cont_smd*tr, sd=1)

x4 = rbinom(n=N, size=1, prob=0.2 + bin_pdif*tr)
x5 = rbinom(n=N, size=1, prob=0.5 + bin_pdif*tr)


# simulate potential outcomes ====

y0 = x1 + x2 + x3 + x4 + x5 + rnorm(n=N, mean=0, sd=res_sd)
y1 = y0 + 2 + 0.25*x1 + 0.2*x4

y = tr * y1 + (1 - tr) * y0


# check models ====

library(effectsize)

pscr = glm(tr ~ x1 + x2 + x3 + x4 + x5, family=binomial('logit'))$fitted
lin_pscr = log(pscr/(1-pscr))

cohens_d(pscr ~ tr)
cohens_d(lin_pscr ~ tr)

tmp = lm(y ~ tr)
summary(tmp)

tmp = lm(y ~ x1 + x2 + x3 + x4 + x5 + tr)
summary(tmp)


# create data set ====

simpop = data.frame(x1, x2, x3, x4, x5, tr, y)


# save data set ====

save(file=paste0(pop_name,'.RData'), simpop)
