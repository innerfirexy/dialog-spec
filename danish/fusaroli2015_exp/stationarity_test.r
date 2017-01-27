# Test the stationarity of time series
# Yang Xu
# 1/27/2017

library(data.table)
library(ggplot2)
library(fpp)
library(forecast)
library(lme4)

# read data
dt = fread('data/all_pairs_entropy.txt')
setnames(dt, c('pairId', 'who', 'ent'))
setkey(dt, pairId, who)


# test
dt.test = dt[, {
        res1 = Box.test(ent)
        res2 = adf.test(ent)
        res3 = kpss.test(ent)
        res4 = pp.test(ent)
        .(boxpval = res1$p.value, adfpval = res2$p.value, kpsspval = res3$p.value, pppval = res4$p.value)
    }, by = .(pairId, who)]

# how many series passed stationarity tests?
nrow(dt.test[boxpval<.05,]) # 1
nrow(dt.test[adfpval<.05,]) # 32
nrow(dt.test[kpsspval>.05,]) # 26
nrow(dt.test[pppval<.05,]) # 32

##
# does entropy increase with utterance position
dt.new = dt[, {
        .(ent, tsId=.GRP, uttPos = 1:.N)
    }, by = .(pairId, who)]
m = lmer(ent ~ uttPos + (1|tsId), dt.new)
summary(m)
# n.s. t = .52

m = lm(ent ~ uttPos, dt.new)
summary(m)
# 1.85   0.0643 .
