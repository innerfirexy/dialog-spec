# Phase shift analysis of Maptask
# Yang Xu
# 12/14/2016

library(data.table)
library(ggplot2)
library(lme4)
library(MASS)
library(pracma)


# read maptask data
dt = readRDS('map.dt.ent_swbd.rds')
setkey(dt, observation, who)

# read the dataset that contains pathdev info
dt.dev = fread('moves_and_deviation.csv')
setnames(dt.dev, "Observation", "observation")
setnames(dt.dev, 'path dev', 'pathdev')
setkey(dt.dev, observation)

# baseline by shuffling
dt.shuffle = dt[, {
        .(ent_swbd = sample(ent_swbd))
    }, by = .(observation, who)]
#################
# note that when we use dt.shuffle instead of dt
# none the of effects is significnat


###
# compute phase shift

# phase shift at the freqs that have max spec, for both giver and follower
dt.ent_maxPS = dt[, {
        y_g = ent_swbd[who=='g']
        y_f = ent_swbd[who=='f']
        len = min(length(y_g), length(y_f))
        y_g = y_g[1:len]
        y_f = y_f[1:len]
        comb.ts = ts(matrix(c(y_g, y_f), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no', plot=F)
        # phase shift at max spec
        maxPS_g = spec$phase[,1][which(spec$spec[,1]==max(spec$spec[,1]))]
        maxPS_f = spec$phase[,1][which(spec$spec[,2]==max(spec$spec[,2]))]
        # return
        .(maxPSg = maxPS_g, maxPSf = maxPS_f)
    }, by = observation]

# phase shift at the peak freqs of giver
dt.ent_peakPSg = dt[, {
        y_g = ent_swbd[who=='g']
        y_f = ent_swbd[who=='f']
        len = min(length(y_g), length(y_f))
        y_g = y_g[1:len]
        y_f = y_f[1:len]
        comb.ts = ts(matrix(c(y_g, y_f), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no', plot=F)
        # phase shift at all peaks
        i_max_g = which(diff(sign(diff(spec$spec[,1])))<0) + 1
        peakPS_g = spec$phase[,1][i_max_g]
        # return
        .(peakPSg = peakPS_g, peakPSg_mean = mean(peakPS_g))
    }, by = observation]

# phase shift at the peak freqs of follower
dt.ent_peakPSf = dt[, {
        y_g = ent_swbd[who=='g']
        y_f = ent_swbd[who=='f']
        len = min(length(y_g), length(y_f))
        y_g = y_g[1:len]
        y_f = y_f[1:len]
        comb.ts = ts(matrix(c(y_g, y_f), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no', plot=F)
        # phase shift at all peaks
        i_max_f = which(diff(sign(diff(spec$spec[,2])))<0) + 1
        peakPS_f = spec$phase[,1][i_max_f]
        # return
        .(peakPSf = peakPS_f, peakPSf_mean = mean(peakPS_f))
    }, by = observation]

dt.ent_maxPS = dt.ent_maxPS[dt.dev[, .(observation, pathdev)], nomatch=0]
dt.ent_peakPSg = dt.ent_peakPSg[dt.dev[, .(observation, pathdev)], nomatch=0]
dt.ent_peakPSf = dt.ent_peakPSf[dt.dev[, .(observation, pathdev)], nomatch=0]


# models
m = lm(pathdev ~ abs(maxPSg), dt.ent_maxPS)
summary(m)
# -1.861   0.0653 .

m = lm(pathdev ~ abs(maxPSf), dt.ent_maxPS)
summary(m)
# n.s.
m = lm(pathdev ~ maxPSf, dt.ent_maxPS) # ??
summary(m)
# -2.741  0.00712 **


m = lm(pathdev ~ abs(peakPSg), dt.ent_peakPSg)
summary(m)
# n.s.

m = lm(pathdev ~ abs(peakPSf), dt.ent_peakPSf)
summary(m)
# -2.25   0.0246 *
m = lm(pathdev ~ peakPSf, dt.ent_peakPSf)
summary(m)
# -2.101   0.0358 *

m = lm(pathdev ~ abs(peakPSg_mean), unique(dt.ent_peakPSg[,c(1,3,4), with=F]))
summary(m)
# -1.68   0.0957 .

m = lm(pathdev ~ abs(peakPSf_mean), unique(dt.ent_peakPSf[,c(1,3,4), with=F]))
summary(m)
# n.s.
m = lm(pathdev ~ peakPSf_mean, unique(dt.ent_peakPSf[,c(1,3,4), with=F]))
summary(m)
# -2.071   0.0407 *



#############
# analyze relative phases at all freqs components
# phase shift at all freqs
dt.allPS = dt[, {
        y_a = ent_swbd[who=='f']
        y_b = ent_swbd[who=='g']
        len = min(length(y_a), length(y_b))
        y_a = y_a[1:len]
        y_b = y_b[1:len]
        comb.ts = ts(matrix(c(y_a, y_b), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no', plot=F)
        # phase shift at all freq components
        .(allPS = spec$phase[,1])
    }, by = observation]
dt.allPS = dt.allPS[dt.dev[, .(observation, pathdev)], nomatch=0]

##
# models
m = lm(pathdev ~ abs(allPS), dt.allPS) ## well, this is not the correct way
summary(m)
# abs(allPS)   -0.1395     0.7141  -0.195    0.845
# Adj-R^2: NA
# F-statistic: 0.03819 on 1 and 5476 DF,  p-value: 0.8451

# use mean, median and max values for all freqs
dt.allPS.mean = dt.allPS[, {
        .(allPSmean = mean(allPS), allPSmedian = median(allPS), allPSmax = max(abs(allPS)))
    }, by = observation]
dt.allPS.mean = dt.allPS.mean[dt.dev[, .(observation, pathdev)], nomatch=0]

m = lm(pathdev ~ allPSmean + allPSmedian + allPSmax, dt.allPS.mean)
summary(m)
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)   -95.42     172.86  -0.552    0.582
# allPSmean      18.32      63.21   0.290    0.772
# allPSmedian   -30.82      38.03  -0.810    0.420
# allPSmax       59.57      58.69   1.015    0.312
step = stepAIC(m)
step$anova
# ok, n.s.
## If we remove the absolute values of allPSmean allPSmedian, still n.s.
summary(lm(pathdev ~ allPSmean, dt.allPS.mean))
# allPSmean    -0.9359    14.2781  -0.066    0.948
# F-statistic: 0.004296 on 1 and 113 DF,  p-value: 0.9479


##
# use weighted means of relative phase, the weight is the power of freqs
dt.allPS.wm = dt[, {
        y_a = ent_swbd[who=='f']
        y_b = ent_swbd[who=='g']
        len = min(length(y_a), length(y_b))
        y_a = y_a[1:len]
        y_b = y_b[1:len]
        comb.ts = ts(matrix(c(y_a, y_b), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no', plot=F)
        # phase shift at all freq components, weighted by spec
        allPSwm_a = weighted.mean(spec$phase[,1], spec$spec[,1])
        allPSwm_b = weighted.mean(spec$phase[,1], spec$spec[,2])
        allPSwm = mean(abs(allPSwm_a), abs(allPSwm_b))
        .(allPSwm = allPSwm)
    }, by = observation]
dt.allPS.wm = dt.allPS.wm[dt.dev[, .(observation, pathdev)], nomatch=0]

m = lm(pathdev ~ allPSwm, dt.allPS.wm)
summary(m)
# n.s.
# remove the abs(), also n.s.


########################
# phase shift at the peak freqs
dt.peakPS = dt[, {
        y_a = ent_swbd[who=='f']
        y_b = ent_swbd[who=='g']
        len = min(length(y_a), length(y_b))
        y_a = y_a[1:len]
        y_b = y_b[1:len]
        comb.ts = ts(matrix(c(y_a, y_b), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no', plot=F)
        # phase shift at all peaks
        i_max_a = which(diff(sign(diff(spec$spec[,1])))<0) + 1
        i_max_b = which(diff(sign(diff(spec$spec[,2])))<0) + 1
        peakPS = spec$phase[,1][union(i_max_a, i_max_b)]
        # return
        .(peakPS = peakPS)
    }, by = observation]
dt.peakPS = dt.peakPS[dt.dev[, .(observation, pathdev)], nomatch=0]


# models
m = lm(pathdev ~ abs(peakPS), dt.peakPS)
summary(m)
# abs(peakPS)  -2.0031     0.9932  -2.017   0.0438 *
# Adjusted R-squared:  0.001049
# F-statistic: 4.067 on 1 and 2920 DF,  p-value: 0.04381


## compute mean, median and max values for peakPS
dt.peakPS.mean = dt.peakPS[, {
        .(peakPSmean = mean(abs(peakPS)), peakPSmedian = median(abs(peakPS)), peakPSmax = max(abs(peakPS)))
    }, by = observation]
### save to rds file
# saveRDS(dt.peakPS.mean, 'dt.peakRP.rds')
dt.peakPS.mean = dt.peakPS.mean[dt.dev[, .(observation, pathdev)], nomatch=0]


m = lm(pathdev ~ peakPSmean + peakPSmedian + peakPSmax, dt.peakPS.mean)
summary(m)
# step
step = stepAIC(m)
step$anova
# Final Model:
# pathdev ~ peakPSmax
m = lm(pathdev ~ peakPSmax, dt.peakPS.mean)
summary(m)
# peakPSmax     -64.86      30.33  -2.138  0.03463 *
# Adjusted R-squared:  0.03039


######
# use the weighted mean of peakPS
dt.peakPS.wm = dt[, {
        y_a = ent_swbd[who=='f']
        y_b = ent_swbd[who=='g']
        len = min(length(y_a), length(y_b))
        y_a = y_a[1:len]
        y_b = y_b[1:len]
        comb.ts = ts(matrix(c(y_a, y_b), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no', plot=F)
        # phase shift at all peaks
        i_max_a = which(diff(sign(diff(spec$spec[,1])))<0) + 1
        i_max_b = which(diff(sign(diff(spec$spec[,2])))<0) + 1
        # peakPS = spec$phase[,1][union(i_max_a, i_max_b)]
        peakPSwm_a = weighted.mean(spec$phase[,1][i_max_a], spec$spec[i_max_a])
        peakPSwm_b = weighted.mean(spec$phase[,1][i_max_b], spec$spec[i_max_b])
        peakPSwm = mean(abs(peakPSwm_a), abs(peakPSwm_b))
        # return
        .(peakPSwm = peakPSwm)
    }, by = observation]
dt.peakPS.wm = dt.peakPS.wm[dt.dev[, .(observation, pathdev)], nomatch=0]

m = lm(pathdev ~ peakPSwm, dt.peakPS.wm)
summary(m)
# n.s.
# remove the abs in weighted.mean computation
# also n.s.
## OK, weighted mean does not seem working
