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
