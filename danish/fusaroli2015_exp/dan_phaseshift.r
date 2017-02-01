# Phase shift analysis on data/all_pairs_entropy.txt
# Yang Xu
# 1/26/2017

library(data.table)
library(MASS)
library(pracma)
library(ggplot2)

# read data
dt = fread('data/all_pairs_entropy.txt')
setnames(dt, c('pairId', 'who', 'ent'))
setkey(dt, pairId, who)

# read performance data
dt.pf = fread('data/PerformanceData.tsv')
setkey(dt.pf, Pair)

##
# analysis
# phase shift at max freqs
dt.maxPS = dt[, {
        y_a = ent[who=='A']
        y_b = ent[who=='B']
        len = min(length(y_a), length(y_b))
        y_a = y_a[1:len]
        y_b = y_b[1:len]
        comb.ts = ts(matrix(c(y_a, y_b), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no', plot=F)
        # phase shift at max spec
        maxPS_a = spec$phase[,1][which(spec$spec[,1]==max(spec$spec[,1]))]
        maxPS_b = spec$phase[,1][which(spec$spec[,2]==max(spec$spec[,2]))]
        # return
        .(maxPS_a = maxPS_a, maxPS_b = maxPS_b)
    }, by = pairId]
dt.maxPS = dt.maxPS[dt.pf, nomatch=0]

# phase shift at the peak freqs
dt.peakPS = dt[, {
        y_a = ent[who=='A']
        y_b = ent[who=='B']
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
    }, by = pairId]
dt.peakPS = dt.peakPS[dt.pf, nomatch=0]

###
# models
m = lm(CollectivePerformance ~ abs(peakPS), dt.peakPS)
summary(m)
# abs(peakPS)  0.07093    0.03302   2.148   0.0319 *
# Adjusted R-squared:  0.002393

m = lm(CollectiveBenefit ~ abs(peakPS), dt.peakPS)
summary(m)
# n.s. 0.994    0.321

m = lm(BestParticipantPerformance ~ abs(peakPS), dt.peakPS)
summary(m)
# abs(peakPS)  0.04259    0.02047    2.08   0.0377 *
# Adjusted R-squared:  0.002204

m = lm(WorstParticipantPerformance ~ abs(peakPS), dt.peakPS)
summary(m)
# abs(peakPS)  0.05469    0.02508   2.181   0.0294 *
# Adjusted R-squared:  0.002487

m = lm(ConfidenceAlignment ~ abs(peakPS), dt.peakPS)
summary(m)
# abs(peakPS) 0.002695   0.001360   1.981   0.0478 *
# Adjusted R-squared:  0.001938

m = lm(ConfidenceConvergence ~ abs(peakPS), dt.peakPS)
summary(m)
# abs(peakPS)   0.6066     0.2847   2.131   0.0333 *
# Adjusted R-squared:  0.002345


##########
# analyze relative phases at all freqs components
# phase shift at all freqs
dt.allPS = dt[, {
        y_a = ent[who=='A']
        y_b = ent[who=='B']
        len = min(length(y_a), length(y_b))
        y_a = y_a[1:len]
        y_b = y_b[1:len]
        comb.ts = ts(matrix(c(y_a, y_b), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no', plot=F)
        # phase shift at all freq components
        .(allPS = spec$phase[,1])
    }, by = pairId]
dt.allPS = dt.allPS[dt.pf, nomatch=0]

##
# models
m = lm(CollectivePerformance ~ abs(allPS), dt.allPS)
summary(m)
# abs(allPS)   0.04657    0.02448   1.902   0.0573 .
# Adjusted R-squared:  0.000954
