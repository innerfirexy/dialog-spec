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
        .(maxPS_a = maxPS_a, maxPS_b = maxPS_b, maxPSm = mean(maxPS_a + maxPS_b), maxPSabsm = mean(abs(maxPS_a) + abs(maxPS_b)))
    }, by = pairId]
dt.maxPS = dt.maxPS[dt.pf, nomatch=0]

# models
m = lm(CollectivePerformance ~ maxPS_a + maxPS_b + maxPSm + maxPSabsm, dt.maxPS)
summary(m)
# (Intercept)  4.56711    0.54960   8.310 2.54e-06 ***
# maxPS_a     -0.41856    0.85618  -0.489    0.634
# maxPS_b      0.07042    0.35724   0.197    0.847
# maxPSm            NA         NA      NA       NA
# maxPSabsm    0.18966    0.46393   0.409    0.690


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
# F-statistic: 4.613 on 1 and 1505 DF,  p-value: 0.03189


###
# use mean, median, and max peak relative phase
dt.peakPS.mean = dt.peakPS[, {
        .(peakPSmean = mean(abs(peakPS)), peakPSmedian = median(abs(peakPS)), peakPSmax = max(abs(peakPS)), peakN = .N)
    }, by = pairId]
dt.peakPS.mean = dt.peakPS.mean[dt.pf, nomatch=0]

m = lm(CollectivePerformance ~ peakPSmean + peakPSmedian + peakPSmax, dt.peakPS.mean)
summary(m)
# step
step = stepAIC(m)
step$anova
# Final Model:
# CollectivePerformance ~ peakPSmean + peakPSmedian + peakPSmax
# peakPSmean     15.646      5.677   2.756   0.0174 *
# peakPSmedian   -7.429      3.609  -2.058   0.0619 .
# peakPSmax     -11.451      7.176  -1.596   0.1366
# Adjusted R-squared:  0.3541
# F-statistic: 3.742 on 3 and 12 DF,  p-value: 0.04158


m0 = lm(CollectivePerformance ~ peakPSmean, dt.peakPS.mean)
summary(m0)



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
# F-statistic: 3.618 on 1 and 2740 DF,  p-value: 0.05728

# mean, median, and max phase of all freqs
dt.allPS.mean = dt.allPS[, {
        .(allPSmean = mean(abs(allPS)), allPSmedian = median(abs(allPS)), allPSmax = max(abs(allPS)))
    }, by = pairId]
dt.allPS.mean = dt.allPS.mean[dt.pf, nomatch=0]

m = lm(CollectivePerformance ~ allPSmean + allPSmedian + allPSmax, dt.allPS.mean)
summary(m)
step = stepAIC(m)
step$anova
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)   40.154     62.951   0.638    0.536
# allPSmean     13.200      9.633   1.370    0.196
# allPSmedian   -6.785      5.822  -1.165    0.267
# allPSmax     -14.406     20.442  -0.705    0.494
summary(lm(CollectivePerformance ~ allPSmean, dt.allPS.mean)) # n.s.
# allPSmean      2.228      2.536   0.879    0.394
# F-statistic: 0.7722 on 1 and 14 DF,  p-value: 0.3944
