# Phase shift analysis of Fusaroli 2015 data
# Yang Xu
# 12/19/2016

library(data.table)
library(MASS)
library(pracma)


# read length data
dt = readRDS('dt.merged.rds')

# read performance data
dt.pf = fread('data/PerformanceData.tsv')
setkey(dt.pf, Pair)


###
# phase shift analysis

# phase shift at the freqs that have max spec, for both giver and follower
dt.maxPS = dt[, {
        y_a = length[who=='A']
        y_b = length[who=='B']
        len = min(length(y_a), length(y_b))
        y_a = y_a[1:len]
        y_b = y_b[1:len]
        comb.ts = ts(matrix(c(y_a, y_b), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no')
        # phase shift at max spec
        maxPS_a = spec$phase[,1][which(spec$spec[,1]==max(spec$spec[,1]))]
        maxPS_b = spec$phase[,1][which(spec$spec[,2]==max(spec$spec[,2]))]
        # return
        .(maxPS_a = maxPS_a, maxPS_b = maxPS_b)
    }, by = cid]

dt.maxPS = dt.maxPS[dt.pf, nomatch=0]

###
# correlations
cor.test(abs(dt.maxPS$maxPS_a), dt.maxPS$CollectivePerformance) # n.s.
cor.test(abs(dt.maxPS$maxPS_a), dt.maxPS$CollectiveBenefit) #
cor.test(abs(dt.maxPS$maxPS_a), dt.maxPS$BestParticipantPerformance) #
cor.test(abs(dt.maxPS$maxPS_a), dt.maxPS$WorstParticipantPerformance) #
cor.test(abs(dt.maxPS$maxPS_a), dt.maxPS$ConfidenceAlignment) #
cor.test(abs(dt.maxPS$maxPS_a), dt.maxPS$ConfidenceConvergence) #

cor.test(abs(dt.maxPS$maxPS_b), dt.maxPS$CollectivePerformance) # n.s.
cor.test(abs(dt.maxPS$maxPS_b), dt.maxPS$CollectiveBenefit) #
cor.test(abs(dt.maxPS$maxPS_b), dt.maxPS$BestParticipantPerformance) #
### *
cor.test(abs(dt.maxPS$maxPS_b), dt.maxPS$WorstParticipantPerformance) # t = -2.0765, df = 14, p-value = 0.05674
###
cor.test(abs(dt.maxPS$maxPS_b), dt.maxPS$ConfidenceAlignment) #
cor.test(abs(dt.maxPS$maxPS_b), dt.maxPS$ConfidenceConvergence) #

cor.test(dt.maxPS$maxPS_a, dt.maxPS$CollectivePerformance) # n.s.
cor.test(dt.maxPS$maxPS_a, dt.maxPS$CollectiveBenefit)
cor.test(dt.maxPS$maxPS_a, dt.maxPS$BestParticipantPerformance)
cor.test(dt.maxPS$maxPS_a, dt.maxPS$WorstParticipantPerformance)
cor.test(dt.maxPS$maxPS_a, dt.maxPS$ConfidenceAlignment)
cor.test(dt.maxPS$maxPS_a, dt.maxPS$ConfidenceConvergence)

cor.test(dt.maxPS$maxPS_b, dt.maxPS$CollectivePerformance) # n.s.
cor.test(dt.maxPS$maxPS_b, dt.maxPS$CollectiveBenefit)
cor.test(dt.maxPS$maxPS_b, dt.maxPS$BestParticipantPerformance) # t = 2.9333, df = 14, p-value = 0.0109
cor.test(dt.maxPS$maxPS_b, dt.maxPS$WorstParticipantPerformance)
cor.test(dt.maxPS$maxPS_b, dt.maxPS$ConfidenceAlignment)
cor.test(dt.maxPS$maxPS_b, dt.maxPS$ConfidenceConvergence)


####
# phase shift at the peak freqs
dt.peakPS = dt[, {
        y_a = length[who=='A']
        y_b = length[who=='B']
        len = min(length(y_a), length(y_b))
        y_a = y_a[1:len]
        y_b = y_b[1:len]
        comb.ts = ts(matrix(c(y_a, y_b), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no')
        # phase shift at all peaks
        i_max_a = which(diff(sign(diff(spec$spec[,1])))<0) + 1
        i_max_b = which(diff(sign(diff(spec$spec[,2])))<0) + 1
        peakPS = spec$phase[,1][union(i_max_a, i_max_b)]
        # return
        .(peakPS = peakPS)
    }, by = cid]

dt.peakPS = dt.peakPS[dt.pf, nomatch=0]

###
# correlations
cor.test(abs(dt.peakPS$peakPS), dt.peakPS$CollectivePerformance) # n.s.
cor.test(abs(dt.peakPS$peakPS), dt.peakPS$CollectiveBenefit) #
cor.test(abs(dt.peakPS$peakPS), dt.peakPS$BestParticipantPerformance) #
cor.test(abs(dt.peakPS$peakPS), dt.peakPS$WorstParticipantPerformance) # t = -1.4054, df = 1509, p-value = 0.1601
cor.test(abs(dt.peakPS$peakPS), dt.peakPS$ConfidenceAlignment) #
cor.test(abs(dt.peakPS$peakPS), dt.peakPS$ConfidenceConvergence) #

cor.test(dt.peakPS$peakPS, dt.peakPS$CollectivePerformance) # n.s.
cor.test(dt.peakPS$peakPS, dt.peakPS$CollectiveBenefit) #
cor.test(dt.peakPS$peakPS, dt.peakPS$BestParticipantPerformance) #
cor.test(dt.peakPS$peakPS, dt.peakPS$WorstParticipantPerformance) #
cor.test(dt.peakPS$peakPS, dt.peakPS$ConfidenceAlignment) # t = 2.0008, df = 1509, p-value = 0.04559
cor.test(dt.peakPS$peakPS, dt.peakPS$ConfidenceConvergence) #

###
# models
m = lm(CollectivePerformance ~ abs(peakPS), dt.peakPS)
summary(m)

m = lm(CollectiveBenefit ~ abs(peakPS), dt.peakPS)
summary(m)

m = lm(BestParticipantPerformance ~ abs(peakPS), dt.peakPS)
summary(m)

m = lm(WorstParticipantPerformance ~ abs(peakPS), dt.peakPS)
summary(m) # -1.405     0.16

m = lm(ConfidenceAlignment ~ abs(peakPS), dt.peakPS)
summary(m)

m = lm(ConfidenceConvergence ~ abs(peakPS), dt.peakPS)
summary(m)


# w/o abs
m = lm(CollectivePerformance ~ peakPS, dt.peakPS)
summary(m)

m = lm(CollectiveBenefit ~ peakPS, dt.peakPS)
summary(m)

m = lm(BestParticipantPerformance ~ peakPS, dt.peakPS)
summary(m)

m = lm(WorstParticipantPerformance ~ peakPS, dt.peakPS)
summary(m)

m = lm(ConfidenceAlignment ~ peakPS, dt.peakPS)
summary(m) # 2.001   0.0456 *

m = lm(ConfidenceConvergence ~ peakPS, dt.peakPS)
summary(m)
