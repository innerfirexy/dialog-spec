# Power Spectral analysis of Fusaroli 2015 data
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
# spectral analysis
setkey(dt, cid, who)
dt.spec = dt[, {
        specval = spec.pgram(length, taper=0, log='no')
        .(spec = specval$spec, freq = specval$freq)
    }, by = .(cid, who)]

dt.pso = dt.spec[, {
        x_a = freq[who=='A']
        y_a = spec[who=='A']
        x_b = freq[who=='B']
        y_b = spec[who=='B']
        # linear interpolation
        x_out = sort(union(x_a, x_b))
        approx_a = approx(x_a, y_a, xout = x_out)
        approx_b = approx(x_b, y_b, xout = x_out)
        # find min ys and remove NAs
        x_out_a = x_out[which(!is.na(approx_a$y))]
        y_out_a = approx_a$y[which(!is.na(approx_a$y))]
        x_out_b = x_out[which(!is.na(approx_b$y))]
        y_out_b = approx_b$y[which(!is.na(approx_b$y))]
        y_min = pmin(approx_a$y, approx_b$y)
        x_min = x_out[which(!is.na(y_min))]
        y_min = y_min[which(!is.na(y_min))]
        # compute AUVs and PSO
        AUV_a = trapz(x_out_a, y_out_a)
        AUV_b = trapz(x_out_b, y_out_b)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_a + AUV_b)
        # return
        .(PSO = PSO)
    }, by = cid]

dt.pso = dt.pso[dt.pf, nomatch=0]


###
# examine correlation
cor.test(dt.pso$PSO, dt.pso$CollectivePerformance) # t = 9.3655e-05, df = 14, p-value = 0.9999
cor.test(dt.pso$PSO, dt.pso$CollectiveBenefit) # t = 1.2847, df = 14, p-value = 0.2197
###
cor.test(dt.pso$PSO, dt.pso$BestParticipantPerformance) # t = -2.0201, df = 14, p-value = 0.06293*
###
cor.test(dt.pso$PSO, dt.pso$WorstParticipantPerformance) # t = -0.25421, df = 14, p-value = 0.803
cor.test(dt.pso$PSO, dt.pso$ConfidenceAlignment) # t = 0.086107, df = 14, p-value = 0.9326
cor.test(dt.pso$PSO, dt.pso$ConfidenceConvergence) # t = 0.40973, df = 14, p-value = 0.6882
