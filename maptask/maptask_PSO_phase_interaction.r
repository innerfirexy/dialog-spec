# Examine the interaction between PSO and relative phase
# Yang Xu
# 2/2/2017

library(data.table)
library(ggplot2)
library(MASS)
library(pracma)


# read data
dt = readRDS('map.dt.ent_swbd.rds')

# read the dataset that contains pathdev info
dt.dev = fread('moves_and_deviation.csv')
setnames(dt.dev, "Observation", "observation")
setnames(dt.dev, 'path dev', 'pathdev')
setkey(dt.dev, observation)


# get PSO
dt.spec = dt[, {
        specval = spectrum(ent_swbd, taper=0, log='no', plot=FALSE, method='pgram')
        .(spec = as.numeric(specval$spec), freq = specval$freq)
    }, by = .(observation, who)]
dt.pso = dt.spec[, {
        x_g = freq[who=='g']
        y_g = spec[who=='g']
        x_f = freq[who=='f']
        y_f = spec[who=='f']
        # linear interpolation
        x_out = sort(union(x_g, x_f))
        approx_g = approx(x_g, y_g, xout = x_out)
        approx_f = approx(x_f, y_f, xout = x_out)
        # find min ys and remove NAs
        x_out_g = x_out[!is.na(approx_g$y)]
        y_out_g = approx_g$y[!is.na(approx_g$y)]
        x_out_f = x_out[!is.na(approx_f$y)]
        y_out_f = approx_f$y[!is.na(approx_f$y)]
        y_min = pmin(approx_g$y, approx_f$y)
        x_min = x_out[!is.na(y_min)]
        y_min = y_min[!is.na(y_min)]
        # compute AUVs and PSO
        AUV_g = trapz(x_out_g, y_out_g)
        AUV_f = trapz(x_out_f, y_out_f)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_g + AUV_f)
        # return PSO
        .(PSO = PSO)
    }, by = observation]
dt.pso = dt.pso[dt.dev[, .(observation, pathdev)], nomatch=0]

##
# get peakPS
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

##
# join dt.pso and dt.peakPS
dt.join = dt.peakPS[dt.pso[, .(observation, PSO)], nomatch=0]

##
# models
m = lm(pathdev ~ abs(peakPS) * PSO, dt.join)
summary(m)
# the interaction is n.s.
step = stepAIC(m)
step$anova
# Final Model:
# pathdev ~ abs(peakPS) + PSO

m0 = lm(pathdev ~ abs(peakPS) + PSO, dt.join)
summary(m0)
# abs(peakPS)   -1.992      0.976  -2.041   0.0413 *
# PSO          165.380     16.132  10.252   <2e-16 ***
# Adjusted R-squared:  0.03544
