# Examine the distribution of entropy in DJD, and how entropy outliters influence the effect of PSO
# Yang Xu
# 1/31/2017

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
# distribution
p = ggplot(dt, aes(x=ent)) + geom_density()

# mean and sd
ent.mean = mean(dt$ent)
ent.sd = sd(dt$ent)

# remove small outliers
dt.rmsmo = copy(dt)
dt.rmsmo[ent < ent.mean - 2*ent.sd, ent := ent.mean]
p = ggplot(dt.rmsmo, aes(x=ent)) + geom_density()

dt.rmsmo.spec = dt.rmsmo[, {
        specval = spec.pgram(ent, taper=0, log='no', plot=F)
        .(spec = specval$spec, freq = specval$freq)
    }, by = .(pairId, who)]
dt.rmsmo.pso = dt.rmsmo.spec[, {
        x_A = freq[who=='A']
        y_A = spec[who=='A']
        x_B = freq[who=='B']
        y_B = spec[who=='B']
        # linear interpolation
        x_out = sort(union(x_A, x_B))
        approx_A = approx(x_A, y_A, xout = x_out)
        approx_B = approx(x_B, y_B, xout = x_out)
        # find min ys and remove NAs
        x_out_A = x_out[!is.na(approx_A$y)]
        y_out_A = approx_A$y[!is.na(approx_A$y)]
        x_out_B = x_out[!is.na(approx_B$y)]
        y_out_B = approx_B$y[!is.na(approx_B$y)]
        y_min = pmin(approx_A$y, approx_B$y)
        x_min = x_out[!is.na(y_min)]
        y_min = y_min[!is.na(y_min)]
        # compute AUVs and PSO
        AUV_A = trapz(x_out_A, y_out_A)
        AUV_B = trapz(x_out_B, y_out_B)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_A + AUV_B)
        # return PSO
        .(PSO = PSO)
    }, by = pairId]
dt.rmsmo.pso = dt.rmsmo.pso[dt.pf, nomatch=0]

m = lm(CollectivePerformance ~ PSO, dt.rmsmo.pso)
summary(m)
# n.s.
# It seems that small entropy outliers in DJD matters a lot in determining the predicting power of PSO


#########
# what if we remove the big outliers
dt.rmbgo = copy(dt)
dt.rmbgo[ent > ent.mean + 2*ent.sd, ent := ent.mean]

dt.rmbgo.spec = dt.rmbgo[, {
        specval = spec.pgram(ent, taper=0, log='no', plot=F)
        .(spec = specval$spec, freq = specval$freq)
    }, by = .(pairId, who)]
dt.rmbgo.pso = dt.rmbgo.spec[, {
        x_A = freq[who=='A']
        y_A = spec[who=='A']
        x_B = freq[who=='B']
        y_B = spec[who=='B']
        # linear interpolation
        x_out = sort(union(x_A, x_B))
        approx_A = approx(x_A, y_A, xout = x_out)
        approx_B = approx(x_B, y_B, xout = x_out)
        # find min ys and remove NAs
        x_out_A = x_out[!is.na(approx_A$y)]
        y_out_A = approx_A$y[!is.na(approx_A$y)]
        x_out_B = x_out[!is.na(approx_B$y)]
        y_out_B = approx_B$y[!is.na(approx_B$y)]
        y_min = pmin(approx_A$y, approx_B$y)
        x_min = x_out[!is.na(y_min)]
        y_min = y_min[!is.na(y_min)]
        # compute AUVs and PSO
        AUV_A = trapz(x_out_A, y_out_A)
        AUV_B = trapz(x_out_B, y_out_B)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_A + AUV_B)
        # return PSO
        .(PSO = PSO)
    }, by = pairId]
dt.rmbgo.pso = dt.rmbgo.pso[dt.pf, nomatch=0]

m = lm(CollectivePerformance ~ PSO, dt.rmbgo.pso)
summary(m)
# PSO          -43.297     16.002  -2.706  0.01706 *
# Adjusted R-squared:  0.2965
# WOW! Big outliers do not matter to PSO
# it is totally contrary to Map Task
# Low entropy utterances determines here in DJD


##########
# what if we keep the small outliers only, replace the rest with mean
dt.kpsmol = copy(dt)
dt.kpsmol[ent >= ent.mean - 2*ent.sd, ent := ent.mean]

dt.kpsmol.spec = dt.kpsmol[, {
        specval = spec.pgram(ent, taper=0, log='no', plot=F)
        .(spec = specval$spec, freq = specval$freq)
    }, by = .(pairId, who)]
dt.kpsmol.pso = dt.kpsmol.spec[, {
        x_A = freq[who=='A']
        y_A = spec[who=='A']
        x_B = freq[who=='B']
        y_B = spec[who=='B']
        # linear interpolation
        x_out = sort(union(x_A, x_B))
        approx_A = approx(x_A, y_A, xout = x_out)
        approx_B = approx(x_B, y_B, xout = x_out)
        # find min ys and remove NAs
        x_out_A = x_out[!is.na(approx_A$y)]
        y_out_A = approx_A$y[!is.na(approx_A$y)]
        x_out_B = x_out[!is.na(approx_B$y)]
        y_out_B = approx_B$y[!is.na(approx_B$y)]
        y_min = pmin(approx_A$y, approx_B$y)
        x_min = x_out[!is.na(y_min)]
        y_min = y_min[!is.na(y_min)]
        # compute AUVs and PSO
        AUV_A = trapz(x_out_A, y_out_A)
        AUV_B = trapz(x_out_B, y_out_B)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_A + AUV_B)
        # return PSO
        .(PSO = PSO)
    }, by = pairId]
dt.kpsmol.pso = dt.kpsmol.pso[dt.pf, nomatch=0]

m = lm(CollectivePerformance ~ PSO, dt.kpsmol.pso)
summary(m)
# n.s.
# PSO           -1.830     14.839  -0.123    0.904
# maybe we remove too many


##########
# keep entropies that are low (below mean - sd)
dt.kplo = copy(dt)
dt.kplo[ent >= ent.mean - ent.sd, ent := ent.mean]

dt.kplo.spec = dt.kplo[, {
        specval = spec.pgram(ent, taper=0, log='no', plot=F)
        .(spec = specval$spec, freq = specval$freq)
    }, by = .(pairId, who)]
dt.kplo.pso = dt.kplo.spec[, {
        x_A = freq[who=='A']
        y_A = spec[who=='A']
        x_B = freq[who=='B']
        y_B = spec[who=='B']
        # linear interpolation
        x_out = sort(union(x_A, x_B))
        approx_A = approx(x_A, y_A, xout = x_out)
        approx_B = approx(x_B, y_B, xout = x_out)
        # find min ys and remove NAs
        x_out_A = x_out[!is.na(approx_A$y)]
        y_out_A = approx_A$y[!is.na(approx_A$y)]
        x_out_B = x_out[!is.na(approx_B$y)]
        y_out_B = approx_B$y[!is.na(approx_B$y)]
        y_min = pmin(approx_A$y, approx_B$y)
        x_min = x_out[!is.na(y_min)]
        y_min = y_min[!is.na(y_min)]
        # compute AUVs and PSO
        AUV_A = trapz(x_out_A, y_out_A)
        AUV_B = trapz(x_out_B, y_out_B)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_A + AUV_B)
        # return PSO
        .(PSO = PSO)
    }, by = pairId]
dt.kplo.pso = dt.kplo.pso[dt.pf, nomatch=0]

m = lm(CollectivePerformance ~ PSO, dt.kplo.pso)
summary(m)
# n.s.
# PSO           -13.08      18.07  -0.724    0.481


#########
# keep entropies that are below mean
dt.belowmean = copy(dt)
dt.belowmean[ent >= ent.mean, ent := ent.mean]

dt.belowmean.spec = dt.belowmean[, {
        specval = spec.pgram(ent, taper=0, log='no', plot=F)
        .(spec = specval$spec, freq = specval$freq)
    }, by = .(pairId, who)]
dt.belowmean.pso = dt.belowmean.spec[, {
        x_A = freq[who=='A']
        y_A = spec[who=='A']
        x_B = freq[who=='B']
        y_B = spec[who=='B']
        # linear interpolation
        x_out = sort(union(x_A, x_B))
        approx_A = approx(x_A, y_A, xout = x_out)
        approx_B = approx(x_B, y_B, xout = x_out)
        # find min ys and remove NAs
        x_out_A = x_out[!is.na(approx_A$y)]
        y_out_A = approx_A$y[!is.na(approx_A$y)]
        x_out_B = x_out[!is.na(approx_B$y)]
        y_out_B = approx_B$y[!is.na(approx_B$y)]
        y_min = pmin(approx_A$y, approx_B$y)
        x_min = x_out[!is.na(y_min)]
        y_min = y_min[!is.na(y_min)]
        # compute AUVs and PSO
        AUV_A = trapz(x_out_A, y_out_A)
        AUV_B = trapz(x_out_B, y_out_B)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_A + AUV_B)
        # return PSO
        .(PSO = PSO)
    }, by = pairId]
dt.belowmean.pso = dt.belowmean.pso[dt.pf, nomatch=0]

m = lm(CollectivePerformance ~ PSO, dt.belowmean.pso)
summary(m)
# n.s.
# PSO          -23.112     18.177  -1.271   0.2243


#########
# keep the entropies that are below (mean + sd)
dt.blwmeanplus1sd = copy(dt)
dt.blwmeanplus1sd[ent >= ent.mean + ent.sd, ent := ent.mean]

dt.blwmeanplus1sd.spec = dt.blwmeanplus1sd[, {
        specval = spec.pgram(ent, taper=0, log='no', plot=F)
        .(spec = specval$spec, freq = specval$freq)
    }, by = .(pairId, who)]
dt.blwmeanplus1sd.pso = dt.blwmeanplus1sd.spec[, {
        x_A = freq[who=='A']
        y_A = spec[who=='A']
        x_B = freq[who=='B']
        y_B = spec[who=='B']
        # linear interpolation
        x_out = sort(union(x_A, x_B))
        approx_A = approx(x_A, y_A, xout = x_out)
        approx_B = approx(x_B, y_B, xout = x_out)
        # find min ys and remove NAs
        x_out_A = x_out[!is.na(approx_A$y)]
        y_out_A = approx_A$y[!is.na(approx_A$y)]
        x_out_B = x_out[!is.na(approx_B$y)]
        y_out_B = approx_B$y[!is.na(approx_B$y)]
        y_min = pmin(approx_A$y, approx_B$y)
        x_min = x_out[!is.na(y_min)]
        y_min = y_min[!is.na(y_min)]
        # compute AUVs and PSO
        AUV_A = trapz(x_out_A, y_out_A)
        AUV_B = trapz(x_out_B, y_out_B)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_A + AUV_B)
        # return PSO
        .(PSO = PSO)
    }, by = pairId]
dt.blwmeanplus1sd.pso = dt.blwmeanplus1sd.pso[dt.pf, nomatch=0]

m = lm(CollectivePerformance ~ PSO, dt.blwmeanplus1sd.pso)
summary(m)
# PSO           -7.807     30.541  -0.256    0.802
# looks like the high entropy utterances are also important in PSO


##########
# what if we replace the entropy in the middle with mean
dt.rplmid = copy(dt)
dt.rplmid[ent <= ent.mean + ent.sd & ent >= ent.mean - ent.sd, ent := ent.mean]
nrow(dt.rplmid[ent == ent.mean,]) / nrow(dt) # 71.6%

dt.rplmid.spec = dt.rplmid[, {
        specval = spec.pgram(ent, taper=0, log='no', plot=F)
        .(spec = specval$spec, freq = specval$freq)
    }, by = .(pairId, who)]
dt.rplmid.pso = dt.rplmid.spec[, {
        x_A = freq[who=='A']
        y_A = spec[who=='A']
        x_B = freq[who=='B']
        y_B = spec[who=='B']
        # linear interpolation
        x_out = sort(union(x_A, x_B))
        approx_A = approx(x_A, y_A, xout = x_out)
        approx_B = approx(x_B, y_B, xout = x_out)
        # find min ys and remove NAs
        x_out_A = x_out[!is.na(approx_A$y)]
        y_out_A = approx_A$y[!is.na(approx_A$y)]
        x_out_B = x_out[!is.na(approx_B$y)]
        y_out_B = approx_B$y[!is.na(approx_B$y)]
        y_min = pmin(approx_A$y, approx_B$y)
        x_min = x_out[!is.na(y_min)]
        y_min = y_min[!is.na(y_min)]
        # compute AUVs and PSO
        AUV_A = trapz(x_out_A, y_out_A)
        AUV_B = trapz(x_out_B, y_out_B)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_A + AUV_B)
        # return PSO
        .(PSO = PSO)
    }, by = pairId]
dt.rplmid.pso = dt.rplmid.pso[dt.pf, nomatch=0]

m = lm(CollectivePerformance ~ PSO, dt.rplmid.pso)
summary(m)
##
# PSO          -34.190     12.921  -2.646  0.01918 *
# Adjusted R-squared:  0.2858
# the extreme entropy values are important to the PSO analysis

# how does the distr look now?
p = ggplot(dt.rplmid, aes(x=ent)) + geom_density()



############
# Can we only use extreme valus to analyze phase shift effect
dt.rplmid = copy(dt)
dt.rplmid[ent <= ent.mean + ent.sd & ent >= ent.mean - ent.sd, ent := ent.mean]

dt.rplmid.peakPS = dt.rplmid[, {
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
dt.rplmid.peakPS = dt.rplmid.peakPS[dt.pf, nomatch=0]

m = lm(CollectivePerformance ~ abs(peakPS), dt.rplmid.peakPS)
summary(m)
# 0.275    0.783
# n.s.
# seems that relative phase requires all entropy to be present
