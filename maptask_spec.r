# Analyze the spectral density of maptask entropy series
# Yang Xu
# 11/20/2016

library(data.table)
library(ggplot2)
library(lme4)
library(MASS)
library(pracma)


##
# experiment with toy data
x = seq(0, 100, .01)
y = sin(5*pi * x) + sin(pi * x) + sin(.5*pi * x)

df = data.frame(time = x, value = y)
p = ggplot(df, aes(x = time, y = value)) + geom_point()

# raw periodogram
spec.pgram(y, taper=0, log='no')
# with smooth
spec.pgram(x, spans=9, taper = 0, log="no")


##########
# analyze map task data
#
dt = readRDS('map.dt.rds')

# use one observation as example
obsv = dt$observation[1]
ent_f = dt[observation == obsv & utterID<=100 & who == 'f', ent]
ent_g = dt[observation == obsv & utterID<=100 & who == 'g', ent]

specval_f = spec.pgram(ent_f, taper=0, log="no")
specval_g = spec.pgram(ent_g, taper=0, log="no")

# smoothed
specval_sm_f = spec.pgram(ent_f, spans=c(9,9), taper=0, log="no")
specval_sm_g = spec.pgram(ent_g, spans=c(9,9), taper=0, log="no")

##
# average overall the observations
setkey(dt, observation, who)

dt.ent_raw = dt[utterID<=100, {
        if (.N > 0) {
            specval = spec.pgram(ent, taper=0, log="no")
            .(spec = specval$spec, freq = specval$freq)
        }
    }, by = .(observation, who)]

dt.ent_smooth1 = dt[utterID<=100, {
        if (.N > 6) {
            specval = spec.pgram(ent, spans=c(3,3), taper=0, log="no")
            .(spec = specval$spec, freq = specval$freq)
        }
    }, by = .(observation, who)]


# aggregated plot
p1 = ggplot(dt.ent_smooth1[who == 'f',], aes(x = freq, y = spec)) +
    geom_line(aes(color=observation)) +
    guides(color=F)

p2 = ggplot(dt.ent_smooth1[who == 'g',], aes(x = freq, y = spec)) +
    geom_line(aes(color=observation)) +
    guides(color=F)


##
# compute the position of min and max for all observations
dt.ent_raw_minmax = dt.ent_raw[, {
        i_max = which(diff(sign(diff(spec)))<0) + 1
        freq_max = ifelse(length(i_max)>0, mean(freq[i_max]), NA)
        i_min = which(diff(sign(diff(spec)))>0) + 1
        freq_min = ifelse(length(i_min)>0, mean(freq[i_min]), NA)
        .(maxFreq = freq_max, minFreq = freq_min)
    }, by = .(observation, who)]

t.test(dt.ent_raw_minmax[who=='f', maxFreq], dt.ent_raw_minmax[who=='g', maxFreq]) # t = 1.134, p = 0.2579
t.test(dt.ent_raw_minmax[who=='f', minFreq], dt.ent_raw_minmax[who=='g', minFreq]) # t = 2.8547**

dt.ent_raw_minmax[, maxmindiff := abs(maxFreq - minFreq)]
t.test(dt.ent_raw_minmax[who=='f', maxmindiff], dt.ent_raw_minmax[who=='g', maxmindiff]) # t = 1.3961, p = 0.1639

# diff between f and g
dt.ent_raw_diff = dt.ent_raw_minmax[, {
        maxFreqF = maxFreq[which(who=='f')]
        minFreqF = minFreq[which(who=='f')]
        maxFreqG = maxFreq[which(who=='g')]
        minFreqG = minFreq[which(who=='g')]
        maxDiff = abs(maxFreqF - maxFreqG)
        minDiff = abs(minFreqF - minFreqG)
        .(maxFreqF=maxFreqF, minFreqF=minFreqF, maxFreqG=maxFreqG, minFreqG=minFreqG, maxDiff=maxDiff, minDiff=minDiff)
    }, by = observation]
# join with dt
setkey(dt, observation)
dt.ent_raw_diff = dt.ent_raw_diff[unique(dt[,.(observation, resultSize)]),]



##########
#
# read the dataset that contains pathdev info
dt.dev = fread('moves_and_deviation.csv')
setnames(dt.dev, "Observation", "observation")
setnames(dt.dev, 'path dev', 'pathdev')
setkey(dt.dev, observation)

# join with dt.dev
dt.ent_raw_diff = dt.ent_raw_diff[dt.dev[, .(observation, pathdev)], nomatch=0]


# model
m1 = lm(pathdev ~ maxFreqF + minFreqF + maxFreqG + minFreqG + maxDiff + minDiff, dt.ent_raw_diff)
summary(m1)
# maxDiff: t = 2.196  p = 0.0302 *
# stepwise to select variables
step = stepAIC(m1, direction='both')
step$anova
# maxDiff is left

# optimal model
m2 = lm(pathdev ~ maxDiff, dt.ent_raw_diff)
summary(m2)
# maxDiff: t = 2.213, p = 0.0289 *, Adjusted R-squared: 0.03307

m2_1 = lm(pathdev ~ log(maxDiff), dt.ent_raw_diff)
summary(m2_1) # p = 0.0891.

m2_2 = lm(log(pathdev) ~ maxDiff, dt.ent_raw_diff)
summary(m2_2) # n.s.

# regression plot
p = ggplot(dt.ent_raw_diff, aes(x = maxDiff, y = pathdev)) +
    geom_point() +
    geom_smooth(method = lm)
pdf('maxdiff_vs_pathdev.pdf', 5, 5)
plot(p)
dev.off()


###
# same analysis for smoothed spectral plot
dt.ent_smooth1_maxfreq = dt.ent_smooth1[, {
        i_max = which(diff(sign(diff(spec)))<0) + 1
        freq_max = ifelse(length(i_max)>0, mean(freq[i_max]), NA)
        .(maxFreq = freq_max)
    }, by = .(observation, who)]

dt.ent_smooth1_diff = dt.ent_smooth1_maxfreq[, {
        maxFreqF = maxFreq[which(who=='f')]
        maxFreqG = maxFreq[which(who=='g')]
        maxDiff = abs(maxFreqF - maxFreqG)
        .(maxFreqF=maxFreqF, maxFreqG=maxFreqG, maxDiff=maxDiff)
    }, by = observation]

dt.ent_smooth1_diff = dt.ent_smooth1_diff[dt.dev[, .(observation, pathdev)], nomatch=0]

# model
m3 = lm(pathdev ~ maxFreqF + maxFreqG + maxDiff, dt.ent_smooth1_diff)
summary(m3)
# maxDiff: t = 1.779   p = 0.0779 . marginal significant
step  =stepAIC(m3, direction='both')
step$anova
# maxDiff is left

m4 = lm(pathdev ~ maxDiff, dt.ent_smooth1_diff)
summary(m4)
# t = 1.919   p = 0.0575 .  Adjusted R-squared:  0.023


#########
# test
summary(lmer(spec ~ who + (1|observation), dt.ent_raw)) # t = -10.78 (whog)
t.test(dt.ent_raw[who=='f', spec], dt.ent_raw[who=='g', spec]) # t = 9.8647, f > g

summary(lmer(spec ~ who + (1|observation), dt.ent_smooth1)) # t = -24.22 (whog)
t.test(dt.ent_smooth1[who=='f', spec], dt.ent_smooth1[who=='g', spec]) # t = 18.91, f > g



#######
# compute the area under curve (AUV) for spectral plots
# and then compute the ratio of the common area (power spectral overlap, PSO)

# examine whether the freq series of f and f are identical for each observation
dt.test = dt.ent_raw[, identical(freq[who=='f'], freq[who=='g']), by = observation]
# summary(dt.test$V1)
# Mode   FALSE    TRUE    NA's
# logical     118      10       0

dt.ent_pso = dt.ent_raw[, {
        x_g = freq[who=='g']
        y_g = spec[who=='g']
        x_f = freq[who=='f']
        y_f = spec[who=='f']
        # linear interpolation
        x_out = sort(union(x_g, x_f))
        approx_g = approx(x_g, y_g, xout = x_out)
        approx_f = approx(x_f, y_f, xout = x_out)
        # find min ys and remove NAs
        x_out_g = x_out[which(!is.na(approx_g$y))]
        y_out_g = approx_g$y[which(!is.na(approx_g$y))]
        x_out_f = x_out[which(!is.na(approx_f$y))]
        y_out_f = approx_f$y[which(!is.na(approx_f$y))]
        y_min = pmin(approx_g$y, approx_f$y)
        x_min = x_out[which(!is.na(y_min))]
        y_min = y_min[which(!is.na(y_min))]
        # compute AUVs and PSO
        AUV_g = trapz(x_out_g, y_out_g)
        AUV_f = trapz(x_out_f, y_out_f)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_g + AUV_f)
        # return PSO
        .(PSO = PSO, AUVg = AUV_g, AUVf = AUV_f, AUVmin = AUV_min)
    }, by = observation]

# join with dt.dev
dt.ent_pso = dt.ent_pso[dt.dev[, .(observation, pathdev)], nomatch=0]

# models
m5 = lm(pathdev ~ PSO + AUVf + AUVg + AUVmin, dt.ent_pso)
summary(m5)

m6 = lm(pathdev ~ PSO, dt.ent_pso)
summary(m6)



########
# apply PSO method on sentence length
dt.len_raw = dt[, {
        specval = spec.pgram(tokenNum, taper=0, log="no")
        .(spec = specval$spec, freq = specval$freq)
    }, by = .(observation, who)]

dt.len_pso = dt.len_raw[, {
        x_g = freq[who=='g']
        y_g = spec[who=='g']
        x_f = freq[who=='f']
        y_f = spec[who=='f']
        # linear interpolation
        x_out = sort(union(x_g, x_f))
        approx_g = approx(x_g, y_g, xout = x_out)
        approx_f = approx(x_f, y_f, xout = x_out)
        # find min ys and remove NAs
        x_out_g = x_out[which(!is.na(approx_g$y))]
        y_out_g = approx_g$y[which(!is.na(approx_g$y))]
        x_out_f = x_out[which(!is.na(approx_f$y))]
        y_out_f = approx_f$y[which(!is.na(approx_f$y))]
        y_min = pmin(approx_g$y, approx_f$y)
        x_min = x_out[which(!is.na(y_min))]
        y_min = y_min[which(!is.na(y_min))]
        # compute AUVs and PSO
        AUV_g = trapz(x_out_g, y_out_g)
        AUV_f = trapz(x_out_f, y_out_f)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_g + AUV_f)
        # return PSO
        .(PSO = PSO, AUVg = AUV_g, AUVf = AUV_f, AUVmin = AUV_min)
    }, by = observation]

dt.len_pso = dt.len_pso[dt.dev, nomatch=0]

m1 = lm(pathdev ~ PSO, dt.len_pso)
summary(m1)
# t = 2.413, p = 0.01743 * , Adjusted R-squared:  0.04058

m = lm(acknowledge ~ PSO, dt.len_pso)
summary(m)
# 2.550   0.0121 *

m = lm(check ~ PSO, dt.len_pso)
summary(m)
# 4.086 8.23e-05 ***

m = lm(clarify ~ PSO, dt.len_pso)
summary(m)
# 2.179   0.0314 *

m = lm(explain ~ PSO, dt.len_pso)
summary(m)
# 4.828 4.35e-06 ***

m = lm(instruct ~ PSO, dt.len_pso)
summary(m)
# 2.352   0.0204 *

m = lm(query_w ~ PSO, dt.len_pso)
summary(m)
# 3.102  0.00243 **

m = lm(query_yn ~ PSO, dt.len_pso)
summary(m)
# 2.648  0.00925 **

m = lm(ready ~ PSO, dt.len_pso)
summary(m)
# 2.176   0.0316 *

m = lm(reply_n ~ PSO, dt.len_pso)
summary(m)
# 3.612 0.000455 ***

m = lm(reply_w ~ PSO, dt.len_pso)
summary(m)
# 4.067 8.83e-05 ***

m = lm(reply_y ~ PSO, dt.len_pso)
summary(m)
# 2.437  0.01638 *

m = lm(TotalMoves ~ PSO, dt.len_pso)
summary(m)
# 3.554 0.000555 ***

m = lm(struc.rep ~ PSO, dt.len_pso)
summary(m)
# n.s.

m = lm(struc.rep.norm ~ PSO, dt.len_pso)
summary(m)
# n.s.

m = lm(align.1 ~ PSO, dt.len_pso)
summary(m)
# n.s.

m = lm(align.norm ~ PSO, dt.len_pso)
summary(m)
# n.s.



########
# read `ent_swbd` column from db
require(RMySQL)
# ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
conn = dbConnect(MySQL(), host = '127.0.0.1', user = 'yang', port = 1234, password = "05012014", dbname = 'map')
sql = 'select observation, utterID, who, ent_swbd from utterances'
df.db = dbGetQuery(conn, sql)
dt.db = data.table(df.db
# save
saveRDS(dt.db, 'map.dt.ent_swbd.rds')
# read
dt.db = readRDS('map.dt.ent_swbd.rds')


setkey(dt.db, observation, who)
dt.ent_swbd = dt.db[, {
        if (.N > 0) {
            specval = spec.pgram(ent_swbd, taper=0)
            .(spec = specval$spec, freq = specval$freq)
        }
    }, by = .(observation, who)]

dt.ent_swbd_pso = dt.ent_swbd[, {
        x_g = freq[who=='g']
        y_g = spec[who=='g']
        x_f = freq[who=='f']
        y_f = spec[who=='f']
        # linear interpolation
        x_out = sort(union(x_g, x_f))
        approx_g = approx(x_g, y_g, xout = x_out)
        approx_f = approx(x_f, y_f, xout = x_out)
        # find min ys and remove NAs
        x_out_g = x_out[which(!is.na(approx_g$y))]
        y_out_g = approx_g$y[which(!is.na(approx_g$y))]
        x_out_f = x_out[which(!is.na(approx_f$y))]
        y_out_f = approx_f$y[which(!is.na(approx_f$y))]
        y_min = pmin(approx_g$y, approx_f$y)
        x_min = x_out[which(!is.na(y_min))]
        y_min = y_min[which(!is.na(y_min))]
        # compute AUVs and PSO
        AUV_g = trapz(x_out_g, y_out_g)
        AUV_f = trapz(x_out_f, y_out_f)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_g + AUV_f)
        # return PSO
        .(PSO = PSO, AUVg = AUV_g, AUVf = AUV_f, AUVmin = AUV_min)
    }, by = observation]

dt.ent_swbd_pso = dt.ent_swbd_pso[dt.dev[, .(observation, pathdev)], nomatch=0]

m1 = lm(pathdev ~ PSO, dt.ent_swbd_pso)
summary(m1)
# 2.602   0.0105 *



########
# phase shift experiment
dt.ent_maxPS = dt.db[, {
        y_g = ent_swbd[who=='g']
        y_f = ent_swbd[who=='f']
        len = min(length(y_g), length(y_f))
        y_g = y_g[1:len]
        y_f = y_f[1:len]
        comb.ts = ts(matrix(c(y_g, y_f), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no')
        # phase shift at max spec
        maxPS_g = spec$phase[,1][which(spec$spec[,1]==max(spec$spec[,1]))]
        maxPS_f = spec$phase[,1][which(spec$spec[,2]==max(spec$spec[,2]))]
        # return
        .(maxPSg = maxPS_g, maxPSf = maxPS_f)
    }, by = observation]

dt.ent_peakPSg = dt.db[, {
        y_g = ent_swbd[who=='g']
        y_f = ent_swbd[who=='f']
        len = min(length(y_g), length(y_f))
        y_g = y_g[1:len]
        y_f = y_f[1:len]
        comb.ts = ts(matrix(c(y_g, y_f), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no')
        # phase shift at all peaks
        i_max_g = which(diff(sign(diff(spec$spec[,1])))<0) + 1
        peakPS_g = spec$phase[,1][i_max_g]
        # return
        .(peakPSg = peakPS_g, peakPSg_mean = mean(peakPS_g))
    }, by = observation]

dt.ent_peakPSf = dt.db[, {
        y_g = ent_swbd[who=='g']
        y_f = ent_swbd[who=='f']
        len = min(length(y_g), length(y_f))
        y_g = y_g[1:len]
        y_f = y_f[1:len]
        comb.ts = ts(matrix(c(y_g, y_f), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no')
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


# survey
