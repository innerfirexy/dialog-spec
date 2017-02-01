# Analyze the temporal distribution patterns of entropy outliers
# and its relationship with pathdev
# Yang Xu
# 1/23/2017

library(data.table)
library(ggplot2)
library(MASS)
library(pracma)


# read the dataset that contains pathdev info
dt.dev = fread('moves_and_deviation.csv')
setnames(dt.dev, "Observation", "observation")
setnames(dt.dev, 'path dev', 'pathdev')
setkey(dt.dev, observation)

##
# compute the outliers and add label column
dt = readRDS('map.dt.ent_swbd.rds')

ent_swbd.mean = mean(dt$ent_swbd)
ent_swbd.sd = sd(dt$ent_swbd)

dt[,outlier:=F][ent_swbd > ent_swbd.mean + 2*ent_swbd.sd, outlier:=T]

nrow(dt[outlier==T,]) / nrow(dt) # 4.73%

# count the number of outliers per dialogue
dt.count = dt[, {
        outlierN = length(.I[outlier == T])
        .(outlierN = outlierN, outlierRatio = outlierN / .N, dialogLen = .N)
    }, by = observation]

# the relationship between outlierN and pathdev
dt.count = dt.count[dt.dev[, .(observation, pathdev)], nomatch=0]

m = lm(pathdev ~ outlierN + outlierRatio + dialogLen, dt.count)
summary(m)
step = stepAIC(m)
step$anova
# final model, pathdev ~ outlierRatio

m = lm(pathdev ~ outlierRatio, dt.count)
summary(m)
# -2.209   0.0292 *
# Adjusted R-squared:  0.0329
# the more entropy outliers (higher outlier ratio), the better performance


##########
# How does outlier number correlate with PSO
##########
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
dt.pso = dt.pso[dt.count]
# dt.pso = dt.pso[dt.dev[, .(observation, pathdev)]]

m = lm(pathdev ~ PSO, dt.pso)
summary(m)
# 2.602   0.0105 *

m = lm(pathdev ~ PSO + outlierN + outlierRatio + dialogLen, dt.pso)
summary(m)
step = stepAIC(m)
step$anova
# final model, pathdev ~ PSO + dialogLen
m = lm(pathdev ~ PSO + dialogLen, dt.pso)
summary(m)
# PSO         253.28880   79.93163   3.169  0.00197 **
# dialogLen    -0.09630    0.04446  -2.166  0.03242 *
# Adjusted R-squared:  0.07829

##
# what is correlated with PSO
m = lm(PSO ~ outlierN + outlierRatio + dialogLen, dt.pso)
summary(m)
step = stepAIC(m)
step$anova
# Final Model:
# PSO ~ outlierN + outlierRatio
m = lm(PSO ~ outlierN + outlierRatio, dt.pso)
summary(m)
# outlierN      0.0031166  0.0008375   3.721 0.000311 ***
# outlierRatio -1.4858146  0.2306544  -6.442 3.06e-09 ***
# Adjusted R-squared:  0.259
## higher PSO indicates larger outlierN and smaller outlierRatio


##
# what about the mean distance between entropy outliers
dt.dist1 = dt[, {
        index = .I[outlier == T & who == 'g']
        dist_g = ifelse(length(index)>1, (index[length(index)] - index[1]) / (length(index)-1), 0)
        index = .I[outlier == T & who == 'f']
        dist_f = ifelse(length(index)>1, (index[length(index)] - index[1]) / (length(index)-1), 0)
        .(dist_g = dist_g, dist_f = dist_f, dist = (dist_g+dist_f)/2)
    }, by = .(observation)]
dt.dist1 = dt.dist1[dt.pso]

m = lm(PSO ~ dist + dist_g + dist_f + outlierN + outlierRatio, dt.dist1)
summary(m)
step = stepAIC(m)
step$anova
# Final Model:
# PSO ~ dist + outlierN + outlierRatio
m = lm(PSO ~ dist + outlierN + outlierRatio, dt.dist1)
summary(m)
# The average distance among entropy outliers is correlated with PSO

m = lm(pathdev ~ dist, dt.dist1)
summary(m)
# n.s.
# thus, distance per se is not a good predictor


##
# what about using the values of distance between outliers, as predictors
dt.dist2 = dt[, {
        dist = c()
        for (who in c('g', 'f')) {
            index = .I[outlier == T & who == 'g']
            if (length(index) > 1) {
                for (i in 1:(length(index)-1)) {
                    dist = c(dist, index[i+1] - index[i])
                }
            }
        }
        .(dist = dist)
    }, by = .(observation)]
dt.dist2 = dt.dist2[dt.pso]

m = lm(PSO ~ dist, dt.dist2)
summary(m)
# 8.196 7.18e-16 ***
# Therefore, higher PSO indicates larger distance between outliers

m = lm(pathdev ~ PSO + dist, dt.dist2)
summary(m)
# n.s.
# thus, the distance values are not good predictors



########################
# maybe we can find another way to call these entropy outliers
# such as high entropy utternaces
########################
# examine the distr of ent_swbd
p = ggplot(dt, aes(x = ent_swbd)) + geom_density()
pdf('plots/ent_swbd_density.pdf', 5, 5)
plot(p)
dev.off()



########################
# since we already know that removing the entropy outliers will cause the correlation between PSO and pathdev to disappear
# then, how does removing outliers influence the magnitude of PSO at the first place??
########################
dt.rmol = dt[,]
dt.rmol[outlier==T, ent_swbd := ent_swbd.mean,]

dt.rmol.spec = dt.rmol[, {
        specval = spectrum(ent_swbd, taper=0, log='no', plot=FALSE, method='pgram')
        .(spec = as.numeric(specval$spec), freq = specval$freq)
    }, by = .(observation, who)]
dt.rmol.pso = dt.rmol.spec[, {
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
dt.rmol.pso = dt.rmol.pso[dt.dev[, .(observation, pathdev)]]

m = lm(pathdev ~ PSO, dt.rmol.pso)
summary(m)
# n.s.

## compare with dt.pso (entropy outliers remained)
t.test(dt.pso$PSO, dt.rmol.pso$PSO)
# t = -6.4273, df = 188.43, p-value = 1.037e-09
# When outlier removed, the PSO increases (makes sense) , but the correlation with pathdev is gone!!



#########################
# what if we keep the outliers only, and replace the non-outliers with ent.mean
dt.rpnol = copy(dt)
dt.rpnol[outlier==F, ent_swbd := ent_swbd.mean]

dt.rpnol.spec = dt.rpnol[, {
        specval = spectrum(ent_swbd, taper=0, log='no', plot=FALSE, method='pgram')
        .(spec = as.numeric(specval$spec), freq = specval$freq)
    }, by = .(observation, who)]
dt.rpnol.pso = dt.rpnol.spec[, {
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
dt.rpnol.pso = dt.rpnol.pso[dt.dev[, .(observation, pathdev)]]

m = lm(pathdev ~ PSO, dt.rpnol.pso)
summary(m)
###
# PSO           66.743     42.035   1.588    0.115
# although it is n.s., the direction is the same as the case where non-outliers are kept
#


##############
# what if we keep the big entropies (above mean + sd)
dt.bigent = copy(dt)
dt.bigent[ent_swbd <= ent_swbd.mean + ent_swbd.sd, ent_swbd := ent_swbd.mean]

dt.bigent.spec = dt.bigent[, {
        specval = spectrum(ent_swbd, taper=0, log='no', plot=FALSE, method='pgram')
        .(spec = as.numeric(specval$spec), freq = specval$freq)
    }, by = .(observation, who)]
dt.bigent.pso = dt.bigent.spec[, {
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
dt.bigent.pso = dt.bigent.pso[dt.dev[, .(observation, pathdev)]]

m = lm(pathdev ~ PSO, dt.bigent.pso)
summary(m)
# PSO           84.668     52.018   1.628    0.106
# hmmmm, closer to a significant predictor



############################
# OK, what if we keep the entropies that are above mean
dt.abovemean = copy(dt)
dt.abovemean[ent_swbd <= ent_swbd.mean, ent_swbd := ent_swbd.mean]
nrow(dt.abovemean[ent_swbd == ent_swbd.mean,]) / nrow(dt) # 63.0%

dt.abovemean.spec = dt.abovemean[, {
        specval = spectrum(ent_swbd, taper=0, log='no', plot=FALSE, method='pgram')
        .(spec = as.numeric(specval$spec), freq = specval$freq)
    }, by = .(observation, who)]
dt.abovemean.pso = dt.abovemean.spec[, {
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
dt.abovemean.pso = dt.abovemean.pso[dt.dev[, .(observation, pathdev)]]

m = lm(pathdev ~ PSO, dt.abovemean.pso)
summary(m)
# PSO           109.80      56.81   1.933   0.0558 .
# Already marginal significant
# Therefore, the large entropy utterances matter more when using PSO to predict task performance
# (in the case of Map Task)



############
# Can we only use extreme valus to analyze phase shift effect
dt.abovemean = copy(dt)
dt.abovemean[ent_swbd <= ent_swbd.mean, ent_swbd := ent_swbd.mean]
nrow(dt.abovemean[ent_swbd == ent_swbd.mean,]) / nrow(dt) # 63.0%

dt.abovemean.peakPS = dt.abovemean[, {
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
dt.abovemean.peakPS = dt.abovemean.peakPS[dt.dev[, .(observation, pathdev)], nomatch=0]

m = lm(pathdev ~ abs(peakPS), dt.abovemean.peakPS)
summary(m)
# abs(peakPS)  -2.1575     0.9888  -2.182   0.0292 *
# Adjusted R-squared:  0.001284
# It works!
