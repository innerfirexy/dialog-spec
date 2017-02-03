# Power Spectral Overlap analysis of maptask
# Yang Xu
# 12/13/2016

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


# compute spectral data
dt.ent_swbd = dt[, {
        if (.N > 0) {
            specval = spec.pgram(ent_swbd, taper=0, log='no', plot=FALSE)
            .(spec = specval$spec, freq = specval$freq)
        }
    }, by = .(observation, who)]

##
# compute the area under curve (AUV) for spectral plots
# and then compute the ratio of the common area (power spectral overlap, PSO)
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
##
# save the first two columns as rds
saveRDS(dt.ent_swbd_pso[,.(observation, PSO)], 'dt.pso.rds')


# models
m1 = lm(pathdev ~ PSO, dt.ent_swbd_pso)
summary(m1)
# 2.602   0.0105 *
# Adjusted R-squared:  0.04818

#####
# plot regression line
p = ggplot(dt.ent_swbd_pso, aes(x = PSO, y = pathdev)) +
    geom_point() +
    geom_smooth(method = lm) + ylab('PATHDEV')
pdf('plots/pathdev_vs_PSO.pdf', 4, 4)
plot(p)
dev.off()



m2 = lm(pathdev ~ PSO + AUVg + AUVf + AUVmin, dt.ent_swbd_pso)
summary(m2)
# n.s. for all factors


###
# PSO analysis of sentence length
dt.len = readRDS('map.dt.rds')
setkey(dt.len, observation, who)
dt.len_raw = dt[, {
        if (.N > 0) {
            specval = spec.pgram(tokenNum, taper=0, log="no")
            .(spec = specval$spec, freq = specval$freq)
        }
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

dt.len_pso = dt.len_pso[dt.dev[, .(observation, pathdev)], nomatch=0]

# models
m1 = lm(pathdev ~ PSO, dt.len_pso)
summary(m1)
# 2.413  0.01743 *
# Adjusted R-squared:  0.04058


###
# compare the effect of entropy and length
dt.ent_len = cbind(dt.len_pso[,.(observation, PSO)], dt.ent_swbd_pso[,.(PSO, pathdev)])
setnames(dt.ent_len, c('observation', 'PSO_len', 'PSO_ent', 'pathdev'))

m1 = lm(pathdev ~ PSO_len + PSO_ent, dt.ent_len)
summary(m1)
# n.s. for both

step = stepAIC(m1, direction='both')
step$anova
# Final Model:
# pathdev ~ PSO_ent

# the PSO of entropy is a better predictor of the PSO of length

##
# plot
p = ggplot(dt.ent_len, aes(x = PSO_ent, y = pathdev)) +
    geom_point() +
    geom_smooth(method = lm)
pdf('pathdev_PSOent.pdf', 5, 5)
plot(p)
dev.off()


###
# explore the relation between PSO and the weighted mean frequency
dt.ent_swbd_wmf = dt.ent_swbd[, {
        wmf_f = weighted.mean(freq[who=='f'], spec[who=='f'])
        wmf_g = weighted.mean(freq[who=='g'], spec[who=='g'])
        .(wmf_f = wmf_f, wmf_g = wmf_g)
    }, by = observation]

dt.ent_swbd_pso = dt.ent_swbd_pso[dt.ent_swbd_wmf, nomatch=0]

m = lm(wmf_f ~ PSO, dt.ent_swbd_pso)
summary(m) # n.s.

m = lm(wmf_g ~ PSO, dt.ent_swbd_pso)
summary(m)
# -2.521   0.0131 *
##
# When PSO is high (larger spectral overlap), the frequency of givers is relatively lower
# corresponding to the slower change of entropy (grounding)

m = lm(abs(wmf_f - wmf_g) ~ PSO, dt.ent_swbd_pso)
summary(m)
# -2.404   0.0178 *

m = lm(pathdev ~ PSO + abs(wmf_f - wmf_g), dt.ent_swbd_pso)
summary(m)
step = stepAIC(m)
step$anova
# final model:
# pathdev ~ PSO


m = lm(wmf_g ~ wmf_f, dt.ent_swbd_pso)
summary(m) # n.s.
t.test(dt.ent_swbd_pso$wmf_g, dt.ent_swbd_pso$wmf_f)
# t = 6.9067 ***
# givers have higher frequency than followers



####
# the relationship between PSO and other annotations
dt.ent_swbd_pso_full = dt.ent_swbd_pso[dt.dev, nomatch=0]

m = lm(acknowledge ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# 2.275   0.0248 *

m = lm(check ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# 3.792 0.000242 ***

m = lm(clarify ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# 1.586   0.1155

m = lm(explain ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# 4.265 4.16e-05 ***

m = lm(instruct ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# 1.882   0.0624 .

m = lm(query_w ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# 2.993  0.00339 **

m = lm(query_yn ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# 2.832  0.00548 **

m = lm(ready ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# 2.070   0.0408 *

m = lm(reply_n ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# 3.414 0.000891 ***

m = lm(reply_w ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# 4.021 0.000105 ***

m = lm(reply_y ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# 2.464   0.0153 *

m = lm(TotalMoves ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# 3.281  0.00138 **

m = lm(struc.rep ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# n.s.
# -1.359    0.177

m = lm(struc.rep.norm ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# n.s.
# -0.815    0.417

m = lm(align.1 ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# n.s.
# 0.033  0.97412

m = lm(align.norm ~ PSO, dt.ent_swbd_pso_full)
summary(m)
# n.s.
# -1.238    0.218
