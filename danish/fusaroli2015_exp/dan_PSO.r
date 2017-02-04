# PSO analysis of the data/all_pairs_entropy.txt
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
dt.spec = dt[, {
        specval = spec.pgram(ent, taper=0, log='no', plot=F)
        .(spec = specval$spec, freq = specval$freq)
    }, by = .(pairId, who)]
dt.pso = dt.spec[, {
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
        y_max = pmax(approx_A$y, approx_B$y)
        x_max = x_out[!is.na(y_max)]
        y_max = y_max[!is.na(y_max)]
        # compute AUVs and PSO
        AUV_A = trapz(x_out_A, y_out_A)
        AUV_B = trapz(x_out_B, y_out_B)
        AUV_min = trapz(x_min, y_min)
        AUV_max = trapz(x_max, y_max)
        # PSO = AUV_min / (AUV_A + AUV_B)
        PSO = AUV_min / AUV_max
        # return PSO
        .(PSO = PSO)
    }, by = pairId]
dt.pso = dt.pso[dt.pf, nomatch=0]

##
# models
m = lm(CollectivePerformance ~ PSO, dt.pso)
summary(m)
# PSO          -23.154      9.011  -2.569  0.02226 *
# Adjusted R-squared:  0.2711
# F-statistic: 6.602 on 1 and 14 DF, p-value: 0.02226

# outlier test
outlierTest(m)
# 1 line is outlier


m = lm(CollectiveBenefit ~ PSO, dt.pso)
summary(m)
# n.s. -1.493   0.1577

m = lm(BestParticipantPerformance ~ PSO, dt.pso)
summary(m)
# n.s. -1.482   0.1606

m = lm(WorstParticipantPerformance ~ PSO, dt.pso)
summary(m)
# PSO          -35.001     10.705  -3.269 0.005592 **
# Adjusted R-squared:  0.3925

m = lm(ConfidenceAlignment ~ PSO, dt.pso)
summary(m)
# n.s. -0.788    0.444

m = lm(ConfidenceConvergence ~ PSO, dt.pso)
summary(m)
# n.s. -1.495    0.157

##########
# plot regression line
p = ggplot(dt.pso, aes(x=PSO, y=CollectivePerformance)) +
    geom_point() +
    geom_smooth(method = lm, color='#D55E00') +
    theme_bw()
pdf('plots/CollectivePerformance_vs_PSO.pdf', 4, 4)
plot(p)
dev.off()

##
# outliers in CollectivePerformance
cp.mean = mean(dt.pso$CollectivePerformance) # 4.680361
cp.sd = sd(dt.pso$CollectivePerformance) # 1.294164
cp.mean + 2*cp.sd # 7.268688
cp.mean - 2*cp.sd # 2.092033

nrow(dt.pso[CollectivePerformance > cp.mean+2*cp.sd,]) # 1
nrow(dt.pso[CollectivePerformance < cp.mean-2*cp.sd,]) # 0

# remove the outliers and fit a new model
m0 = lm(CollectivePerformance ~ PSO, dt.pso[CollectivePerformance <= cp.mean+2*cp.sd])
summary(m0)
# n.s.
# the upper-left point is removed

##
# outliers in PSO
pso.mean = mean(dt.pso$PSO)
pso.sd = sd(dt.pso$PSO)

nrow(dt.pso[PSO > pso.mean + 2*pso.sd,]) # 0
nrow(dt.pso[PSO < pso.mean - 2*pso.sd,]) # 2

m1 = lm(CollectivePerformance ~ PSO, dt.pso[PSO >= pso.mean-2*pso.sd,])
summary(m1)
# n.s.
# same direction but n.s., tow many missing PSO in the middle
# the left-most two points are removed

####
# OK, because of data scarcity, we do not mark outliers in DJD


#####
# plot WorstParticipantPerformance ~ PSO
p = ggplot(dt.pso, aes(x=PSO, y=WorstParticipantPerformance)) +
    geom_point() +
    geom_smooth(method = lm, color='red') +
    theme_bw()
pdf('plots/WorstParticipantPerformance_vs_PSO.pdf', 4, 4)
plot(p)
dev.off()

m2 = lm(WorstParticipantPerformance ~ PSO, dt.pso[PSO >= pso.mean-2*pso.sd,])
summary(m2)





##########################
# what if we use only the first half of the data to predict
##########################
dt.half = dt[, {.(ent = ent[1:(floor(.N/2))])}, by = .(pairId, who)]

dt.half.spec = dt.half[, {
        specval = spec.pgram(ent, taper=0, log='no', plot=F)
        .(spec = specval$spec, freq = specval$freq)
    }, by = .(pairId, who)]
dt.half.pso = dt.half.spec[, {
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
dt.half.pso = dt.half.pso[dt.pf, nomatch=0]

# models
m = lm(CollectivePerformance ~ PSO, dt.half.pso)
summary(m)
# n.s.

m = lm(CollectiveBenefit ~ PSO, dt.half.pso)
summary(m)
# n.s.

m = lm(BestParticipantPerformance ~ PSO, dt.half.pso)
summary(m)
# n.s.

m = lm(WorstParticipantPerformance ~ PSO, dt.half.pso)
summary(m)
# n.s.

m = lm(ConfidenceAlignment ~ PSO, dt.half.pso)
summary(m)
# n.s.

m = lm(ConfidenceConvergence ~ PSO, dt.pso)
summary(m)
# n.s.

## ok, it turns out that the "predicting" ability of PSO is not that good
## PSO is only predictive of the overall performance, when all information is included.
