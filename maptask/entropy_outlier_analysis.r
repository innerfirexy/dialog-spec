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
