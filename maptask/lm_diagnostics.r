# Diagnostics analysis of the linear models
# Yang Xu
# 2/2/2017

library(data.table)
library(car)
library(MASS)

# read data
dt = readRDS('map.dt.ent_swbd.rds')

# read the dataset that contains pathdev info
dt.dev = fread('moves_and_deviation.csv')
setnames(dt.dev, "Observation", "observation")
setnames(dt.dev, 'path dev', 'pathdev')
setkey(dt.dev, observation)


####################
# PSO
####################
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

m = lm(pathdev ~ PSO, dt.pso)

# outliers
outlierTest(m)

# Influence Plot
influencePlot(m, main="Influence Plot", sub="Circle size is proportial to Cook's Distance")


# Normality of Residuals
# qq plot for studentized resid
qqPlot(m, main="QQ Plot")
# distribution of studentized residuals
sresid = studres(m)
hist(sresid, freq=FALSE,
   main="Distribution of Studentized Residuals")
xfit = seq(min(sresid), max(sresid),length=40)
yfit = dnorm(xfit)
lines(xfit, yfit)


# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(m)
# plot studentized residuals vs. fitted values
spreadLevelPlot(m)
# Chisquare = 7.115326    Df = 1     p = 0.00764277
