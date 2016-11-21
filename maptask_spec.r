# Analyze the spectral density of maptask entropy series
# Yang Xu
# 11/20/2016

library(data.table)
library(ggplot2)

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
