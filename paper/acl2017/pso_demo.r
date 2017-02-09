# Demo the computation of PSO
# Yang Xu
# 2/4/2017

library(data.table)
library(ggplot2)

f = seq(0, .5, .01)
s1 = dnorm(f, .2, .05)
s2 = dnorm(f, .3, .05)

d.plot = rbindlist(list(
        data.table(Frequency = f, Power = s1, Spectrum = 'Pa'),
        data.table(Frequency = f, Power = s2, Spectrum = 'Pb')
    ))
d.common = data.table(Frequency = f, Power = pmin(s1, s2))
d.total = data.table(Frequency = f, Power = pmax(s1, s2))

p = ggplot(d.plot, aes(x=Frequency, y=Power, Group=Spectrum)) +
    geom_line(aes(color=Spectrum), size=1) +
    geom_ribbon(data = d.total, aes(x = Frequency, ymax = Power), ymin=0, fill='red', alpha=.1, inherit.aes=F) +
    geom_ribbon(data = d.common, aes(x = Frequency, ymax = Power), ymin=0, fill='blue', alpha=.2, inherit.aes=F) +
    scale_color_discrete(labels = c(bquote(P[k]^A), bquote(P[k]^B))) +
    annotate('text', x=.25, y=.5, label='Common area') +
    theme_bw() +
    theme(legend.position = c(.9, .75)) +
pdf('pso_demo.pdf', 5, 2.5)
plot(p)
dev.off()
