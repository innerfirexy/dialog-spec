# Merge the files under data/ folder: pair1.csv through pair16.csv
# Yang Xu
# 12/19/2016

library(data.table)

dt = data.table()
for (i in 1:16) {
    filename = paste0('data/pair', i, '.csv')
    tmp = fread(filename)
    tmp$V2 = NULL
    setnames(tmp, c('who', 'length'))
    tmp$cid = i
    dt = rbindlist(list(dt, tmp))
}

saveRDS(dt, 'dt.merged.rds')
