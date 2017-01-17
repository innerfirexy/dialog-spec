# Examine the words that are of certain entropy levels
# Yang Xu
# 1/16/2017

import MySQLdb
import sys
import numpy as np
import pandas as pd

from nltk.probability import FreqDist


# get db connection
def db_conn(db_name):
    # db init: ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
    conn = MySQLdb.connect(host = "127.0.0.1",
                    user = "yang",
                    port = 1234,
                    passwd = "05012014",
                    db = db_name)
    return conn


##########################
# ent_swbd experiment
##########################
conn = db_conn('map')
cur = conn.cursor()

# create dataframe
sql = 'select tokens, tokenNum, ent_swbd from utterances where clean <>\"\"'
cur.execute(sql)

data = []
for row in cur.fetchall():
    data.append(dict(zip(range(3), row)))

df = pd.DataFrame(data, columns = range(3))
df.columns = ('text', 'length', 'entropy')

# mean and sd of entropy
# df.describe()
ent_mean = np.mean(df.entropy) # 16.21
ent_std = np.std(df.entropy) # 17.50

###
# iterate over `text` column and put all words (unigram, bigram, trigram) into FreqDist obj
unigram_fd1 = FreqDist()
unigram_fd2 = FreqDist()

bigram_fd1 = FreqDist()
bigram_fd2 = FreqDist()

trigram_fd1 = FreqDist()
trigram_fd2 = FreqDist()

# for entropy non-outliers
for i, text in df[df.entropy <= ent_mean + 2*ent_std].text.iteritems():
    words = text.strip().split()
    for w in words:
        unigram_fd1[w] += 1

# for entropy outliers
for i, text in df[df.entropy > ent_mean + 2*ent_std].text.iteritems():
    words = text.strip().split()
    for w in words:
        unigram_fd2[w] += 1
    # get bigrams
    pass
    # get trigrams
    pass
