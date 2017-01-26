#!/usr/bin/python
# Compute the per-word entropy of utterances in data/pair*.csv files
# Yang Xu
# 1/26/2017

from srilm import *
import sys
import glob
import math

if __name__ == '__main__':
    # load lm
    lm_file = 'data/train_set.lm'
    lm = initLM(3)
    readLM(lm, lm_file)

    # compute
    csv_files = glob.glob('data/pair*.csv')
    results = []
    for i, f in enumerate(csv_files):
        with open(f, 'r') as fr:
            for line in fr:
                items = line.split(',')
                speaker = items[0]
                sent = items[1]
                # compute
                try:
                    ppl = getSentencePpl(lm, sent, int(items[2]))
                    ent = math.log(ppl, 10)
                except Exception as e:
                    print 'Problematic sentence: %s' % sent
                    raise
                else:
                    results.append((str(i+1), speaker, str(ent)))
        # print
        sys.stdout.write('\r%s/%s' % (i+1, len(csv_files)))
        sys.stdout.flush()

    # write resutls to file
    with open('data/all_pairs_entropy.txt', 'w') as fw:
        for row in results:
            fw.write(','.join(row) + '\n')
