#!/usr/bin/python
# Compute the per-word entropy of utterances in data/pair*.csv files
# Yang Xu
# 1/26/2017

from srilm import *
import sys
import glob
import math
import csv

from nltk.util import ngrams


# compute sentence entropy
# adding <s> to the left, and </s> to the right
def sentence_entropy(lm, text):
    words = text.split()
    if len(words) == 0:
        return None
    trigrams = list(ngrams(words, 3, pad_left=True, left_pad_symbol='<s>', pad_right=True, right_pad_symbol='</s>'))
    trigrams = trigrams[1:-1]
    probs = [-getTrigramProb(lm, ' '.join(gram)) for gram in trigrams]
    ent = float(sum(probs)) / len(probs)
    return ent

##
# the original way of computing sentence entropy:
# use the logarithm of ppl as entropy, used for results in the submission
def compute_old():
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
                    results.append((i+1, speaker, ent))
        # print
        sys.stdout.write('\r%s/%s' % (i+1, len(csv_files)))
        sys.stdout.flush()

    # write resutls to file
    with open('data/all_pairs_entropy.txt', 'w') as f:
        csvwriter = csv.writer(f)
        csvwriter.writerow(['pairId', 'who', 'ent'])
        for row in results:
            csvwriter.writerow(row)


##
# The new way of computing entropy for the camera-ready submission
# used the customed method for the Cognition paper
def compute_new():
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
                    ent = sentence_entropy(lm, sent)
                except Exception as e:
                    print 'Problematic line:\n %s' % line
                    raise
                else:
                    if ent is not None:
                        results.append((i+1, speaker, ent))
        # print
        sys.stdout.write('\r%s/%s' % (i+1, len(csv_files)))
        sys.stdout.flush()

    # write resutls to file
    with open('data/all_pairs_entropy_new.txt', 'w') as f:
        csvwriter = csv.writer(f)
        csvwriter.writerow(['pairId', 'who', 'ent'])
        for row in results:
            csvwriter.writerow(row)



if __name__ == '__main__':
    # compute_old()
    compute_new()
