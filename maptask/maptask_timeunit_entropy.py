#!/usr/bin/python

# Compute segment entropy for even-time series (model-data/even-time/*.txt)
# Yang Xu
# 1/10/2017

from __future__ import print_function
from srilm import *

import glob
import re
import math
import sys


# the function that helps decide utt id, when there are 0s among two ids
def voteUttId(id1, id2):
    if id1 == 0:
        return id2
    else:
        return id1

# return ngram sequence for a list of words
def ngram(words, order=3, symbol='<s>', pad_left=True):
    '''
    words: a list of str
    '''
    assert isinstance(order, int) and order >= 2
    assert isinstance(symbol, str)
    ret = []
    if pad_left:
        for i in range(len(words)):
            if i >= order - 1:
                ret.append(' '.join(words[(i - order + 1) : (i+1)]))
            else:
                gram = [symbol]*(order - 1 - i) + words[:(i+1)]
                ret.append(' '.join(gram))
    else:
        if len(words) >= order:
            for i in range(order-1, len(words)):
                ret.append(' '.join(words[(i - order + 1) : (i+1)]))
        else:
            raise Exception('words length should be larger than order when pad_left is False!')
    return ret


##
# main
if __name__ == '__main__':
    # read lm
    lm_file = 'model-data/training_sentences_swbd.lm'
    lm = initLM(3)
    readLM(lm, lm_file)

    # compute
    test_files = glob.glob('model-data/even-time/*.txt')
    results_dir = 'model-data/even-time-results/'
    for i, tf in enumerate(test_files):
        result = []
        result_file = results_dir + re.search(r'q.+.txt', tf).group(0).replace('seg', 'res')
        pp_row = None # the previous row of previous row
        p_row = None # the previous row

        with open(tf, 'r') as fr:
            # preprocess lines
            lines = fr.readlines()
            items = []
            for line in lines:
                goods = line.split('\t')
                items.append((goods[0].strip(), tuple(goods[0].strip().split()), int(goods[1]), int(goods[2])))
            # process items
            for j, item in enumerate(items):
                if j == 0:
                    # look forward to decide if we treat the current utterance as an independent sentence
                    if voteUttId(item[2], item[3]) == 0 or voteUttId(items[j+1][2], items[j+1][3]) == 0 or voteUttId(item[2], item[3]) != voteUttId(items[j+1][2], items[j+1][3]):
                        ppl = getSentencePpl(lm, item[0], len(item[1]))
                        ent = math.log(ppl, 10)
                        result.append(ent)
                    else:
                        grams = ngram(list(item[1]))
                        ent = -sum([getTrigramProb(lm, gram) for gram in grams])
                        result.append(ent)
                else if j == len(items)-1:
                    if voteUttId(item[2], item[3]) == 0 or voteUttId(items[j-1][2], items[j-1][3]) == 0 or voteUttId(item[2], item[3]) != voteUttId(items[j-1][2], items[j-1][3]):
                        ppl = getSentencePpl(lm, item[0], len(item[1]))
                        ent = math.log(ppl, 10)
                        result.append(ent)
                    else:
                        grams = []
                        if len(items[j-1][1]) >= 2:
                            words = list(items[j-1][1])[-2:] + list(item[1])
                            grams = ngram(words, pad_left=False)
                        else if voteUttId(item[2], item[3]) == voteUttId(items[j-2][2], items[j-2][3]):
                            words = [list(items[j-2][1])[-1], items[j-1][0]] + list(item[1])
                            grams = ngram(words, pad_left=False)
                        else:
                            words = [items[j-1][0]] + list(item[1])
                            grams = ngram(words)
                else:
                    if # same as previous utt:
                        pass
                    else: # same as next utt (treat utt=0 as this case)
                        pass
