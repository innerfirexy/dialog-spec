#!/usr/local/bin/python3

# Use timed-units.xml files in maptask corpus to construct time series that have even time intervals
# Yang Xu
# 12/30/2016

from lxml import etree
import sys
import glob
import re

import pandas as pd
import numpy as np


# survey the utterance lengths
def survey_uttlen():
    data_folder = '/Users/yangxu/Documents/HCRC Map Task Corpus/maptask/maptask-xml/'
    tu_files = glob.glob(data_folder + '*.timed-units.xml')

    data = []
    for i, tuf in enumerate(tu_files):
        tree = etree.parse(tuf)
        tus = tree.findall('tu')
        for tu in tus:
            ids = tu.get('id')
            m = re.search(r'[g|f]\.', ids)

            conv_id = ids[:m.start(0)]
            who = m.group(0)[0]
            tu_id = ids[m.end(0):]

            utt_id = tu.get('utt')
            time = float(tu.get('end')) - float(tu.get('start'))
            text = tu.text
            # append to the last row of df
            data.append(dict(zip(range(6), (conv_id, who, tu_id, utt_id, time, text))))

    # create dataframe
    df = pd.DataFrame(data, columns = range(6))
    df.columns = ('convId', 'who', 'tuId', 'uttId', 'time', 'text')
    df.to_csv('model-data/maptask_timeunit_survey.csv', index=False)


##
# split the sequence of utterance into evenly distributed segments
def split_utt(tree, delta=1):
    '''
    tree: an lxml.etree object parsed from a time-unit xml file
    delta: the length of segment, default = 1 second
    '''
    tustream = tree.getroot()
    segments = []
    i = 0
    while i < len(tustream):
        unit = tustream[i]
        # handle the last unit
        if i == len(tustream)-1:
            if unit.tag == 'tu' and unit.text != '':
                segments.append((unit.text))
        # for units in the middle
        seg = []
        if unit.tag == 'tu' and unit.text != '':
            seg.append(unit.text)
        begintime = float(unit.get('start'))
        for j in range(i+1, len(tustream)):
            u = tustream[j]
            btime = float(u.get('start'))
            etime = float(u.get('end'))
            mtime = (btime + etime) / 2
            if mtime - begintime > delta:
                i = j
                break
            else:
                if u.tag == 'tu' and u.text != '':
                    seg.append(u.text)
                if j == len(tustream)-1:
                    i = j+1
        if len(seg)>0:
            segments.append(tuple(seg))
    # return
    return segments

##
# todo:
# remove 's--', 'th--', etc.
# split "it's", "we're" etc.
# indicate the start of utternace


# experiment with split_utt
def split_utt_exp():
    data_folder = '/Users/yangxu/Documents/HCRC Map Task Corpus/maptask/maptask-xml/'
    tu_files = glob.glob(data_folder + '*.timed-units.xml')

    tuf = '/Users/yangxu/Documents/HCRC Map Task Corpus/maptask/maptask-xml/q7ec5.g.timed-units.xml'
    segments = split_utt(etree.parse(tuf))

    resfile = 'model-data/' + re.search(r'q[a-z0-9]+\.[f|g]\..+\.', tuf).group(0) + 'seg.txt'
    with open(resfile, 'w') as fw:
        for row in segments:
            fw.write(' '.join(row) + '\n')


##
# main
if __name__ == '__main__':
    # survey_uttlen()

    split_utt_exp()
