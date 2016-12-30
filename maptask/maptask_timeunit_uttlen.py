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
# main
if __name__ == '__main__':
    survey_uttlen()
