#!/usr/local/bin/python3

# Use timed-units.xml files in maptask corpus to construct time series that have even time intervals
# Yang Xu
# 12/30/2016

from lxml import etree
import sys
import glob
import re
import subprocess
import MySQLdb

import pandas as pd
import numpy as np
from nltk.probability import FreqDist


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
        # check the tag, and skip 'noi' and 'sil' units
        if unit.tag != 'tu':
            i += 1
            continue
        ## process 'tu' units only
        # for the last unit
        if i == len(tustream)-1 and unit.text != '':
            if '--' in unit.text:
                break
            beginutt = unit.get('utt')
            beginutt = 0 if beginutt is None else int(beginutt)
            endutt = beginutt
            segments.append((tuple(sp_quote(unit.text)), beginutt, endutt))
            break

        # for units in the middle
        seg = []
        begintime = float(unit.get('start'))
        beginutt = unit.get('utt')
        beginutt = 0 if beginutt is None else int(beginutt)
        endutt = beginutt

        if unit.text != '':
            seg.append(unit.text)
        for j in range(i+1, len(tustream)):
            u = tustream[j]
            # skip non-tu units
            if u.tag != 'tu':
                i = j+1
                break
            # for tu units
            if u.get('utt') is not None and int(u.get('utt')) != beginutt:
                i = j
                break
            btime = float(u.get('start'))
            etime = float(u.get('end'))
            mtime = (btime + etime) / 2
            if mtime - begintime > delta:
                i = j
                break
            else:
                if u.text != '':
                    seg.append(u.text)
                    endutt = 0 if u.get('utt') is None else int(u.get('utt'))
                if j == len(tustream)-1:
                    i = j+1

        # filter out the words that contain '--', and process single quote
        seg = [w for w in seg if '--' not in w]
        seg_new = []
        for word in seg:
            ws = sp_quote(word)
            for w in ws:
                seg_new.append(w)

        if len(seg_new)>0:
            segments.append((tuple(seg_new), beginutt, endutt))
    # return
    return segments

##
# todo:
# remove 's--', 'th--', etc.
# split "it's", "we're" etc.

# the func that split a word if it contains single quote
def sp_quote(word):
    if "\'" in word:
        ind = word.index("\'")
        if ind == 0 or ind == len(word)-1:
            return [word]
        else:
            return [word[:ind], word[ind:]]
    else:
        return [word]


# experiment with split_utt
def split_utt_exp():
    data_folder = '/Users/yangxu/Documents/HCRC Map Task Corpus/maptask/maptask-xml/'
    tu_files = glob.glob(data_folder + '*.timed-units.xml')

    for i, tuf in enumerate(tu_files):
        # tuf = '/Users/yangxu/Documents/HCRC Map Task Corpus/maptask/maptask-xml/q7ec5.g.timed-units.xml'
        try:
            segments = split_utt(etree.parse(tuf), delta=1)
        except Exception as e:
            print('file name:')
            print(tuf)
            raise
        else:
            resfile = 'model-data/even-time/' + re.search(r'q[a-z0-9]+\.[f|g]\..+\.', tuf).group(0) + 'seg.txt'
            with open(resfile, 'w') as fw:
                for row in segments:
                    fw.write(' '.join(row[0]) + '\t' + str(row[1]) + '\t' + str(row[2]) + '\n')
        sys.stdout.write('\r{}/{} processed'.format(i+1, len(tu_files)))
        sys.stdout.flush()


# examine the results files
# in model-data/even-time
def examine_eventime_res():
    resfiles = glob.glob('model-data/even-time/*.txt')
    lexicons = []
    for resf in resfiles:
        with open(resf, 'r') as fr:
            for line in fr:
                words = line.split('\t')[0]
                for w in words.split():
                    lexicons.append(w)
    # count the occurrences of '-'
    count_dash = FreqDist()
    for w in lexicons:
        if '-' in w:
            count_dash[w] += 1
    # count single quote
    count_quote = FreqDist()
    for w in lexicons:
        if "\'" in w:
            count_quote[w] += 1
    # check how many single quotes are there at most
    for key in count_quote.keys():
        if key.count("\'") > 1:
            print(key)
    # it turns out that no word contains more than 1 single quote


###
# get training sentences from Switchboard and train the language model usring srilm
def trainLM():
    conn = db_conn('swbd')
    cursor = conn.cursor()

    # select sentences from db
    sql = 'select rawWord from entropy where rawWord <> \"\"'
    cursor.execute(sql)
    sents = [row[0].lower() for row in cursor.fetchall()]
    print('selecting done')

    # save sentences into file
    sents_file = 'model-data/training_sentences_swbd.txt'
    with open(sents_file, 'w') as fw:
        for s in sents:
            fw.write(s + '\n')
    print('saving done')

    # train langauge model
    lm_file = sents_file[:-3] + 'lm'
    srilm_dir = '/Users/yangxu/projects/srilm-1.7.1/bin/macosx/'
    train_cmd = [srilm_dir + 'ngram-count', '-order', '3', '-text', sents_file, '-lm', lm_file]
    return_code = subprocess.check_call(train_cmd)
    if return_code != 0:
        print('train failure')
        exit()
    print('training done')


# get db connection
def db_conn(db_name):
    # db init: ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
    conn = MySQLdb.connect(host = "127.0.0.1",
                    user = "yang",
                    port = 1234,
                    passwd = "05012014",
                    db = db_name)
    return conn


##
# main
if __name__ == '__main__':
    # survey_uttlen()
    # split_utt_exp()
    trainLM()
