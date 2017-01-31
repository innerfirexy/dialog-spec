# Examine whether the entropy outliers in maptask correspond to some certain types of moves
# Yang Xu
# 1/30/2017

import MySQLdb
import sys
import glob
import re
from lxml import etree


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
# read `raw` and `ent_swbd` columns from db, and return a dict with unique `raw` as keys
def read_dict(cursor):
    sql = 'select distinct raw, ent_swbd from utterances'
    cursor.execute(sql)
    data = [(row[0], row[1]) for row in cursor.fetchal()]
    return dict(data)

##
# given time_unit_file and time_unit_id, return the corresponding text
def get_tu_text(tree, tu_id):
    '''
    tu_id: str or a list of str
    '''
    if isinstance(tu_id, str):
        tu_id = [tu_id]
    res = []
    for tid in tu_id:
        try:
            tu = tree.find('tu[@id="{}"]'.format(tid))
            if tu is not None:
                res.append(tu.text)
        except Exception as e:
            print('time unit id: {}'.format(tid))
            raise
    return ' '.join(res)

##
# process a move_file and a corresponding tu_file, and return a list of tuples: [(label, text), ...]
def process_files(move_file, tu_file):
    m_tree = etree.parse(move_file)
    tu_tree = etree.parse(tu_file)
    move_stream = m_tree.getroot()
    conv_id = move_stream.attrib['id'][5:-2]
    who = move_stream.attrib['id'][-1]
    moves = move_stream.findall('move')
    res = []
    for move in moves:
        label = move.get('label')
        href = move.get('href')
        matches = re.findall(r'id\([0-9a-z|\.]+\)', href)
        tu_id = [m[3:-1] for m in matches]
        if len(tu_id) > 1:
            m = re.search(r'\.[0-9]+$', tu_id[0])
            start_id = int(m.group(0)[1:])
            m = re.search(r'\.[0-9]+$', tu_id[1])
            end_id = int(m.group(0)[1:])
            head = tu_id[1][:(m.start()+1)]
            if end_id - start_id > 1:
                tu_id = [head + str(i) for i in range(start_id, end_id+1)]
        tu_text = get_tu_text(tu_tree, tu_id)
        res.append((conv_id, who, tu_text, label))
    return res


##
# main
if __name__ == '__main__':
    # data files
    data_dir = '/Users/yangxu/Documents/HCRC Map Task Corpus/maptask/maptask-xml/'
    move_files = glob.glob(data_dir + '*.moves.xml')
    time_unit_files = glob.glob(data_dir + '*.timed-units.xml')

    # run
    results = []
    for i, m_file in enumerate(move_files):
        results += process_files(m_file, time_unit_files[i])
        sys.stdout.write('\r{}/{} processed'.format(i+1, len(move_files)))
        sys.stdout.flush()
    # save
    with open('model-data/move_label.txt', 'w') as fw:
        for row in results:
            fw.write(','.join(row) + '\n')
