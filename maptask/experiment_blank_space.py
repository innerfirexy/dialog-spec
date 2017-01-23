# Generate time series in a way that includes blank spaces from the other speaker
# Yang Xu
# 1/23/2017

import MySQLdb
import sys

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
    # db conn
    conn = db_conn('map')
    cur = conn.cursor()

    # select observation ids from db
    sql = 'select distinct observation from utterances'
    cur.execute(sql)
    observIds = [row[0] for row in cur.fetchall()]

    # construct results
    results = []
    for i, obid in enumerate(observIds):
        sql = 'select who, ent_swbd from utterances where observation = %s'
        cur.execute(sql, [obid])
        data = cur.fetchall()
        # append data entries for different speakers
        for row in data:
            if row[0] == 'g':
                results.append((obid, 'g', row[1]))
                results.append((obid, 'f', 0))
            else:
                results.append((obid, 'g', 0))
                results.append((obid, 'f', row[1]))

    # write resutls to file
    with open('model-data/maptask_ent_swbd_withzeros.csv', 'w') as fw:
        for row in results:
            fw.write(','.join(map(str, row)) + '\n')
