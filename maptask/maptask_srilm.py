# Re-compute sentence entropy using SRILM
# Yang Xu
# 11/28/2016

import MySQLdb
import pickle
import sys
import re
import subprocess
import glob
import os


# get db connection
def db_conn(db_name):
    # db init: ssh yvx5085@brain.ist.psu.edu -i ~/.ssh/id_rsa -L 1234:localhost:3306
    conn = MySQLdb.connect(host = "127.0.0.1",
                    user = "yang",
                    port = 1234,
                    passwd = "05012014",
                    db = db_name)
    return conn
