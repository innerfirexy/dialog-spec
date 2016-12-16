# Extract utterances from the tsv files provided by Fusaroli
# Yang Xu
# 12/16/2016

import csv

# the func that processes single file
def process_file(in_file, out_file):
    with open(in_file, 'r') as infile, open(out_file, 'w') as outfile:
        reader = csv.reader(infile, delimiter=',')
        next(reader, None) # skip the header

        for row in reader:
            # clean the utterance
            # write to out_file
            outfile.write(row[4] + '\n')


# main
if __name__ == '__main__':
    input_folder = '/Users/yangxu/Documents/Danish_MapTask/'
    output_folder = 'data/'

    in_file = input_folder + 'Pair1.tsv'
    out_file = output_folder + 'pair1.csv'

    process_file(in_file, out_file)
