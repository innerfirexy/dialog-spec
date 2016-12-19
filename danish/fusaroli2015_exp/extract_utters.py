# Extract utterances from the tsv files provided by Fusaroli
# Yang Xu
# 12/16/2016

import csv
import re
import glob


# the func that processes single file
def process_file(in_file, out_file):
    with open(in_file, 'r') as infile, open(out_file, 'w') as outfile:
        reader = csv.reader(infile, delimiter=',')
        next(reader, None) # skip the header

        a_text = []
        b_text = []
        for row in reader:
            speaker = row[2]
            # clean the utterance
            text = row[4].replace('?', ' ').replace('!', ' ').strip()
            text = re.sub(r'\(.*\)', '', text)
            text = text.replace('[', '') # replace square brackets
            text = text.replace(']', '')
            text = ' '.join(text.split())
            # append text
            if speaker == 'A':
                a_text.append(text)
            else:
                b_text.append(text)

        # write to out_file
        for text in a_text:
            outfile.write('A, ' + text + '\n')
        for text in b_text:
            outfile.write('B, ' + text + '\n')


# main
if __name__ == '__main__':
    input_folder = '/Users/yangxu/Documents/Danish_MapTask/'
    output_folder = 'data/'

    input_files = glob.glob(input_folder + 'Pair*.tsv')
    for in_file in input_files:
        m = re.search(r'Pair[0-9]+\.tsv', in_file)
        out_file = output_folder + m.group(0).lower().replace('.tsv', '.csv')
        process_file(in_file, out_file)
