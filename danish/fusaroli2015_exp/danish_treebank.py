# Process Danish Treebank corpus and use it to train the LM to compute entropy
# Yang Xu
# 12/21/2016

from lxml import etree
import sys
import glob
import re
import string
import subprocess


# get all the tokens in a <s> instance
def get_tokens(s):
    """
    param s: an instance of lxml.etree._Element
    """
    words = [t.text.lower() for t in list(s)]
    return words

# get training sentences by processing .tag files
# does NOT work well because the .tag files are not in valid XML format
def process_tag_files():
    corpus_dir = '/Users/yangxu/Documents/mbkromann-copenhagen-dependency-treebank-2fa64f8/da/'
    tag_files = glob.glob(corpus_dir + '*-da.tag')
    output_file = 'data/train_set.txt'

    text_lines = []
    for i, tf in enumerate(tag_files):
        # read valid lines from the .tag file
        valid_lines = []
        with open(tf, 'r') as fr:
            for line in fr:
                if line.startswith('<text '):
                    if len(valid_lines) == 0:
                        valid_lines.append(line.strip())
                else:
                    if len(valid_lines) != 0:
                        valid_lines.append(line.strip())
        xml_str = ''.join(valid_lines)
        # remove the invalid xml parts
        xml_str = re.sub(r'\sid=[0-9a-zA-Z]+', '', xml_str)
        xml_str = re.sub(r'\stype=[0-9a-zA-Z]+', '', xml_str)
        # get xml tree obj from string
        try:
            tree = etree.fromstring(xml_str)
        except Exception as e:
            print('Error when processing file: {}'.format(tf))
            m = re.search(r'column\s[0-9]+', e.message)
            errpos = int(m.group(0).split()[1])
            print('Error position: {}'.format(errpos))
            print('Error text: {}'.format(xml_str[errpos-10:errpos+10]))
            raise
        else:
            sents = tree.findall('s')
            for s in sents:
                tokens = get_tokens(s)
                text_lines.append(' '.join(tokens))

    with open(output_file, 'w') as fw:
        for line in text_lines:
            fw.write(line + '\n')

###
# process the .txt files in Danish treebank
def process_txt_files():
    corpus_dir = '/Users/yangxu/Documents/mbkromann-copenhagen-dependency-treebank-2fa64f8/da/'
    txt_files = glob.glob(corpus_dir + '*-da.txt')
    output_file = 'data/train_set.txt'

    all_sents = []
    for i, txtfile in enumerate(txt_files):
        with open(txtfile, 'r') as fr:
            for line in fr:
                if line.strip() == '':
                    continue
                # break the line by sentence separaters
                sents = re.split(r'[\.|!|\?]', line)
                for s in sents:
                    # remove all punctuations
                    # s = re.sub(r'[^\P{P}-]+', '', s) # does not work
                    remove = string.punctuation
                    remove = remove.replace('-', '')
                    pattern = r'[{}]'.format(remove)
                    s = re.sub(pattern, '', s).strip()
                    if s != '':
                        all_sents.append(s)
        # print progress
        sys.stdout.write('\r{}/{} extracted'.format(i+1, len(txt_files)))
        sys.stdout.flush()
    # write to output file
    with open(output_file, 'w') as fw:
        for s in all_sents:
            fw.write(s + '\n')

##
# train language model
def trainLM():
    input_file = 'data/train_set.txt'
    lm_file = 'data/train_set.lm'
    binary_dir = '/Users/yangxu/projects/srilm-1.7.1/bin/macosx/'
    train_cmd = [binary_dir + 'ngram-count', '-order', '3', '-text', input_file, '-lm', lm_file]
    return_code = subprocess.check_call(train_cmd)
    if return_code != 0:
        print('train failure')
    pass


##
# main
if __name__ == '__main__':
    # process_txt_files()
    trainLM()
