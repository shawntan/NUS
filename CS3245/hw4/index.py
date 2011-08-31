#!/usr/bin/python

import sys, os, re, math, getopt, glob
import nltk
from nltk.stem.porter import PorterStemmer

stemmer = PorterStemmer()

def read_process_text(fn):
    """
    tokenize and stem words in a file
    """
    fh = open(fn, 'r')
    text = fh.read()
    text = re.sub('\n', ' ', text) 
    text = re.sub(' +', ' ', text) 
    fh.close()
    my_words = {}
        
    for sent in nltk.sent_tokenize(text):
        for w in nltk.word_tokenize(sent):
            
            stemmed_word = stemmer.stem_word(w.lower())
            # count the number of occurences of a word in the given document 
            if my_words.has_key(stemmed_word):
                my_words[stemmer.stem_word(w.lower())] += 1
            else:
                my_words[stemmer.stem_word(w.lower())] = 1

    return my_words


    

def generate_skip_list(posting_list, insert_skips=True):
    """
    the format for a postings list with skips is (for example):
    [[w1,skip1], [w2], [w3], [w4,skip2], ...]
    """
    p = len(posting_list)
    skip_list = [[e] for e in posting_list]
    if p <= 2:
        return skip_list 

    if insert_skips:
        num_skips = int(math.floor(math.sqrt(p)))
        step = int(math.floor(p / num_skips))
        i = 0
        while num_skips > 0:
            next_i = i + step
            if num_skips == 1:
                next_i = p - 1
            skip_list[i] = [skip_list[i][0], next_i]
            i = next_i
            num_skips -= 1
    return skip_list

def index_documents(input_dir, output_dict, output_post):
    """
    Format: <word> <document frequency> <index>
    """

    words = {}
    # sort the documents to be indexed
    sorted_files = sorted(glob.glob(input_dir+'/*'), 
            lambda x, y: cmp(int(os.path.basename(x)), int(os.path.basename(y))))

    # intermediate results will be written to fh1
    fh1 = open('intermediate.txt', 'w')

    all_fids = {}
    for fn in sorted_files:
        print fn
        fid = os.path.basename(fn)
        all_fids[fid] = 1
        my_words = read_process_text(fn)
        # each line in the intermediate file contains doc ID followed by all unique terms appear in this doc
        
        temp_list = []
        for key in sorted(my_words.keys()):
           temp_list.append([ key,my_words[key] ])

        fh1.write(fid+' '+str(temp_list)+'\n')

        for w in my_words:
            words[w] = 1
    if words.has_key(''): del words['']
    words = sorted(words.keys())
    fh1.close()

    # store file/doc IDs
    all_fids = [int(a) for a in all_fids.keys()]
    all_fids.sort()

    fh2 = open(output_dict, 'w')
    fh3 = open(output_post, 'w')
    # first line of postings.txt stores the total number of documents for
    # N when calculating tf-idf
    fh3.write(str(len(all_fids))+"\n")
    lineno = 2
    
    inter_dict = {}
    
    fh1 = open('intermediate.txt', 'r')
    
    for line in fh1:
        tokens = line.split()
        fid = int(tokens.pop(0))
        term_counts_list = eval(''.join(tokens))
        
        for term, count in term_counts_list:
            if inter_dict.has_key(term):
                inter_dict[term].append([fid, count])
            else:
                inter_dict[term]= [[fid, count]]

    fh1.close()
    
    for w in words:
        print w
        fh2.write(w+" "+str(len(inter_dict[w]))+" "+str(lineno)+"\n")
        fh3.write(str(inter_dict[w])+"\n") 
        lineno += 1

    fh3.close()

def usage():
    print "usage: " + sys.argv[0] + " -i directory-of-documents -d dictionary-file -p postings-file"

if __name__ == "__main__":
    input_dir = output_dict = output_post = None
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'i:d:p:')
    except getopt.GetoptError, err:
        usage()
        sys.exit(2)
    for o, a in opts:
        if o == '-i':
            input_dir = a
        elif o == '-d':
            output_dict = a
        elif o == '-p':
            output_post = a
        else:
            assert False, "unhandled option"
    if input_dir == None or output_dict == None or output_post == None:
        usage()
        sys.exit(2)

    index_documents(input_dir, output_dict, output_post)
