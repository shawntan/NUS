#!/usr/bin/python

import sys, os, re, math, getopt, glob
import nltk
import compress, vbe
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
    
    word_pos = 1 # tracks the word position
    
    for sent in nltk.sent_tokenize(text):
        for w in nltk.word_tokenize(sent):
           
            stemmed_word = stemmer.stem_word(w.lower())
            
            if my_words.has_key(stemmed_word):
                my_words[stemmed_word].append(word_pos)
            else:
                my_words[stemmed_word] = [word_pos]
        
            word_pos += 1
    
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
    
    print "indexing ..."

    words = {}
    # sort the documents to be indexed
    sorted_files = sorted(glob.glob(input_dir+'/*'), 
            lambda x, y: cmp(int(os.path.basename(x)), int(os.path.basename(y))))

    # intermediate results will be written to fh1
    fh1 = open('intermediate.txt', 'w')

    all_fids = {}
    for fn in sorted_files:
        fid = os.path.basename(fn)
        all_fids[fid] = 1
        my_words = read_process_text(fn)
        # each line in the intermediate file contains doc ID followed by all unique terms appear in this doc
        for key in my_words.keys():
            # write FID WORD [1, 3, 4, 67]
            fh1.write(fid+' '+key+' '+str(my_words[key])+'\n')
        
        for w in my_words:
            words[w] = 1
    
    if words.has_key(''): del words['']
    words = sorted(words.keys())
    fh1.close()

    fh4 = open("all_id.txt", 'w')
    # store file/doc IDs
    all_fids = [int(a) for a in all_fids.keys()]
    all_fids.sort()

    fh2 = open(output_dict, 'w')
    fh3 = open(output_post, 'w')
    # first line of postings.txt stores the postings list for all documents for
    # the purpose of processing NOT query
    fh4.write(generate_skip_list(all_fids).__str__()+"\n")


    # load entire intermediate file onto memory
    fh1 = open('intermediate.txt', 'r')

    inter_dict = {}

    for line in fh1.readlines():
        tokens = line.split()
        docID = int(tokens.pop(0))
        word = tokens.pop(0)
        positions = eval(' '.join(tokens))
        

        if inter_dict.has_key(word):
            inter_dict[word].append([docID, positions])
        else:
            inter_dict[word] = [[docID, positions]]
    

    byte_idx = 0
    for w in words:
        print w
       
        posting_list = []
        positions_list = []
        
        for elt in inter_dict[w]:
            posting_list.append(elt[0])
            positions_list.append(elt[1])

        posting_list = generate_skip_list(posting_list)
        # Merge postings and positional lists
        posting_list = merge_postings_pos_lists(posting_list, positions_list)
        
        # uncompressed posting list
        #fh3.write(str(posting_list)+ "\n")

        # compress the posting list
        gap_encoded_postings = compress.encode_gaps_in_postings(posting_list)
        flatten_postings     = compress.flatten_postings(gap_encoded_postings)
        vb_encoded_postings  = vbe.vb_encode(flatten_postings)
        bytestream           = vbe.encoded_bytestream(vb_encoded_postings) 
            
        fh2.write(w+" "+str(len(posting_list))+" "+str(byte_idx)+ " " + str(len(bytestream))+"\n") # dictionary.txt
        
        for byte in bytestream: fh3.write(byte)
        
        byte_idx += len(bytestream)    
        fh3.write(str(gap_encoded_postings)+"\n")

    fh2.close()
    fh3.close()



def merge_postings_pos_lists(postings_list, positions_list):
    result = []
    for i in range(0, len(postings_list)):
        postings_list[i].append(positions_list[i])
        result.append(postings_list[i])
    
    return result

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
