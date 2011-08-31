from nltk.stem.porter import *

import getopt
import heapq
import math
import nltk
import os
import string
import sys 

dict_file = ""
postings_file = ""
doc_dir = ""

def create_dictionary_and_postings(merged_wordlist_path):
    """Creates dictionary using the merged wordlist. 
       Returns filename of dictionary and postings file"""
    global dict_file
    global postings_file
    outfile = open(dict_file, "w")
    p_outfile = open(postings_file, "w")
    prev_word = curr_word = ""
    count = line_num = 1
    postings = []

    for line in open(merged_wordlist_path, "r"):
        
        curr_word, docId = line.split("\t")
         
        if prev_word == "":
            prev_word = curr_word
            postings.append(str(docId).strip())
        
        elif curr_word == prev_word:
            count += 1
            postings.append(str(docId).strip())

        else:
            
            outfile.write(prev_word + "\t" + str(count) + "\t" + str(line_num) + "\n")
            
            # postings might be out of order, so sort them first
            postings.sort()
            # Since we have our postings list, insert skips! 
            postings = create_skip_list(postings)
            p_outfile.write(str(line_num) + "\t" + " ".join(postings) + "\n")
                        
            prev_word = curr_word
            count = 1
            line_num += 1
            postings = [str(docId).strip()] 

    # Write the last entry. 
    outfile.write(curr_word + "\t" + str(count) + "\t" + str(line_num))
    
    # Sorts the posting ids. 
    postings = create_skip_list(postings)
    p_outfile.write(str(line_num) + "\t" + " ".join(postings) + "\n")
    
    return [outfile.name, p_outfile]


def create_skip_list(list):
    """ 
    Create skip list given an input of postings list
    """
    if len(list) <= 10: 
        return list
    
    else:
        idx = 0
        skip_distance = int(math.floor(math.sqrt(len(list))))

        while idx < len(list):
            
            if idx + skip_distance >= len(list):
                list[idx] += ",-1"

            else:
                list[idx] += "," + list[idx + skip_distance] 

            idx += skip_distance

    return list


def merge_wordlists(wordlist_filenames):
    """Performs a merge of all wordlists. 
       Returns the filename of the merged_wordlist"""

    # Step 1 Given each file, sort them.
    for filename in wordlist_filenames: 

        lines = []
        for line in open(filename, "r"): lines.append(line)        
        lines.sort()
            
        # overwrite previous unsorted file with a sorted version
        out_file = open(filename, "w")
        for line in lines: out_file.write(line)         
        
    # Step 2: Given each sorted file, merge them together to form one
    #         sorted list file
    
    # Copy the first wordlist_1 over to sorted_wordlist
    out_file = open("merged_wordlist", "w")
    
    files = map(open, wordlist_filenames) 
   
    for line in heapq.merge(*[f.readlines() for f in files]): 
        out_file.write(line)
    
    return out_file.name


def create_wordlist_files(dir_of_docs):
    """ Creates files containing wordlist of all the documents in unsorted order.
        The resulting file is in <term, docID>. 
        A list of filenames created is returned """
    
    count = 1;
    line_count = 0
    max_lines = 5000
    outfile_list = ["wordlist_1"]

    outfile = open("wordlist_1" , 'w')

    for dirname, dirnames, filenames in os.walk(dir_of_docs):
        
        for filename in filenames:
            full_filename = os.path.join(dirname, filename)
            
            # Make content lowercase 
            content = str(open(full_filename).read()).lower()
            # Strip out punctuations
            content = content.translate(string.maketrans("",""), string.punctuation)

            for term, docId in create_token_sequence(content, filename):
                
                if line_count <= max_lines: 
                    outfile.write(term + "\t" + docId + "\n")
                    line_count += 1
                
                else:
                    count += 1
                    line_count = 0
                    
                    outfile = open("wordlist_" + str(count), "w")    
                    outfile_list.append(outfile.name)

    return outfile_list


def create_token_sequence(doc, docId):
    """Creates token sequence, given a document and a document identifier.
       The resultant token sequence is in sorted order."""
    
    token_sequence = []

    for sentence in nltk.sent_tokenize(doc):
        for word in nltk.word_tokenize(sentence):
            stemmed_word = PorterStemmer().stem_word(word)
            
            if (stemmed_word, docId) not in token_sequence:
                token_sequence.append((stemmed_word, docId))
    
    token_sequence.sort(key=lambda x:x[0])
    return token_sequence


def usage():
    print "Usage: python index.py -i dir-of-documents -d dictionary-file -p postings-file"


def main(argv):
    try:

        opts, args = getopt.getopt(argv, "d:i:p:")
        
        for opt, args in opts:
            if opt == "-i":
                global doc_dir
                doc_dir = args
                merged_fname = merge_wordlists(create_wordlist_files(doc_dir))
                create_dictionary_and_postings(merged_fname)

            elif opt == "-d":
                global dict_file
                dict_file = args

            elif opt == "-p":
                global postings_file
                postings_file = args

    except getopt.GetoptError:

        usage()
        sys.exit(2)


def create_postings_file_names(dir_docs):
    
    l = []
    f = open("files.txt", "w")

    for filename in os.listdir(dir_docs):
        f.write(filename+"\n")
    
if __name__ == '__main__':
    main(sys.argv[1:])
    create_postings_file_names(doc_dir)
