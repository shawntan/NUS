#!/usr/bin/python

import heapq, sys, getopt, linecache, re, math
import index

word_dict = {}
postings_file = None
all_fids = None

# according to page 34 of IIR, skips for intermediate results are not available
# but we may choose to support this by change always_insert_skips to True
always_insert_skips = False 

def load_dictionary(input_dict):
    """
    load dictionary into word_dict
    word_dict maps a word to a list [document frequency, posting pointer]
    """
    fh = open(input_dict)
    for line in fh:
        ws = line.split()
        word_dict[ws[0]] = [int(e) for e in ws[1:]]
    
    fh.close()



def get_query_tokens(query):
    qlist = re.split("([ ()])", query)
    qlist = filter(lambda a: a != "" and a != " ", qlist)     # remove " " and ""

    for i in range(0, len(qlist)):
        # stem query words
        qlist[i] = index.stemmer.stem_word(qlist[i].lower())   
    
    return qlist


def compute_score(query, k=10):
    """
    Given query and the postings file, return the top k highest document ids
    This implements a modified version of CosineScore(q) in IIR Chapter 6. 
    In partiular, this implementation uses lnn.ltc (SMART notation)
    """
    N = eval(linecache.getline(postings_file, 1)) # Read  the total number of documents
    
    q_tf = {}   # stores the query term frequency
    scores = {} # stores the scores
    squared_w_tq = 0 

    # calculate the query term frequency
    for q in query:
        if q_tf.has_key(q):
            q_tf[q] += 1
        else:
            q_tf[q] = 1
    
    #w_tq = 0
    for q in query:
        
        if word_dict.has_key(q):
            df = word_dict[q][0]
            idf = math.log(float(N)/df,10)    

            w_tq = (1 + math.log(q_tf[q],10)) * idf
            
            squared_w_tq += math.pow(w_tq, 2)    

            postings_list = eval(linecache.getline(postings_file, word_dict[q][1]))

            for docId, tf in postings_list:
                if scores.has_key(docId):
                    scores[docId] += w_tq * (1 + math.log(tf,10))    
                else:
                    scores[docId] = w_tq * (1 + math.log(tf,10))    
   
    if squared_w_tq == 0: return []

    # Normalize the scores
    norm_const = 1/math.sqrt(squared_w_tq) * 1/len(query)

    for key in scores.keys():
        if norm_const != 0:
            scores[key] *= norm_const
       
    # return the top k scores   
    result =  heapq.nlargest(k, scores, key = lambda k: scores[k]) 
    
    # Pretty print results
    #for r in result:
    #    print str(r) + " : " + str(scores[r])

    for i in range(0, len(result)):
        result[i] = str(result[i])
    
    return result



def usage():
    print "usage: " + sys.argv[0] + " -d dictionary-file -p postings-file -q file-of-queries -o output-file-of-results"
    print "    or " + sys.argv[0] + " -d dictionary-file -p postings-file -i"

if __name__ == "__main__":
    dictionary_file = query_file = result_file = None
    interactive = False
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'd:p:q:o:i')
    except getopt.GetoptError, err:
        usage()
        sys.exit(2)
    for o, a in opts:
        if o == '-d':
            dictionary_file = a
        elif o == '-p':
            postings_file = a
        elif o == '-q':
            query_file = a
        elif o == '-o':
            result_file = a
        elif o == '-i':
            interactive = True
            query_file = ""
            result_file = ""
        else:
            assert False, "unhandled option"
    if dictionary_file == None or postings_file == None or query_file == None or result_file == None:
        usage()
        sys.exit(2)

    load_dictionary(dictionary_file)

    # interactive mode: you can key in your query one at a time at the command prompt
    if interactive:
        print "Interactive search. Ctrl-C to exit."
        while True:
            try:
                query = raw_input("Search: ")
                query_tokens = get_query_tokens(query.strip())
                results = compute_score(query_tokens)
                print ' '.join(results)

            except KeyboardInterrupt:
                print "\nBye"
                sys.exit()
    else:

        fh = open(query_file, "r")
        fh2 = open(result_file, "w")
        
        for query in fh:
            query_tokens = get_query_tokens(query.strip())
            
            print "Search: " + query
           
            results = compute_score(query_tokens)
            print results
            
            res_str = ' '.join(results) 
            
            fh2.write(res_str+"\n")
        fh.close()
        fh2.close()
