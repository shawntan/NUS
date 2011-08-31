#!/usr/bin/python

import sys, getopt, linecache, re
import index
import compress, vbe

word_dict = {}
ops = ["(", ")", "\"", "AND", "OR", "NOT"]
postings_file = None
all_fids = None
# according to page 34 of IIR, skips for intermediate results are not available
# but we may choose to support this by change always_insert_skips to True
always_insert_skips = False 

def load_dictionary(input_dict):
    """
    load dictionary into word_dict
    word_dict maps a word to a list [frequency, posting pointer]
    """
    fh = open(input_dict)
    for line in fh:
        ws = line.split()
        word_dict[ws[0]] = [int(e) for e in ws[1:]]
    fh.close()

def union_with_skips(p1, p2):
    """
    union two postings lists together
    """
    tmp_dict = {}
    for a in p1:
        tmp_dict[a[0]] = 1
    for a in p2:
        tmp_dict[a[0]] = 1
    answer = tmp_dict.keys()
    answer.sort()
    answer = index.generate_skip_list(answer, always_insert_skips)
    return answer

def intersect_with_skips(p1, p2):
    """
    algorithm in figure 2.10 of IIR
    intersect two postings lists together
    """

    if p1 == [] or p2 == []: return []

    answer = []
    ptr1 = 0
    ptr2 = 0
    while ptr1 != len(p1) and ptr2 != len(p2):
        
        if p1[ptr1][0] == p2[ptr2][0]:
            answer.append(p1[ptr1][0])
            ptr1 += 1
            ptr2 += 1
        else:
            if p1[ptr1][0] < p2[ptr2][0]:
                # len(p1[ptr1]) == 3 means hasSkip
                # p1[ptr1][1] is the skip pointer
                if len(p1[ptr1]) == 3 and p1[ p1[ptr1][1] ][0] <= p2[ptr2][0]:
                    while len(p1[ptr1]) == 3 and p1[ p1[ptr1][1] ][0] <= p2[ptr2][0]:
                        ptr1 = p1[ptr1][1]
                else:
                    ptr1 += 1
            else:

                if len(p2[ptr2]) == 3 and p2[ p2[ptr2][1] ][0] <= p1[ptr1][0]:
                    while len(p2[ptr2]) == 3 and p2[ p2[ptr2][1] ][0] <= p1[ptr1][0]:
                        ptr2 = p2[ptr2][1]
                else:
                    ptr2 += 1
    
    if answer == []: return []
    else: return index.generate_skip_list(answer, always_insert_skips)

def process_query_list(qlist):
    
    # process parentheses
    qlist2 = []
    while len(qlist) != 0:
        if qlist[0] == "(":
            sub_qlist = []
            qlist.pop(0)
            while qlist[0] != ")":
                sub_qlist.append(qlist.pop(0))
            qlist.pop(0)
            posting_list = process_query_list(sub_qlist)[0]
            qlist2.append(posting_list)
        else:
            qlist2.append(qlist.pop(0))

    qlist3 = []
    while len(qlist2) != 0:
        if qlist2[0] == "\"":
            sub_qlist = []
            qlist2.pop(0)
            while qlist2[0] != "\"":
                sub_qlist.append(qlist2.pop(0))
            qlist2.pop(0)
            posting_list = process_PHRASES(sub_qlist)[0]
            qlist3.append(posting_list)
        else:
            qlist3.append(qlist2.pop(0))

    # process NOTs
    qlist4 = []
    while len(qlist3) > 0:
        if qlist3[0] == "NOT":
            qlist3.pop(0)
            qlist4.append(process_NOT(qlist3.pop(0)))
        else:
            qlist4.append(qlist3.pop(0))
            
    # process ANDs and ORs
    while len(qlist4) >= 3:
        a  = qlist4.pop(0)
        op = qlist4.pop(0)
        b  = qlist4.pop(0)
        if op == "OR":
            posting_list = process_OR([a, b])
            qlist4.insert(0, posting_list)
        elif op == "AND":
            and_list = []
            and_list.append(a)
            and_list.append(b)
            while len(qlist4) >= 2 and qlist4[0] == "AND":
                qlist4.pop(0)
                and_list.append(qlist4.pop(0))
            posting_list = process_AND(and_list)
            qlist4.insert(0, posting_list)
    return qlist4

def process_PHRASES(a):
    """
    process phrasal queries
    """
    if [] in a: return [[]]

    elif len(a) == 1:
            
        start_byte = word_dict[a[0]][1]
        num_bytes = word_dict[a[0]][2]
        
        fh = open(postings_file)
        fh.seek(start_byte)
        bytestream = fh.read(num_bytes)
        result = compress.uncompress_postings_list(bytestream)
        return [result]
    
    else:
        sublist = []

        for elt in a:
            start_byte = word_dict[elt][1]
            num_bytes = word_dict[elt][2]
        
            fh = open(postings_file)
            fh.seek(start_byte)
            bytestream = fh.read(num_bytes)
            result = compress.uncompress_postings_list(bytestream)
            sublist.append(result)
            
        result = positional_intersect(sublist[0], sublist[1])            
        
        # remove processed sublist
        sublist.pop(0)
        sublist.pop(0)
        
        # process the rest of the sublist elements
        while len(sublist) > 0:
            result = positional_intersect(result, sublist.pop(0))
        
        return [result]



def positional_intersect(p1, p2, k=1):
    """
    Implements positional intersect take from IIR 2.4, Fig 2.12.
    The modification comes from encorcing the exact spacing.
    """
    if p1 == [] or p2 == []: return [[]]

    answer = []
    ptr1 = ptr2 = 0
    
    while ptr1 != len(p1) and ptr2 != len(p2):
        
        if p1[ptr1][0] == p2[ptr2][0]: # docID match
            l = []
            pptr1 = pptr2 = 0
            
            while pptr1 != len(p1[ptr1][:1]):
                while pptr2 != len(p2[ptr2][:-1]):
                    if abs(p1[ptr1][:-1][pptr1] - p2[ptr2][:-1][pptr2]) <= k:
                        l.append(p2[ptr2][:-1][pptr2])

                    elif p2[ptr2][:-1][pptr2] > p1[ptr1][:-1][pptr1]:
                        break
                    pptr2 += 1

                    while len(l) != 0 and abs(l[0] - p1[ptr1][:-1][pptr1]) > k:
                        del l[0]

                    for ps in l:
                        answer.append([p1[ptr1][0], p1[ptr1][:-1][pptr1], ps])

                pptr1 += 1
            
            ptr1 += 1
            ptr2 += 1
        
        elif p1[ptr1][0] < p2[ptr2][0]:
            ptr1 += 1
        else:
            ptr2 += 1

    return answer




def process_NOT(a):
    # read the postings list for all doc ID's into all_fids when this function is first called
    global all_fids
    if all_fids == None:

        all_fids = [pair[0] for pair in eval(linecache.getline("all_id.txt", 1))]
    if not isinstance(a, list):
       
        start_byte = word_dict[a[0]][1]
        num_bytes = word_dict[a[0]][2]
        
        fh = open(postings_file)
        fh.seek(start_byte)
        bytestream = fh.read(num_bytes)
        lst = compress.uncompress_postings_list(bytestream)
        
        a_fids = [pair[0] for pair in lst]
    else:
        a_fids = [pair[0] for pair in a]
    
    other_fids = filter(lambda a: a not in a_fids, all_fids)
    return index.generate_skip_list(other_fids, always_insert_skips)

def process_OR(pair):
    """
    process a pair of terms/postings that are OR'ed
    """
    if not isinstance(pair[0], list):
       
        start_byte = word_dict[pair[0]][1]
        num_bytes = word_dict[pair[0]][2]

        fh = open(postings_file)
        fh.seek(start_byte)
        bytestream = fh.read(num_bytes)
        p1 = compress.uncompress_postings_list(bytestream)
       
    else:
        p1 = pair[0]
    if not isinstance(pair[1], list):

        start_byte = word_dict[pair[1]][1]
        num_bytes = word_dict[pair[1]][2]

        fh = open(postings_file)
        fh.seek(start_byte)
        bytestream = fh.read(num_bytes)
        p2 = compress.uncompress_postings_list(bytestream)
 
    else:
        p2 = pair[1]
    return union_with_skips(p1, p2)

def process_AND(lst):
    """
    process a list of terms/postings that are AND'ed
    """
    for i in range(0, len(lst)):
        if not isinstance(lst[i], list):
            
            start_byte = word_dict[lst[i]][1]
            num_bytes = word_dict[lst[i]][2]

            fh = open(postings_file)
            fh.seek(start_byte)
            bytestream = fh.read(num_bytes)
            result = compress.uncompress_postings_list(bytestream)

            # strip off the positions, so we don't interfere with the original method
            lst[i] = remove_positions(result)
    
    # sort the list by frequency so that less frequent terms come first
    lst = sorted(lst, lambda x, y: cmp(len(x), len(y)))
    while len(lst) > 1:
        p1 = lst.pop(0)
        p2 = lst.pop(0)
    
        lst.insert(0, intersect_with_skips(p1,p2))
    
    return lst[0]

def remove_positions(posting):
    result = []
    for p in posting:
        result.append(p[0:-1])

    return result

def search_query(query):
    qlist = re.split("([ ()\"])", query) # split on quotes
    qlist = filter(lambda a: a != "" and a != " ", qlist)     # remove " " and ""

    for i in range(0, len(qlist)):
        if qlist[i] not in ops:
            # stem query words
            qlist[i] = index.stemmer.stem_word(qlist[i].lower())   
            if not word_dict.has_key(qlist[i]):
                # assign empty list to words not in dictionary
                qlist[i] = []

    # qlist only contains one query term
    if len(qlist) == 1:
        if isinstance(qlist[0], list):
            res_list = qlist
        else:
             
            start_byte = word_dict[qlist[0]][1]
            num_bytes = word_dict[qlist[0]][2]

            fh = open(postings_file)
            fh.seek(start_byte)
            bytestream = fh.read(num_bytes)
            result = compress.uncompress_postings_list(bytestream)
            res_list = [result]

    else:
        res_list = process_query_list(qlist)
    
    res = [pair[0] for pair in res_list[0]]
    return " ".join(str(a) for a in res)

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
                res_str = search_query(query.strip())
                print "Results: " + res_str + "\n"
            except KeyboardInterrupt:
                print "\nBye"
                sys.exit()
    else:
        fh = open(query_file, "r")
        fh2 = open(result_file, "w")
        for query in fh:
            query = query.strip()
            #print "Search: " + query
            res_str = search_query(query)
            #print "Results: " + res_str + "\n"
            fh2.write(res_str+"\n")
        fh.close()
        fh2.close()
