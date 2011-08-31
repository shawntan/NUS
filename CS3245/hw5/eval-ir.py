#!/usr/bin/python
# eval-ir.py - Interpolated precision and recall

import getopt
import linecache
import sys

def f_measure(precision, recall, weight=0.5):
    beta_sq = float(1-weight)/weight
    f = ((beta_sq+1) * precision * recall) / (beta_sq * precision + recall)
    return f

def evaluate_ir(l, correct_results_fn, evaluated_results_fn, output_fn):
    
    gold_doc_ids = linecache.getline(correct_results_fn, int(l)).split()
    pred_doc_ids = linecache.getline(evaluated_results_fn, int(l)).split()
    total_relevant = len(gold_doc_ids)
    
    recall = precision = 0
    num_relevant = 0  
    num_retrieved = 0
    
    result = []
        
    for p in pred_doc_ids:
        num_retrieved += 1
        
        if p in gold_doc_ids:
            num_relevant += 1
        
        recall = float(num_relevant)/total_relevant * 100
        precision = float(num_relevant)/num_retrieved * 100
        
        result.append([recall, precision])

    max_precision = 0

    # interpolation for precision
    for i in reversed(range(len(result))):
        
        recall, precision = result[i]
        if precision >= max_precision:
            max_precision = precision
        else: 
            result[i] = [recall, max_precision]

    i = 1
    
    f = open(output_fn, "w")

    for recall, precision in result:
        f.write("Precision at Rank " + str(i) + ": " + "%.2f" % precision + "\n")
        f.write("Recall at Rank " + str(i) + ": " + "%.2f" % recall + "\n")
        f.write("F1 at Rank " + str(i) + ": " + "%.2f" % f_measure(recall, precision) + "\n")
        print "Precision at Rank " + str(i) + ": " + "%.2f" % precision 
        print "Recall at Rank " + str(i) + ": " + "%.2f" % recall 
        print "F1 at Rank " + str(i) + ": " + "%.2f" % f_measure(recall, precision)
        i += 1
    
    f.close()


def usage():
    print "usage: " + sys.argv[0] + "-l l -g gold-standard-answers.txt -p predicted-answers.txt -o output-statistics.txt"

l = input_file_g = input_file_p = output_file = None

try:
    opts, args = getopt.getopt(sys.argv[1:], 'l:g:p:o:')
except getopt.GetoptError, err:
    usage()
    sys.exit(2)
    
for o, a in opts:
    if o == '-l':
        l = a
    elif o == '-g':
        input_file_g = a
    elif o == '-p':
        input_file_p = a
    elif o == '-o':
        output_file = a
    else:
        assert False, "unhandled option"
if input_file_g == None or input_file_p == None or output_file == None:
    usage()
    sys.exit(2)


evaluate_ir(l, input_file_g, input_file_p, output_file)
