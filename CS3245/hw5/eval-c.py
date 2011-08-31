#!/usr/bin/python
# eval-c.py - Simple script to for evaluation of a classification IR system

import sys
import getopt


def load_gold_standard(gold_std_fn):
    gold_std = {}
    for l in open(gold_std_fn):
        cls, url = l.split()
        gold_std[url] = cls
    return gold_std

def load_predicted(predicted_fn):
    predicted = {}
    for l in open(predicted_fn):
        cls, url = l.split()
        predicted[url] = cls
    return predicted 

def load_classes(class_fn):
    classes = []
    for l in open(class_fn):
        classes.append(l.strip())
    return classes

def precision(tp,fp):
    return float(tp)/(float(tp)+float(fp)) * 100

def recall(tp,fn):
    return float(tp)/(float(tp)+float(fn)) * 100

def f_measure(precision, recall, weight=0.5):
    beta_sq = float(1-weight)/weight
    f = ((beta_sq+1) * precision * recall) / (beta_sq * precision + recall)
    return f

def evaluate_ir(classes, gold_std, predicted, output_fn):
    results = {}    
    classes.sort()
    
    # intialize results: tp, fp, tn, fn 
    for c in classes:
        results[c] = [0,0,0,0]
    
    for p in predicted.keys():
        if predicted[p] == gold_std[p]:
            # increase true positive of gold/predicted class
            results[predicted[p]][0] += 1
            for c1 in classes:
                if c != c1:
                    # increase true negative of remaining classes 
                    results[c1][2] += 1
        else:
            # increase false positive of retrieved class
            results[predicted[p]][1] += 1
            # increase false negative of gold std class
            results[gold_std[p]][3] += 1
                       
    total_p = 0
    total_r = 0
    total_fm = 0
    
    outfile = open(output_fn, "w")

    for c in classes:
        
        p = precision(results[c][0], results[c][1])
        r = recall(results[c][0], results[c][3])
        fm = f_measure(p,r)
        total_p += p
        total_r += r
        total_fm += fm
        print "Precision of " + c + ": " + str("%.2f" % p)
        print "Recall of " + c + ": " + str("%.2f" % r)
        print "F1 of " + c + ": " + str("%.2f" % fm)
    
        outfile.write("Precision of " + c + ": " + str("%.2f" % p) + "\n")
        outfile.write("Recall of " + c + ": " + str("%.2f" % r) + "\n")
        outfile.write("F1 of " + c + ": " + str("%.2f" % fm) + "\n")

    num_c = len(classes)
    avg_p = total_p / num_c
    avg_r = total_r / num_c
    avg_fm = total_fm / num_c
    print "Average Precision: " + str("%.2f" % avg_p)
    print "Average Recall: " + str("%.2f" % avg_r)
    print "Average F1: " + str("%.2f" % avg_fm)

    outfile.write("Average Precision: " + str("%.2f" % avg_p) + "\n")
    outfile.write("Average Recall: " + str("%.2f" % avg_r) + "\n")
    outfile.write("Average F1: " + str("%.2f" % avg_fm) + "\n")
    outfile.close()



def usage():
    print "usage: " + sys.argv[0] + " -g gold-standard-answers.txt -p predicted-answers.txt -c classes.txt -o output-statistics.txt"

input_file_g = input_file_p = input_file_c = output_file = None

try:
    opts, args = getopt.getopt(sys.argv[1:], 'g:p:c:o:')
except getopt.GetoptError, err:
    usage()
    sys.exit(2)
    
for o, a in opts:
    if o == '-g':
        input_file_g = a
    elif o == '-p':
        input_file_p = a
    elif o == '-c':
        input_file_c = a
    elif o == '-o':
        output_file = a
    else:
        assert False, "unhandled option"
if input_file_g == None or input_file_p == None or input_file_c == None or output_file == None:
    usage()
    sys.exit(2)


# Load classes 
c = load_classes(input_file_c)
g = load_gold_standard(input_file_g)
p = load_predicted(input_file_p)
evaluate_ir(c,g,p, output_file)

