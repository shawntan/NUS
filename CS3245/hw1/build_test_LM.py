#!/usr/bin/python
import re
import sys
import getopt
import operator
import string


def build_LM(in_file, options):
    """
    build language models for each label
    each line in in_file contains a label and an URL separated by a tab(\t)
    returns a dictionary of probilities indexed by label
    """
    print 'building language models...'
   
    arts_ngrams = dict()
    news_ngrams = dict()
    sports_ngrams = dict()

    file = open(in_file,"r")
    
    for line in file:

        label, url = line.split("\t")
        url = url.replace("http://","")
        
        if(options["to_lower"] == True): url = url.lower()
        if(options["remove_punct"] == True): url = url.translate(string.maketrans("",""), string.punctuation)
        
        # Build language models of the respective labels.
        for ngram in build_NGRAMS(url, options):

            if label == "Arts":
                
                if ngram in arts_ngrams: arts_ngrams[ngram] += 1
                else: arts_ngrams[ngram] = 1
            
            elif label == "News":
                
                if ngram in news_ngrams: news_ngrams[ngram] += 1
                else: news_ngrams[ngram] = 1
            
            elif label == "Sports":
            
                if ngram in sports_ngrams: sports_ngrams[ngram] += 1
                else: sports_ngrams[ngram] = 1
    
    # Combine everything to form a set.
    all_ngrams = set(arts_ngrams)
    all_ngrams = all_ngrams.union(set(news_ngrams))
    all_ngrams = all_ngrams.union(set(sports_ngrams))

    num_ngrams = len(all_ngrams)    
    
    total_arts = total_news = total_sports = 0
   
    # Calculate the totals for each category. Then apply 1-smoothing
    for ngram in arts_ngrams: total_arts += arts_ngrams[ngram]
    for ngram in news_ngrams: total_news += news_ngrams[ngram]
    for ngram in sports_ngrams: total_sports += sports_ngrams[ngram]
    
    total_arts += num_ngrams - len(arts_ngrams)
    total_news += num_ngrams - len(news_ngrams)
    total_sports += num_ngrams - len(sports_ngrams)

    # Calculate the probabilities, applying 1-smoothing
    for ngram in all_ngrams:

        if ngram in arts_ngrams:
            arts_ngrams[ngram] = (arts_ngrams[ngram]+1.0)/total_arts
        else:
            arts_ngrams[ngram] = 1.0/total_arts

        if ngram in news_ngrams:
            news_ngrams[ngram] = (news_ngrams[ngram]+1.0)/total_news
        else:
            news_ngrams[ngram] = 1.0/total_news

        if ngram in sports_ngrams:
            sports_ngrams[ngram] = (sports_ngrams[ngram]+1.0)/total_sports
        else:
            sports_ngrams[ngram] = 1.0/total_sports

    file.close()

    return {"Arts":arts_ngrams, "News":news_ngrams, "Sports": sports_ngrams}


def build_NGRAMS(input, options):
    """
    build ngrams given input, given the number of gram units can be specified.
    returns a list of ngrams
    """
    n = options["n"]

    terms = []
    
    if options["gram_unit"] == "char":
        
        for idx in range(len(input)):
            term = string.strip(input[idx:idx+n])
            
            if(len(term) > 0):
                terms.append(term)
    
    elif options["gram_unit"] == "tokens": 
        tokens = re.findall('\w+', input) 
        
        for idx in range(len(tokens)):
            terms.append("".join(tokens[idx:idx+n]))
    
    return terms

#def calculate_probabilty(ngrams_list, url_to_predict, options, total_uniques):
def calculate_probabilty(ngrams_list, url_to_predict, options):
    """
    calculates the probability that the url belongs to the category given by ngrams_list
    """
    result = 1;
    
    for term in build_NGRAMS(url_to_predict, options): 
        if term in ngrams_list: result *= ngrams_list[term]

    return result


def label_with_highest_probabilty(LM, url_to_predict, options): 
    """
    returns the label with the highest probabilty, given a url and a language model
    """
    probabilties = dict()
    
    for label, ngrams in LM.items():
   
        probability = calculate_probabilty(ngrams, url_to_predict, options)
        probabilties[label] = probability 
    
    return max(probabilties.iteritems(), key=operator.itemgetter(1))[0]


def test_LM(in_file, out_file, LM, options):
    """
    test the language models on new URLs
    each line of in_file contains an URL
    you should print the most probable label for each URL into out_file
    """
    print "testing language models..."
    
    # Write to outut file
    input_file = open(in_file, "r")
    output_file = open(out_file, "w")
    
    # Output the highest probability b/w the 3 language models
    for line in input_file:
        url = line.replace("http://","")
        output_file.write(label_with_highest_probabilty(LM, url, options) + "\t" + line)
    
    output_file.close()
    input_file.close()


def usage():
    print "usage: " + sys.argv[0] + " -b input-file-for-building-LM -t input-file-for-testing-LM -o output-file"

input_file_b = input_file_t = output_file = None

try:
    opts, args = getopt.getopt(sys.argv[1:], 'b:t:o:')
except getopt.GetoptError, err:
    usage()
    sys.exit(2)
for o, a in opts:
    if o == '-b':
        input_file_b = a
    elif o == '-t':
        input_file_t = a
    elif o == '-o':
        output_file = a
    else:
        assert False, "unhandled option"

# Options available:
#
# n             - number of characters in each ngram
# gram_unit     - char or tokens
# remove_punct  - removes punctation if True, False otherwise
options = dict()
options["n"] = 5                        # n = 1 .... n
options["gram_unit"] = "char"             # gram_unit = "char"/"tokens"
options["to_lower"] = False 
options["remove_punct"] = False 

if input_file_b == None or input_file_t == None or output_file == None:
    usage()
    LM = build_LM("urls.build.txt", options)   
    test_LM("urls.test.txt", "urls.predict.txt", LM, options)

else:
    LM = build_LM(input_file_b, options)   
    test_LM(input_file_t, output_file, LM)
