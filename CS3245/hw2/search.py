import getopt
import linecache
import math
import sys
from heapq import merge

dict_file = ""
postings_file = ""
results_file = ""
queries_file = ""
query_results = []

def evaluate_expr(expr, all_docs):
    """
    Evaluates the boolean expression, and returns the result. 
    """
    
    # Step 1: Extract and replace parenthesized statements '(statement)'
    while "(" and ")" in expr:

        l_paren, r_paren = expr.find("("), expr.find(")")
        paren_stmt = expr[l_paren+1: r_paren]
        
        # recursive call! 
        result = evaluate_expr(paren_stmt, all_docs)
        query_results.append(result)
        
        expr = expr.replace("(" + paren_stmt + ")", "@" + str(len(query_results)))

    # Step 2: Extract and replace 'NOT statement'
    tokens = expr.split()
    
    while "NOT" in tokens:
        
        idx = 0 
        for t in tokens:
            if t == "NOT":
                break
            else:
                idx += 1
        
        not_stmt = tokens[idx] + " " + tokens[idx+1]
    
        p1 = get_postings(int(get_postings_index(tokens[idx+1])))
        results = evaluate_NOT(p1, all_docs)
        
        query_results.append(results)

        expr = expr.replace(not_stmt, "@" + str(len(query_results)))
         
        del tokens[idx+1]
        del tokens[idx]
        
    # Step 3: Now we have a "flattened" statement. Next we need to evaluate the possible 
    #         combinations available
     

    while is_query_completed_evaluated(expr) == False:

        # Step 3a : Find the subquery that costs the least
        combinations = get_expr_combinations(expr)

        subquery = combinations[get_cheapest_subquery_idx(combinations)]
        
        # Evaluate this subquery. 
        result = evaluate_subquery(subquery, query_results)
        
        # Store that result
        query_results.append(result)
        
        # Replace processed subquery with an index.
        expr = expr.replace(' '.join(subquery), "@"+str(len(query_results)))
        
    return query_results[-1]


def is_query_completed_evaluated(expr):
    """
    Returns true if query has been completely evaluated
    """
    if expr.find("NOT") != -1:
        return False
    elif expr.find("AND") != -1:
        return False
    elif expr.find("OR") != -1:
        return False

    return True



def get_expr_combinations(expr):
    """ 
    Returns a combination of queries that can be processed
    """
    statements = [] 
    tokens = expr.split()

    idx1 = 0
    idx2 = 3

    while idx2 <= len(tokens):
        
        statements.append(tokens[idx1:idx2])
        idx1 += 2
        idx2 += 2
    
    return statements


def evaluate_subquery(subquery, query_results):
    
    operator = subquery[1]
    op1 = subquery[0].lower()
    op2 = subquery[2].lower()
    

    if operator == "NOT": 
        
        p1 = get_postings_index(op1)
        p2 = all_docs

        if p1 == None:
            p1 = []

        return evaluate_NOT(p1,p2)

    elif operator == "AND": 
        
        if op1.find("@") != -1:
            p1 = query_results[int(op1[1:])-1]
        else:
            p1 = get_postings(int(get_postings_index(op1)))
        
        if op2.find("@") != -1:
            p2 = query_results[int(op2[1:])-1]
        else:
            p2 = get_postings(int(get_postings_index(op2)))

            
        if p1 == None:
            p1 = []
        if p2 == None:
            p2 = []

        return evaluate_AND(p1,p2)

    elif operator == "OR": 
        
        if op1.find("@") != -1:
            p1 = query_results[int(op1[1:])-1]
        else:
            p1 = get_postings(int(get_postings_index(op1)))
        
        if op2.find("@") != -1:
            p2 = query_results[int(op2[1:])-1]
        else:
            p2 = get_postings(int(get_postings_index(op2)))
        
        if p1 == None:
            p1 = []
        if p2 == None:
            p2 = []
        
        return evaluate_OR(p1,p2)

def evaluate_AND(p1,p2):
    """ 
    Evaluates AND queries.
    """
    return intersect_with_skips(p1,p2)


def evaluate_OR(p1,p2):
    """
    Evaluates OR queries.
    """
    l = list(merge(remove_skips(p1), remove_skips(p2)))
    set = {}

    return [ set.setdefault(x,x) for x in l if x not in set ]


def evaluate_NOT(p1, all_docs):

    """
    Evaluates NOT queries
    """
    results = []
    results.extend(all_docs)
    
    p1 = remove_skips(p1)


    for x in p1:
        results.remove(x)

    return results


def create_documents_list():
    """
    Creates a list containing all document ids
    """    
    l = []
    files = open("files.txt")
    for line in files.readlines():
        l.append(line.strip())
    
    return l


def remove_skips(l):
    """
    Remove skips from list
    """
    list = []
    
    for x in l:
        if x.find(","):
            list.append(x.split(",")[0])
        else:
            list.append(x)
    
    return list

def get_cheapest_subquery_idx(subqueries):
    """
    Returns the index of the cheapest subquery given a list of subqueries.
    """

    cheapest = sys.maxint 
    sel_idx = 0
    idx = 0

    for subquery in subqueries:
        operator = subquery[1]
        op1 = subquery[0]
        op2 = subquery[2]
        
        cost = estimate_results_count(operator,op1,op2)
        
        if int(cost) < cheapest:
            cheapest = cost
            sel_idx = idx

        idx += 1
    return sel_idx 


def estimate_results_count(operator, op1, op2):
    """
    Estimates the number of returned results (by taking the upper bound)
    """
    
    # This will almost surely return the largest number of results. 
    if operator == "NOT":
        return len(get_postings_count(op1))-len(op1) 
    
    elif operator == "OR":
         return max([get_postings_count(op1), get_postings_count(op2)])
    
    elif operator == "AND":    
         return min([get_postings_count(op1), get_postings_count(op2)])

def get_postings_count(word): 
    """
    Retrieves the postings count from the dictionary. Otherwise, returns 0.
    """
    global dict_file
    f = open(dict_file)

    for line in f.readlines():
        tokens = line.split("\t")

        if tokens[0] == word:
            return tokens[1]

    return 0

def get_postings_index(word):
    """
    Retrieves the postings index from the dictionary. Otherwise, returns -1.
    """
    global dict_file
    f = open(dict_file)
    
    for line in f.readlines():
        tokens = line.split("\t")

        if tokens[0] == word:
            return tokens[2]
    
    return -1


def get_postings(index):
    """
    Retrieves the postings from the postings.txt file. If invalid index, None is returned.
    Index corresponds to the line number in the postings file.
    """
    if index == -1:
        return None
    else:
        global postings_file 
        index, postings = linecache.getline(postings_file,int(index)).rstrip().split("\t")
        return postings.split(" ")


def intersect_with_skips(p1, p2):
    """
    Intersect postings p1 and p2, and returns the result of the intersection. Uses 
    skip pointers fore more efficient interection
    """
    answer = []
    idx1 = 0
    idx2 = 0
   
    while idx1 < len(p1) and idx2 < len(p2) :
        
        if doc_id(p1[idx1]) == doc_id(p2[idx2]) :
            
            answer.append(doc_id(p1[idx1]))
            idx1 += 1
            idx2 += 1
        
        elif doc_id(p1[idx1]) < doc_id(p2[idx2]) :

            if has_skip(p1[idx1]) and doc_id(get_skip(p1[idx1])) <= doc_id(p2[idx2]) :
                while has_skip(p1[idx1]) and doc_id(get_skip(p1[idx1])) <= doc_id(p2[idx2]) :
                    idx1 += int(math.floor(math.sqrt(len(p1))))
            else:
                idx1 += 1
    
        elif doc_id(p2[idx2]) < doc_id(p1[idx1]) :

            if has_skip(p2[idx2]) and doc_id(get_skip(p2[idx2])) <= doc_id(p1[idx1]) :
                while has_skip(p2[idx2]) and doc_id(get_skip(p2[idx2])) <= doc_id(p1[idx1]) :
                    idx2 += int(math.floor(math.sqrt(len(p2))))
            else:
                idx2 += 1
    
    return answer


def doc_id(posting):
    """
    Returns the docID of this posting
    """
    if has_skip(posting):
        return posting.split(",")[0]
    return posting

def get_skip(posting):
    """
    Returns the skip pointer of this posting
    """
    if has_skip(posting):
        return posting.split(",")[1]

    return "-1";

def has_skip(posting):
    if posting.count(",") == 1 and posting.split(",")[1] != "-1" : return True 
    return False


def usage():
    print "python search.py -d dictionary-file -p postings-file -q file-of-queries -o output-file-of-results"
    

def main(argv):

    try:

        opts, args = getopt.getopt(argv, "d:p:q:o:")
        
    except getopt.GetoptError:

        usage()
        sys.exit(2)

    for opt, args in opts:
            if opt == "-d":
                global dict_file 
                dict_file = args
            
            elif opt == "-p":
                global postings_file 
                postings_file = args

            elif opt == "-q":
                global queries_file
                queries_file = args

            elif opt == "-o":
                global results_file
                results_file = args


if __name__ == '__main__':
    
    main(sys.argv[1:])

    all_docs = create_documents_list()
    file_of_queries = open(queries_file)
    queries_results = open(results_file ,"w")
    
    results = []

    for query in file_of_queries.readlines():
         
        result = ' '.join(evaluate_expr(query.strip(), all_docs))
        results.append(result)
    
    del results[-1]

    for result in results:
        queries_results.write(result + "\n")
    
    



