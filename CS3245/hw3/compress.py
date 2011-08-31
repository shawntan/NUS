# compress.py
# module to perform gap based encoding, and variable byte encoding on posting lists.
import vbe

def encode_gaps_in_postings(l):
    """
    Encode the list with gaps based encoding. Note that the list come in this format:
    [[N1, S1, [P1, P2, ... PM]], ...], where N is the document id, and P is the positions,
    S is an optional skip list
    """    
    result = [ [l[0][0]] ]                                  # Append the first number
    
    if len(l[0]) == 3: result[-1].append( l[0][1] )         # Append skip list of first position
    
    result[-1].append( encode_gaps_in_positions(l[0][-1]) ) # calculate gap for positions

    for i in range(1, len(l)):

        result.append( [ l[i][0]-l[i-1][0] ] )                  # calculate gap
        
        if len(l[i]) == 3: result[-1].append( l[i][1] )         # append skip list
        
        result[-1].append( encode_gaps_in_positions(l[i][-1]) ) # calculate gap for positions
    
    return result


def encode_gaps_in_positions(l):
    """
    Encode the list with gaps based encoding. Note that the list come in this format:
    [P1, P2, ... PM]], where P is the positions
    """    
    result = [ l[0] ]# Append the first number
    
    for i in range(1, len(l)): result.append( l[i]-l[i-1] )
    
    return result


def decode_gaps_in_postings(l):
    """
    Decode the list with gaps based encoding. Note that the list come in this format:
    [[N1, S1, [P1, P2, ... PM]], ...], where N is the document id, and P is the positions,
    S is an optional skip list
    """    
    
    result = [ [l[0][0]] ]# Append the first number

    if len(l[0]) == 3: result[-1].append( l[0][1] )         # Append skip list of first position
    
    result[-1].append( decode_gaps_in_positions(l[0][-1]) ) # calculate gap for positions

    for i in range(1, len(l)):
        
        result.append( [ result[-1][0] + l[i][0] ] )            # calculate gap
     
        if len(l[i]) == 3: result[-1].append( l[i][1] )         # append skip list
        
        result[-1].append( decode_gaps_in_positions(l[i][-1]) ) # calculate gap for positions
    
    return result

def decode_gaps_in_positions(l):
    """
    Decode the list with gaps based encoding. Note that the list come in this format:
    [P1, P2, ... PM]], where P is the positions
    """    
    result = [ l[0] ]# Append the first number
    
    for i in range(1, len(l)): result.append( result[-1] + l[i] )
    
    return result


def flatten_postings(postings_list):
    """
    Flattens the postings list. The resulting flattened posting list will be of the following format:
    <docID><skip><len><positions>. Note that if there are no skip pointers, "0" will be used instead.
    """
    result = []
    
    for p in postings_list:
        result.append(p[0])
        
        if len(p) == 3: # has skip list
            result.append(p[1])
        else:
            result.append(0) # or 0 for no skip list

        result.append(len(p[-1])) # add the length of positions list
        
        for pp in p[-1]: result.append(pp) # add positions                   
        
    return result

def rebuild_postings(flatten_list):
    """
    Rebuild the postings list from flattened version
    """
    result = []
    
    while len(flatten_list) > 0:
        tmp_list = []
        tmp_list.append(flatten_list.pop(0))
        if(flatten_list[0] != 0):
            tmp_list.append(flatten_list.pop(0))
        else:
            flatten_list.pop(0)

        pos_len = flatten_list.pop(0)
        
        tmp_list2 = []

        while pos_len > 0: 
            tmp_list2.append(flatten_list.pop(0))
            pos_len -= 1

        tmp_list.append(tmp_list2) # attach positional postings to postings       
        result.append(tmp_list)

    return result


def uncompress_postings_list(line):

    bs = []
    for char in line: #ignore newline character
        bs.append(char)

    vb_decoded = vbe.vb_decode(bs)
    rebuilt_list = rebuild_postings(vb_decoded)
    decoded_postings = decode_gaps_in_postings(rebuilt_list)
    
    return decoded_postings

if __name__ == "__main__":
    l = [[12, 2, [109, 200, 201,204]], [13,2, [59]], [19, [1, 6, 9, 11]]]
    
    print "Before Encoding: " + str(l)

    encode = encode_gaps_in_postings(l)
    print "After Encoding : " + str(encode)
    
    # [12, 2, 4, 109, 91, 1, 3, 1, 2, 1, 59, 6, 0, 4, 1, 5, 3, 2]
    flat = flatten_postings(encode)
    print "After Flatten  : " + str(flat)
    
    vb_encoded = vbe.vb_encode(flat)
    print "After VBEncode : " + str(vb_encoded)

    bytestream = vbe.encoded_bytestream(vb_encoded)
    print "Byte Stream    : " + str(bytestream)
    
    ### Recovering the list ###

    vb_decoded = vbe.vb_decode(bytestream)
    print "After VBDecode  : " + str(vb_decoded)

    rebuilt_list = rebuild_postings(vb_decoded)
    print "Rebuilt List    : " + str(rebuilt_list)

    decoded_postings = decode_gaps_in_postings(rebuilt_list)
    print "Decoded postings: " + str(decoded_postings)



