#vbe.py

# implements the variable byte encding algorithm in IIR


def vb_encode_number(n):
    """
    Implements VBEncodeNumber in IIR
    """

    bytes = []
    while 1:
        bytes.insert(0, n % 128)
        if n < 128 : break
        n = n / 128
    bytes[len(bytes)-1] += 128

    return bytes


def vb_encode(numbers):
    """
    Implements VBEncode in IIR. Takes a list of numbers, and returns a bytestream
    that is the ASCII reprentation of the numbers.
    """
    
    bytestream = []
    for n in numbers:
        bytes = vb_encode_number(n)
        bytestream.extend(bytes)
    
    return bytestream


def write_bytestream_to_file(bytestream):
    f = open("test.txt", "wb")
    for byte in bytestream: f.write("%c" % byte)
    f.write("\n")
    f.close()

def encoded_bytestream(bytestream):
    return [ "%c" % byte for byte in bytestream ]


def read_bytestream_from_file(fn):
    result = []
    f = open(fn, "rb")
    for line in f.readlines():
        for char in line[0:-1]: # ignore the last character, a newline
            result.append(char)
        
    return result        

def vb_decode(bytestream):
    """
    Implements VBDecode in IIR. Takes in the bytestream, and convert it back to the
    original list of numbers.
    """
    numbers = []
    n = 0
    for i in range(0, len(bytestream)):

        if ord(bytestream[i]) < 128:
            n = 128 * n + ord(bytestream[i])
        else:
            n = 128 * n + (ord(bytestream[i]) - 128)
            numbers.append(n)
            n = 0

    return numbers


    




if __name__ == "__main__":
    orig = [2053, 15]
    bs = vb_encode(orig)
    print "Origin: " + str(orig)
    print "Encode: " + str(bs)
    
    print encoded_bytestream(bs)
    #write_bytestream_to_file(bs)
    result = read_bytestream_from_file("test.txt")
    print result

    print "Decode: " + str(vb_decode(result))
