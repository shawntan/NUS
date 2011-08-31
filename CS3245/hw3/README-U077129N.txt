This is the README file for U077129N's submission

== General Notes about this assignment ==

- [Code Credits] -

I have made use of Ziheng's sample code from submission 2.

The rest of the code is done independently. 

== Files included with this submission ==

index.py            - the indexer
search.py           - the searcher
vbe.py              - module used for variable byte encoding/decoding
compress.py         - module used for compressing/decompressing the 
                      posting list entries.
intermediate.txt    - intermediate file used during indexing
dictionary.txt      - the dictionary file
postings.txt        - the postings file, compressed.
all_id.txt          - file containing all the docIDs
README-U077129N.txt - this file
essay-U077129N.txt  - answers to essay question

- Optimizations -

The indexer takes around 3 minutes to complete indexing. This is because the entire intermediate.txt file is read into memory and processed in a hash, with a list for positional and docIDs as values.

For NOT queries, the docIds of all the files are stored onto all_fid.txt.

- Known Issues - 

There are some problems with intersection with AND queries, especially for longer queries. Short queries such as new AND york seem to work fine though.

== Statement of individual work ==

Please initial one of the following statements.

[X] I, U077129N, certify that I have followed the CS 3245 Information
Retrieval class guidelines for homework assignments.  In particular, I
expressly vow that I have followed the Facebook rule in discussing
with others in doing the assignment and did not take notes (digital or
printed) from the discussions.  


== References ==

Most of the code has been translated from the IIR pseudocode. 
I have also referenced forum postings from IVLE, especially for variable encoding.

