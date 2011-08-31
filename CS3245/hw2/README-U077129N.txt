This is the README file for U077129N's submission

== General Notes about this assignment ==

- Optimizations -

- Posting lists -
I have tried to optimize the creation of the posting list, by creating several smaller wordlists, then merging them together to form a final merged file. 

Skips are only inserted when the length is more than 10. 

The query optimizations are quite naive. The only heuristics they use are an estimated number of returned results based on their posting lists. However, parenthesized queries and NOT queries are always processed first, without regard to their computational cost.

- Query processing -

"( )" and NOT statements are processed first. Then they are replaced by a tag "@N", where N is a number. Once this process is done, the query is 
"flattened". Then, combinations of subqueries are taken and the cheapest
in terms of computation cost is taken. The process repeats until no more 
queries are left.

- Intermediate files - 

As mentioned in the optimizations section, intermediate files are created to facilitate the merging process. They are named wordlist_NN, where NN is an integer.

Another file that is created is files.txt. This is the list of filenames that are were previously indexed. This file is useful to create a list of all the files, to be used for NOT queries.

Due to lack of time, I have simply hardcoded files.txt.

== Files included with this submission ==

index.py  - the indexer
search.py - the searcher

postings - the postings file
README-U077129N.txt - this file
essay-U077129N.txt - answers to essay question

files.txt - a list of filenames that were previously indexed. 

== Statement of individual work ==

Please initial one of the following statements.

[X] I, U077129N, certify that I have followed the CS 3245 Information
Retrieval class guidelines for homework assignments.  In particular, I
expressly vow that I have followed the Facebook rule in discussing
with others in doing the assignment and did not take notes (digital or
printed) from the discussions.  



== References ==

Stackoverflow.com was very useful in searching code snippets. In this assignment, I've used code that performed the merging of pre-sorted wordlists, that resulted in the final merged_wordlist. 

Also, the same merge algorithm was used to build the postings list.

Many python lists operations I also consulted the various posters made by the same website. An example is listed here:

http://stackoverflow.com/questions/464342/combining-two-sorted-lists-in-python

The textbook's code listing was a little peculiar when it came to indentation. I had searched the net for smilier implementations of skip lists. I managed to find an implementation done in ruby. 

http://www.skorks.com/2010/03/faster-list-intersection-using-skip-pointers/

However, in the end, I implemented one on my own, after reading the explanations on this site.

