This is the README file for U077129N's submission

Email: u0707129@nus.edu.sg

== General Notes about this assignment ==

I have written the program such that it is easy to experiment on various options such as:

a) The number of characters in each n-gram
b) To use tokens instead of characters
c) Force lowercase for URLs
d) Strip punctuations

This is implemented using an options dictionary, and these options can be changed in 'build_test_LM.py'.

I have modified the script such that it can run without any arguments, relying on the default file names. Otherwise, the script can run with specified file names.

Experimental results are given in 'essay-U077129N.txt'

== Files included with this submission ==

build_test_LM.py 	: Main program
eval.py				: For evaluating accuracy of results. (Unchanged)
essay-U077129N.txt   : Answers to essay questions
README-U077129N.txt : This file


== Statement of individual work ==

Please initial one of the following statements.

[X] I, U077129N, certify that I have followed the CS 3245 Information
Retrieval class guidelines for homework assignments.  In particular, I
expressly vow that I have followed the Facebook rule in discussing
with others in doing the assignment and did not take notes (digital or
printed) from the discussions.  

== References ==

a) CS3245 Slides
b) PyDocs
c) StackOverflow.com, for an elegant way of stripping out punctuations.