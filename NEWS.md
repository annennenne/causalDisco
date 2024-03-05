# causalDisco 0.9.3
* Changed structure of tpdag and tskeleton objects so they include tamat objects
rather than amat and order slots. This is a major change, not backwards 
compatible! 
* tpc now passes ... argument to skeleton() function call 

# causalDisco 0.9.2
* Fixed bug in maketikz() function so it now works with tpdag and tamat objects,
  and added examples to the documentation. 

# causalDisco 0.9.1
* Added three new functions: simDAG, simGausFromDAG, nDAGs.
* Changed structure of tamat objects so they inherit directly from matrix.

# causalDisco 0.9.0
* Added a NEWS.md file to track changes to the package.
* First CRAN submission.