1. a variable is live if it holds a value that may be needed in the futute. 
2. a variable is live-in at a node if it is live on any of the in-edges of that node. 
3. To speed up the convergence of the liveness algo, we should do the calculation in order. For liveness analysis, we should flow from the future to the history. Ordering od the nodes could be determined by a DFS. 
4. a condition that prevents a and b being allocated to the same register is called an interference. 
5. 