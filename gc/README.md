# Bugs and Problems
~~is 8 bytes allocation enough to store an argument (a int) and a pointer? Should we make it 16 bytes of allocation to accomodate the 64 bit system that we are running on?~~

~~test07, 09 causes segmentation fault, investigation pending.~~

# Progress

- [x] Passed all tests converting cish output to legitimate c code
- [x] Integrating the mark and sweep garbage collector and tests if it is working correctly
- [x] Code the stop and copy garbage collector
- [x] benchmarking the performance. Memory locality, the stop interval, overall runtime, memory patter. Main tool is Linux perf.

# Reproducibility:
The experiment and benchmark is handled by another team member at the other branch.   
For general testing, cd into gc_test and call make is enough. The tests will be run automatically. 

# update
assemble
add command in makefile
## compile
make   
make -B test case=01
