# Bugs and Problems
~~is 8 bytes allocation enough to store an argument (a int) and a pointer? Should we make it 16 bytes of allocation to accomodate the 64 bit system that we are running on?~~

~~test07, 09 causes segmentation fault, investigation pending.~~

# Progress

- [x] Passed all tests converting cish output to legitimate c code
- [ ] Integrating the mark and sweep garbage collector and tests if it is working correctly
- [ ] Code the stop and copy garbage collector
- [ ] benchmarking the performance. Memory locality, the stop interval, overall runtime, memory patter. Main tool is Linux perf. 

# update
assemble
add command in makefile
## compile
make   
make -B test case=01
