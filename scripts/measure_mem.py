#!/usr/bin/python
# Measure the heap usage of the (already compiled) benchmarks.
# Takes no parameters.

import os
from subprocess import Popen, PIPE
try:
    from subprocess import DEVNULL # py3k
except ImportError:
    import os
    DEVNULL = open(os.devnull, 'wb')

VERBOSE = False

def exitCode(program, mem):
    FNULL = open(os.devnull, 'w')
    process = Popen([program, str(mem*2)], stdout=DEVNULL, stderr=DEVNULL)
    # (output, err) = process.communicate()
    exit_code = process.wait()
    # print "Command output : ", output
    return exit_code

def findMemory(program, mem, grain):
    if (grain<2):
	if (exitCode('./'+program, mem) == 0):
	    return mem
	else:
	    print "Could not find minimum heap size for "+program
	    exit(-1)
    else:
        currMem = mem
	if (VERBOSE):
            print "Trying: "+str(currMem)
        while (exitCode('./'+program, currMem) == 0):
            currMem = currMem - grain
	    if (VERBOSE):
		print "Trying again: "+str(currMem)
        return findMemory(program, currMem+grain, grain/10)

def main():
    print "Measuring memory..."

    benchmarks = ['collatz', 'digits_of_e1', 'ntak', 'church', 'queens', 'queens_num', 'quick_sort', 'tree_sort', 'reverse']
    mem = {}

    for bench in benchmarks:
        mem[bench] = {}

    # Starting (approximate) heap sizes (multiply with 2 when using the two-space
    # collector). Some of the sizes below have already been set to their minimum
    # measured values (esp. for long-running benchmarks). Make them bigger to
    # measure them again.
    mem['collatz']['full'] = 41400000
    mem['collatz']['compact'] = 13500000
    mem['digits_of_e1']['full'] = 625000000
    mem['digits_of_e1']['compact'] = 195000000
    mem['ntak']['full'] = 11516398810
    mem['ntak']['compact'] = 3636757520
    mem['church']['full'] = 2250000
    mem['church']['compact'] = 700000
    mem['queens']['full'] = 1503132770
    mem['queens']['compact'] = 498798920
    mem['queens_num']['full'] = 8500
    mem['queens_num']['compact'] = 2750
    mem['quick_sort']['full'] = 2788938330
    mem['quick_sort']['compact'] = 875448910
    mem['reverse']['full'] = 685380260
    mem['reverse']['compact'] = 216432080
    mem['tree_sort']['full'] = 1652780690
    mem['tree_sort']['compact'] = 525870220

    for bench in benchmarks:
        meas = {}
        for repr in ['full', 'compact']:
            program = 'gic_'+bench+'_'+repr
            m = mem[bench][repr]
	    if (VERBOSE):
		print "Measuring memory for", bench, "representation =",repr, "program =",program,"start =",m
	    memDigits = len(str(m))
	    meas[repr] = findMemory(program, m, 10**(memDigits-1))
	mFull = meas['full']
	mCompact = meas['compact']
        reduction = (float(mFull) - float(mCompact))/float(mFull)
        print '* program: '+bench+', full='+str(mFull)+', compact='+str(mCompact)+', reduction = '+str(reduction)+'%'

main()

# Results:
# * program: collatz, full=41360250, compact=13200080, reduction = 0.680851058686%
# * program: digits_of_e1, full=602368070, compact=192246560, reduction = 0.680848687747%
# * program: ntak, full=11516398810, compact=3636757520, reduction = 0.684210526224%
# * program: church, full=2198570, compact=672690, reduction = 0.694032939593%
# * program: queens, full=1503132770, compact=498798920, reduction = 0.668160438016%
# * program: queens_num, full=8140, compact=2600, reduction = 0.68058968059%
# * program: quick_sort, full=2788938330, compact=875448910, reduction = 0.686099581126%
# * program: tree_sort, full=1652780690, compact=525870220, reduction = 0.681826982139%
# * program: reverse, full=685380260, compact=216432080, reduction = 0.684216058397%
