#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
Splits a mcstas-generated C file into six files.
'''
import logging
import argparse
import re
import os

def linenums_embedded_file(subfilename, lines):
    l1 = -1
    l2 = -1

    for i in range(len(lines)):
        l = lines[i]

        m = re.search('embedding file "%s"' % subfilename, l)
        if m:
            l1 = i
        m = re.search('End of file "%s"' % subfilename, l)
        if m:
            l2 = i

    meat = None
    potato_before = None
    potato_after = None
    try:
        meat = lines[l1:l2]
        potato_before = lines[:l1]
        potato_after = lines[l2:]
    except:
        print("could not extrace subfile segment for filename: %s" % subfilename)
        quit()
    return meat, potato_before + ['#include "_%s"' % subfilename] + potato_after

def main(args):
    logging.basicConfig(level=logging.INFO)
    f = os.path.basename(args.file)
    d = os.path.dirname(args.file)
    if d == "":
        d = '.'
    text = open(args.file).read()

    lines = text.splitlines()
    
    seg_mcc, remainder = linenums_embedded_file("mccode-r.c", lines)
    seg_mch, remainder = linenums_embedded_file("mccode-r.h", remainder)
    seg_msc, remainder = linenums_embedded_file("mcstas-r.c", remainder)
    seg_msh, remainder = linenums_embedded_file("mcstas-r.h", remainder)
    seg_mcm, remainder = linenums_embedded_file("mccode_main.c", remainder)

    open("%s/_mccode-r.c" % d, "w").write("\n".join(seg_mcc))
    open("%s/_mccode-r.h" % d, "w").write("\n".join(seg_mch))
    open("%s/_mcstas-r.c" % d, "w").write("\n".join(seg_msc))
    open("%s/_mcstas-r.h" % d, "w").write("\n".join(seg_msh))
    open("%s/_mccode_main.c" % d, "w").write("\n".join(seg_mcm))

    open("%s/_%s" % (d, f), "w").write("\n".join(remainder))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    #parser.add_argument('comproot', nargs='+', help='mcstas-comps directory searched recursively.')
    parser.add_argument('file', help='input mcstas-generated C file')
    args = parser.parse_args()

    main(args)

