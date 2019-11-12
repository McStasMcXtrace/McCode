#!/usr/bin/env python
# -*- coding: utf-8 -*-
import logging
import argparse
import json
import os
from os.path import join, dirname
from os import walk
import shutil
import jinja2

ERROR_PERCENT_THRESSHOLD_ACCEPT = 20


def get_header_info(testobj):
    i = testobj.get("_meta", None)
    # TODO: implement

def run_normal_mode(testdir, reflabel):
    ''' load test data and print to html label '''

    def get_cell_tuple(cellobj, refval=None):
        ''' set up and format cell data '''
        state = None
        compiletime = None
        runtime = None
        testval = None
        refp = None

        if not cellobj["compiled"]:
            state = 4
            return (state, )
        elif not cellobj["didrun"]:
            state = 3
            compiletime = "%.2f s" % cellobj["compiletime"]
            return (state, compiletime)
        else:
            testval = "%.2g" % float(cellobj["testval"])
            runtime = "%.2f s" % cellobj["runtime"]
            compiletime = "%.2f s" % cellobj["compiletime"]
            if cellobj["testnb"] > 1:
                # if this is a second test of the same instr, it was already compiled, thus 0.001 compiletime is nonsense
                compiletime = ""

            if refval is None:
                refval = float(cellobj["targetval"])
            else:
                refval = float(refval)
            refp = abs(float(cellobj["testval"])/refval*100)
            if abs(refp-100) > ERROR_PERCENT_THRESSHOLD_ACCEPT:
                state = 2
            else:
                state = 1
            refp = "(%2.f" % refp + "%)"

            return (state, compiletime, runtime, testval, refp)

    def get_empty_cell_tuple():
        return ("state_four", 0, 0, 0, 0)

    # load test data
    for _, alllabels, _ in walk(testdir): break
    alllabels.sort()

    refobj = None
    refmeta = None
    testlabels = []
    for t in alllabels:
        obj = json.loads(open(join(testdir, t, "testresults_%s.json" % t)).read())
        meta = obj.get("_meta", None)
        if meta:
            del obj["_meta"]
        if reflabel == t:
            refobj = obj
            refmeta = meta
        else:
            testlabels.append((t, obj, meta)) # key, object tuple

    # create header row
    hrow = ["%s (ref)" % reflabel] + [t[0] for t in testlabels]
    rows = []

    # create rows - 1) all instr tests in reference
    refkeys = list(refobj.keys())
    refkeys.sort()
    for key in refkeys:
        row = []
        rows.append(row)

        # instr
        row.append(key)

        # ref col
        row.append(get_cell_tuple(refobj[key]))

        # test cols
        for (label, obj, meta) in testlabels:
            o = obj.get(key, None)
            if o:
                row.append(get_cell_tuple(o, refobj[key]["targetval"]))
            else:
                row.append(get_empty_cell_tuple())

    # TODO: create remaining rows
    # TODO: in the above, delete already used keys
    # TODO: repeat the above, iterating through the next test, etc. (a while loop or recursive)

    text = open(join(dirname(__file__), "main.template")).read()
    html = jinja2.Template(text).render(hrow=hrow, rows=rows)

    open("output.html", "w").write(html)

def run_interactive_mode(testroot):
    ''' a simple utility for deleting useless test directories '''
    for _, dirs, _ in walk(testroot):
        for d in dirs:
            print(d)
            testdir = join(testroot, d)
            for _, tests, _ in walk(join(testroot, d)): break
            for t in tests:
                for _, instrs, _ in walk(join(testroot, d, t)): break
                print("    " + t + " (%d)" % len(instrs))
            ans = input("delete %s (y/n)? " % join(testroot, d))
            if ans == "y":
                try:
                    shutil.rmtree(testdir)
                except Exception as e:
                    print("could not be deleted...", str(e))
        break
    print("maintenance complete")

def main(args):
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG, format="%(message)s")
    else:
        logging.basicConfig(level=logging.INFO, format="%(message)s")

    testroot = args.testroot
    testdir = None
    if args.testdir:
        testdir = args.testdir
    reflabel = args.reflabel

    if not testdir and testroot:
        print("interactive mode")
        run_interactive_mode(testroot)
        quit(0)
    else:
        run_normal_mode(testdir, reflabel)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('testdir', nargs="?", help='test data is drawn from this root folder')
    parser.add_argument('--reflabel', nargs="?", help='reference label name')
    parser.add_argument('--testroot', nargs="?", help='test root folder for test result management')
    parser.add_argument('--verbose', action='store_true', help='output excessive information for debug purposes')
    args = parser.parse_args()

    main(args)


