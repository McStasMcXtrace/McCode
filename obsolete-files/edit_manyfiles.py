#!/usr/bin/env python
# -*- coding: utf-8 -*-
import argparse
import os
import sys
import subprocess

sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

def edit_file(filepath, start_cond_str, end_cond_str, editor_cmd):
    text = open(filepath).read()

    # cut the block 
    idx_1 = text.find(start_cond_str)
    idx_2 = text.find(end_cond_str)

    block = text[idx_1: idx_2]

    print()
    print("%s: " % filepath)
    print(block)
    print()

    user = input("edit file? (block:b, source:f or continue:c/anykey): ")


    if user == 'b':
        # create/overwrite buffer file
        open("/tmp/_tmp_.buffer", "w").write(block)
        p = subprocess.Popen("%s /tmp/_tmp_.buffer" % editor_cmd, shell=True)
        p.communicate()

        # replace block in orgiginal file with buffer contents
        new_block = open("/tmp/_tmp_.buffer").read()
        text = text[:idx_1] + new_block + text[idx_2:]
        open(filepath, 'w').write(text)

        print("file block was edited...")
        print()
        print()

    elif user == 'f':
        # edit source file
        p = subprocess.Popen("%s %s" % (editor_cmd, filepath), shell=True)
        p.communicate()

        print("file was edited...")
        print()
        print()

    else: 
        print("skipping...")
        print()
        print()


def main(args):
    if not os.path.isfile(args.shortlist):
        print("please supply the shortlist file, exiting...")
        quit()
    if not os.path.isdir(args.mccoderepo):
        print("please supply the mccode repo dir, exiting...")
        quit()

    lines = open(args.shortlist).read().splitlines()
    fullpath_lines = [os.path.join(args.mccoderepo, l) for l in lines]

    for f in fullpath_lines:
        try:
            edit_file(f, "DECLARE", "INITIALIZE", "gedit")
        except Exception as e:
            print("exception thrown while editing %s: %s", (f, str(e)))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('shortlist', help='shortlist file path')
    parser.add_argument('mccoderepo', help='McCode repo directory')
    args = parser.parse_args()

    main(args)


