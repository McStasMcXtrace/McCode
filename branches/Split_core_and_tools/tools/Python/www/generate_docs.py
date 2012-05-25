#!/usr/bin/env python

import glob, re


def grab_list():
    return glob.glob('sim/src/*.instr')


def gen_html(instr):
    src = file(instr).read()
    doc = re.search(r'^/\*\*+(.*)\%[Ee]', src,
                    re.MULTILINE + re.DOTALL).group(1)

    out = instr + '.doc.txt'
    file(out, 'w').write(doc)


if __name__ == '__main__':
    map(gen_html, grab_list())
