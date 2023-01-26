#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import logging
import json
import jinja2
from pathlib import Path


def safe_format(value):
    def safer_format(value):
        return "{v:0.3{t}}".format(v=0 if value is None else value, t='g' if value < 0.01 or value > 1e4 else 'f')

    if not hasattr(value, '__len__'):
        return safer_format(value)
    else:
        inner = ", ".join([safer_format(x) for x in value])
        return f"[{inner}]"


def safe_error_percentage(value, expected):
    if hasattr(value, '__len__') and hasattr(expected, '__len__'):
        # For lack of a better idea, find the total ratio:
        return abs(sum(value) / sum(expected)) * 100
    return abs(value / expected) * 100


def get_col_header(label, meta):
    values = [meta.get(x, "") for x in ('ncount', 'hostname', 'cpu_type', 'gpu_type', 'date')]
    values[0] = f"{label} - {values[0]}"
    return "<br>".join(values)


def get_header_lst(meta):
    """ composes an easily-templatable list fom a "_meta" test header object """
    fields = ('ncount', 'date', 'hostname', 'user', 'cpu_type', 'gpu_type', 'date')
    return [] if meta is None else [meta[x] for x in fields]


def get_cell_tuple(obj, tolerance):
    """ set up and format cell data """
    # Deconstruct localfile path to find 'label' corresponding to current cell
    label = obj["localfile"].split("/")[-3]
    base_url = f"{label}/{obj['instrname']}/"
    url = f"{base_url}/{obj['testnb']}/browse.html"
    comp_url = f"{base_url}/compile_stdout.txt"

    comp_time = safe_format(obj['compiletime'])
    run_time = safe_format(obj['runtime'])

    if not obj["compiled"]:
        return 4, "<strong><font color=\"red\">! Compile error !</font></strong>", "", "", "", comp_url
    elif not obj["didrun"]:
        return 3, "" if obj['testnb'] != 0 else comp_time, "", "", "", base_url
    elif not obj["testval"]:
        return 2, "" if obj['testnb'] != 0 else comp_time, run_time, "missing", "", url
    else:
        testval = safe_format(obj['testval'])
        refp = safe_error_percentage(obj['testval'], obj['targetval'])
        state = 2 if abs(refp-100) > tolerance else 1
        percentage_repr = f"{refp:0.2g}" if abs(refp) > 1000 else f"{refp:0.2f}"
        return state, "" if obj['testnb'] != 0 else comp_time, run_time, testval, percentage_repr, url


def run_normal_mode(testdir: Path, reflabel: str, tolerance):
    """ load test data and print to html label """
    def has_test(labels):
        """ labels : [(t, obj, meta)] """
        for x in labels:
            if len(x.keys()) > 0:
                return True
        return False

    def iterate_obj_to_populate_rows(iterobj: dict, otherobjs, rows, ncols, del_used_from_overobjs=True):
        """
        Used to construct rows from a dict and a list of dicts with similar keys, either
        from a reference column, or as egalitarian with a lead "iterate" object. Appends to rows.

        cols: if higher than 1+len(otherobjs), empty cells are first appended to rows, in order to orient cols correctly)
        """
        # use default order, default sorting (e.g. list.sort()) wasn't satisfactory
        for key in iterobj:
            row = []
            rows.append(row)
            # instr
            row.append(key)

            # prepare row list to have the requested amount of cells (cols)
            for col in range(ncols - len(otherobjs) - 1):
                row.append((4, "not on branch" if col == 0 else "no test"))

            # ref col
            row.append(get_cell_tuple(iterobj[key], tolerance))

            # remaining cols
            for other_obj in otherobjs:
                if other_obj.has(key):
                    row.append(get_cell_tuple(other_obj[key], tolerance))
                    # delete "used" cell keys
                    if del_used_from_overobjs:
                        del other_obj[key]
                else:
                    # errmsg = iterobj[key]["errmsg"]
                    row.append((4, "not on branch"))

    # load test data
    all_labels = [x for x in testdir.iterdir() if x.is_dir()]
    # get number of data columns
    numcols= len(all_labels)

    refobj = None
    refmeta = None
    testlabels = []
    testobjs = []
    testmetas = []
    for subdir in all_labels:
        with open(subdir.joinpath(f'testresults_{subdir.name}.json'), 'r') as file:
            obj = json.load(file)
        meta = obj.get("_meta", None)
        if meta:
            del obj["_meta"]
        if reflabel == subdir.name:
            refobj = obj
            refmeta = meta
        else:
            testlabels.append(subdir.name)
            testobjs.append(obj)
            testmetas.append(meta)

    # create header row
    hrow = [get_col_header(f"{reflabel} (ref)", refmeta)]
    for i in range(len(testlabels)):
        hrow.append(get_col_header(testlabels[i], testmetas[i]))

    # create rows - 1) all instr tests in reference
    rows = []
    iterate_obj_to_populate_rows(refobj, testobjs, rows, ncols=numcols)

    # WARNING: untested in the non-trivial case
    while has_test(testobjs):
        leadcol = testobjs.pop(0)
        iterate_obj_to_populate_rows(leadcol, testobjs, rows, ncols=numcols)

    with open(Path(__file__).parent.joinpath('main.template'), 'r') as file:
        text = file.read()
    html = jinja2.Template(text).render(hrow=hrow, rows=rows, header=get_header_lst(refmeta))

    ofile = Path().joinpath(f"{testdir.stem}_output.html").resolve()
    logging.info(f"Writing output to {ofile}")
    with open(ofile, 'w') as file:
        file.write(html)


def main(args):
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG, format="%(message)s")
    else:
        logging.basicConfig(level=logging.INFO, format="%(message)s")

    if not args.testdir:
        logging.info("Previous 'interactive mode' omitted -- remove directories using system utilities instead.")
        quit(0)
    else:
        run_normal_mode(args.testdir, args.reflabel, args.tolerance)


if __name__ == '__main__':
    import argparse
    from pathlib import Path
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('testdir',
                        type=lambda p: Path(p).resolve(),
                        nargs="?",
                        help='test data is drawn from this root folder')
    parser.add_argument('--reflabel', type=str, nargs="?", help='reference label name')
    parser.add_argument('--tolerance', type=float, default=20., help='percentage tolerance for test comparison')
    parser.add_argument('--verbose', action='store_true', help='output excessive information for debug purposes')

    main(parser.parse_args())


