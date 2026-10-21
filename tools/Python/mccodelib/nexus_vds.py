#!/usr/bin/env python3
"""Link the per-node MPI event lists (mccode_node<N>.h5) into mccode.h5.

A completed MPI run with --format=NeXus does this itself. After an
interrupted run (e.g. SIGTERM) the event lists are only in the per-node
files; this creates the missing 'events' virtual data sets in mccode.h5,
concatenating the nodes in rank order, as the final save would have.

Usage: python3 nexus_vds.py DIR
"""
import re
import sys
from pathlib import Path

import h5py


def main(outdir):
    outdir = Path(outdir)
    sources = {}  # mccode_path -> [(rank, file name, data set name, shape)]
    for node in outdir.glob("mccode_node*.h5"):
        m = re.fullmatch(r"mccode_node(\d+)\.h5", node.name)
        if not m:
            continue
        with h5py.File(node, "r") as f:
            for name, ds in f.items():
                if isinstance(ds, h5py.Dataset) and "mccode_path" in ds.attrs and ds.shape[0]:
                    path = ds.attrs["mccode_path"]
                    path = path.decode() if isinstance(path, bytes) else path
                    sources.setdefault(path, []).append((int(m.group(1)), node.name, name, ds.shape))
    with h5py.File(outdir / "mccode.h5", "r+") as f:
        entry = next(k for k, g in f.items() if g.attrs.get("NX_class") in (b"NXentry", "NXentry"))
        for path, srcs in sorted(sources.items()):
            target = f"{entry}/{path}"
            if target in f:
                print(f"exists:  {target}")
                continue
            srcs.sort()
            rows, cols = sum(s[3][0] for s in srcs), srcs[0][3][1]
            layout = h5py.VirtualLayout(shape=(rows, cols), dtype="f8")
            start = 0
            for rank, fname, dset, shape in srcs:
                layout[start:start + shape[0]] = h5py.VirtualSource(fname, dset, shape=shape)
                start += shape[0]
            f.require_group(target.rsplit("/", 1)[0])
            f.create_virtual_dataset(target, layout).attrs["signal"] = 1
            print(f"linked:  {target} ({rows} events from {len(srcs)} nodes)")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
