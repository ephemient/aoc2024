# cython: boundscheck=False, wraparound=False, initializedcheck=False, embedsignature=True
import cython
from collections.abc import Collection
from cython.cimports.cpython.mem import PyMem_Free, PyMem_Realloc
from cython.cimports.cpython.unicode import PyUnicode_Tailmatch


@cython.ccall
def solve(
    keys: Collection[str], targets: Collection[str]
) -> tuple[cython.int, cython.long]:
    i: cython.int
    j: cython.int
    k: cython.int
    numkeys: cython.int = len(keys)
    numtargets: cython.int = len(targets)
    counts: cython.p_long = cython.NULL
    numcounts: cython.int = 0
    part1: cython.int = 0
    part2: cython.long = 0
    try:
        for i in range(numtargets):
            target: str = targets[i]
            if not target:
                continue
            targetlen: cython.int = len(target)
            if numcounts < targetlen + 1:
                numcounts = targetlen + 1
                counts = cython.cast(
                    cython.p_long,
                    PyMem_Realloc(counts, numcounts * cython.sizeof(cython.long)),
                )
            counts[0] = 1
            for j in range(1, targetlen + 1):
                counts[j] = 0
                for k in range(numkeys):
                    key: str = keys[k]
                    keylen: cython.int = len(key)
                    if PyUnicode_Tailmatch(target, key, 0, j, 1) == 1:
                        counts[j] += counts[j - keylen]
            n: cython.long = counts[targetlen]
            part1 += 1 if n else 0
            part2 += n
        return part1, part2
    finally:
        PyMem_Free(counts)
