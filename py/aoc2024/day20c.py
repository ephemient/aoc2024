# cython: boundscheck=False, wraparound=False, initializedcheck=False, embedsignature=True
import cython
from collections.abc import Sequence


@cython.ccall
def solve(
    path: Sequence[tuple[tuple[int, int], int]], cheats: cython.int, time: cython.int
) -> cython.int:
    count: cython.int = len(path)
    i: cython.int
    result: cython.int = 0
    for i in range(count):
        y1: cython.int
        x1: cython.int
        t1: cython.int
        (y1, x1), t1 = path[i]
        j: cython.int
        for j in range(i + 1, count):
            y2: cython.int
            x2: cython.int
            t2: cython.int
            (y2, x2), t2 = path[j]
            if y2 > y1 + cheats or y2 == y1 + cheats and x2 > x1:
                break
            distance: cython.int = abs(y2 - y1) + abs(x2 - x1)
            if distance <= cheats and distance + time <= abs(t2 - t1):
                result += 1
    return result
