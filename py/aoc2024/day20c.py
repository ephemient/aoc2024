# cython: boundscheck=False, wraparound=False, initializedcheck=False, embedsignature=True
import cython


@cython.ccall
@cython.nogil
def solve(path: cython.int[:], cheats: cython.int, time: cython.int) -> cython.int:
    count: cython.int = path.shape[0] // 3
    i: cython.int
    result: cython.int = 0
    for i in range(count):
        y1: cython.int = path[3 * i]
        x1: cython.int = path[3 * i + 1]
        t1: cython.int = path[3 * i + 2]
        j: cython.int
        for j in range(i + 1, count):
            y2: cython.int = path[3 * j]
            x2: cython.int = path[3 * j + 1]
            t2: cython.int = path[3 * j + 2]
            if y2 > y1 + cheats or y2 == y1 + cheats and x2 > x1:
                break
            distance: cython.int = abs(y2 - y1) + abs(x2 - x1)
            if distance <= cheats and distance + time <= abs(t2 - t1):
                result += 1
    return result
