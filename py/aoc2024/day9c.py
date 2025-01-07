# cython: boundscheck=False, wraparound=False, initializedcheck=False, embedsignature=True
import cython
from cython.cimports.cpython.mem import PyMem_Malloc, PyMem_Free


@cython.cfunc
def rangesum(start: cython.int, size: cython.int) -> cython.int:
    return (2 * start + size - 1) * size // 2


@cython.ccall
def part1(chunks: cython.int[:]) -> cython.int:
    total: cython.int = 0
    offset: cython.int = 0
    i: cython.int = 0
    j: cython.int = chunks.shape[0] - 1
    while i <= j:
        size: cython.int
        if not i % 2:
            size = chunks[i]
            total += i // 2 * rangesum(offset, size)
            offset += size
            i += 1
        elif not j % 2:
            size = min(chunks[i], chunks[j])
            total += j // 2 * rangesum(offset, size)
            offset += size
            chunks[i] -= size
            if chunks[i] <= 0:
                i += 1
            chunks[j] -= size
            if chunks[j] <= 0:
                j -= 1
        else:
            j -= 1
    return total


@cython.ccall
def part2(chunks: cython.int[:]) -> cython.long:
    offsets: cython.p_int = cython.cast(
        cython.p_int, PyMem_Malloc(chunks.shape[0] * cython.sizeof(cython.int))
    )
    try:
        i: cython.int
        offset: cython.int = 0
        for i in range(chunks.shape[0]):
            offsets[i] = offset
            offset += chunks[i]
        total: cython.long = 0
        for i in range(len(chunks) - 1 & ~1, -1, -2):
            size: cython.int = chunks[i]
            offset = offsets[i]
            for j in range(1, i, 2):
                if chunks[j] >= size:
                    offset = offsets[j]
                    offsets[j] += size
                    chunks[j] -= size
                    break
            total += i // 2 * rangesum(offset, size)
        return total
    finally:
        PyMem_Free(offsets)
