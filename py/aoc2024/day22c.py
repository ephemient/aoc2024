# cython: boundscheck=False, wraparound=False, initializedcheck=False, embedsignature=True
# ruff: noqa: F821
import cython
from cython.parallel import prange
from cython.cimports.libc.string import memset


@cython.cfunc
@cython.nogil
def _step(num: cython.uint) -> cython.uint:
    num = num ^ num << 6 & 16777215
    num = num ^ num >> 5 & 16777215
    num = num ^ num << 11 & 16777215
    return num


@cython.ccall
@cython.nogil
def part1(data: cython.uint[:]) -> cython.ulong:
    i: cython.int
    n: cython.int = len(data)
    result: cython.ulong = 0
    for i in prange(n):
        j: cython.int
        secret: cython.uint = data[i]
        for j in range(2000):
            secret = _step(secret)
        result += secret
    return result


@cython.ccall
@cython.nogil
def part2(data: cython.uint[:]) -> cython.uint:
    i: cython.int
    acc: cython.uint[19 * 19 * 19 * 19]
    memset(cython.address(acc[0]), 0, cython.sizeof(acc))
    n: cython.int = len(data)
    result: cython.uint = 0
    for i in prange(n):
        j: cython.int
        secret: cython.uint = data[i]
        seen: cython.bint[19 * 19 * 19 * 19]
        memset(cython.address(seen[0]), 0, cython.sizeof(seen))
        window: cython.uint[4]
        window[0] = secret % 10
        best: cython.uint = 0
        cur: cython.uint
        for j in range(1, 2001):
            secret = _step(secret)
            price: cython.uint = secret % 10
            if j >= 4:
                p0: cython.uint = window[j % 4]
                p1: cython.uint = window[(j + 1) % 4]
                p2: cython.uint = window[(j + 2) % 4]
                p3: cython.uint = window[(j + 3) % 4]
                d1: cython.int = p0 - p1
                d2: cython.int = p1 - p2
                d3: cython.int = p2 - p3
                d4: cython.int = p3 - price
                key: cython.uint = (
                    19 * (19 * (19 * (d1 + 9) + d2 + 9) + d3 + 9) + d4 + 9
                )
                if not seen[key]:
                    seen[key] = True
                    cur = (
                        atomic_fetch_add_uint(
                            cython.cast(
                                cython.pointer(atomic_uint), cython.address(acc[key])
                            ),
                            price,
                        )
                        + price
                    )
                    if best < cur:
                        best = cur
            window[j % 4] = price
        cur = atomic_load_uint(
            cython.cast(cython.pointer(atomic_uint), cython.address(result))
        )
        while cur < best:
            atomic_compare_exchange_weak_uint(
                cython.cast(cython.pointer(atomic_uint), cython.address(result)),
                cython.address(cur),
                best,
            )
    return result
