# cython: boundscheck=False, wraparound=False, initializedcheck=False, embedsignature=True
import cython
from collections.abc import Container
from cython.cimports.cpython.mem import PyMem_Malloc, PyMem_Free
from cython.cimports.libc.string import memcpy, memset
from cython.parallel import prange


@cython.cfunc
def obstructions_bitmap(
    max_bounds: tuple[cython.int, cython.int], obstructions: Container[tuple[int, int]]
) -> cython.p_uchar:
    height: cython.int
    width: cython.int
    height, width = max_bounds
    result: cython.p_uchar = cython.cast(cython.p_uchar, PyMem_Malloc(height * width))
    y: cython.int
    for y in range(height):
        x: cython.int
        for x in range(width):
            result[y * width + x] = (y, x) in obstructions
    return result


Walker = cython.struct(
    height=cython.int,
    width=cython.int,
    obstructions=cython.p_uchar,
    y=cython.int,
    x=cython.int,
    dy=cython.int,
    dx=cython.int,
)


@cython.cfunc
@cython.nogil
def walk_next(walker: cython.pointer(Walker)) -> cython.bint:
    next_y: cython.int = walker.y + walker.dy
    next_x: cython.int = walker.x + walker.dx
    if not (0 <= next_y < walker.height and 0 <= next_x < walker.width):
        return False
    if walker.obstructions[next_y * walker.width + next_x]:
        walker.dy, walker.dx = walker.dx, -walker.dy
    else:
        walker.y, walker.x = next_y, next_x
    return True


@cython.cfunc
def _part1(
    initial_pos: tuple[cython.int, cython.int],
    max_bounds: tuple[cython.int, cython.int],
    obstructions: cython.p_uchar,
) -> set[tuple[int, int]]:
    walker: Walker = Walker(
        height=max_bounds[0],
        width=max_bounds[1],
        obstructions=obstructions,
        y=initial_pos[0],
        x=initial_pos[1],
        dy=-1,
        dx=0,
    )
    result: set[tuple[int, int]] = set()
    while True:
        result.add((walker.y, walker.x))
        if not walk_next(cython.address(walker)):
            break
    return result


@cython.ccall
def part1(
    initial_pos: tuple[cython.int, cython.int],
    max_bounds: tuple[cython.int, cython.int],
    obstructions: Container[tuple[int, int]],
) -> cython.int:
    obstructions2: cython.p_uchar = obstructions_bitmap(max_bounds, obstructions)
    try:
        return len(_part1(initial_pos, max_bounds, obstructions2))
    finally:
        PyMem_Free(obstructions2)


@cython.ccall
def part2(
    initial_pos: tuple[cython.int, cython.int],
    max_bounds: tuple[cython.int, cython.int],
    obstructions: Container[tuple[int, int]],
) -> cython.int:
    height: cython.int
    width: cython.int
    height, width = max_bounds
    obstructions2: cython.p_uchar = obstructions_bitmap(max_bounds, obstructions)
    try:
        candidates = _part1(initial_pos, max_bounds, obstructions2)
        candidates.remove(initial_pos)
        candidates = list(candidates)
        i: cython.int
        count: cython.int = len(candidates)
        result: cython.int = 0
        for i in prange(count, nogil=True):
            y: cython.int
            x: cython.int
            with cython.gil:
                y, x = candidates[i]
                obstructions3: cython.p_uchar = cython.cast(
                    cython.p_uchar, PyMem_Malloc(width * height)
                )
            try:
                memcpy(obstructions3, obstructions2, width * height)
                obstructions3[y * width + x] = True
                with cython.gil:
                    seen: cython.p_uchar = cython.cast(
                        cython.p_uchar, PyMem_Malloc(width * height)
                    )
                try:
                    memset(seen, 0, width * height)
                    walker: Walker = Walker(
                        height=height,
                        width=width,
                        obstructions=obstructions3,
                        y=initial_pos[0],
                        x=initial_pos[1],
                        dy=-1,
                        dx=0,
                    )
                    is_up: cython.bint = True
                    while walk_next(cython.address(walker)):
                        if walker.dy == -1:
                            if not is_up:
                                if seen[walker.y * width + walker.x]:
                                    break
                                seen[walker.y * width + walker.x] = True
                            is_up = True
                        else:
                            is_up = False
                    else:
                        continue
                    result += 1
                finally:
                    with cython.gil:
                        PyMem_Free(seen)
            finally:
                with cython.gil:
                    PyMem_Free(obstructions3)
        return result
    finally:
        PyMem_Free(obstructions2)
